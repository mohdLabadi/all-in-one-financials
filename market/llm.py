"""LLM chat client: Ollama Cloud primary, OpenAI fallback."""

import time

import requests

from .config import OLLAMA_CLOUD_API_KEY, OPENAI_API_KEY


def rate_limit_429_hint() -> str:
    return (
        " [429 = rate limit: wait 1-3 min, then retry. The 3-agent pipeline sends several LLM requests per run. "
        "Upgrade API tier or space out runs. Alpha Vantage free tier: 5 requests/minute.]"
    )

def llm_chat_messages(messages, api_key, provider: str = "ollama", model: str | None = None) -> dict:
    if not api_key:
        return {"ok": False, "error": "No API key provided."}
    if model is None:
        model = "gpt-oss:120b" if provider == "ollama" else "gpt-4o-mini"
    if provider == "ollama":
        url = "https://ollama.com/api/chat"
        body = {"model": model, "messages": messages, "stream": False}
    else:
        url = "https://api.openai.com/v1/chat/completions"
        body = {"model": model, "messages": messages, "max_tokens": 2048}
    headers = {"Authorization": f"Bearer {api_key}", "Content-Type": "application/json"}

    last_status = None
    last_error = "Chat request failed."
    for attempt in range(1, 6):
        if attempt > 1:
            if last_status != 429:
                break
            time.sleep([6, 15, 30, 45][attempt - 2])
        try:
            resp = requests.post(url, headers=headers, json=body, timeout=60)
            last_status = resp.status_code
            if resp.status_code == 200:
                out = resp.json()
                text = out.get("message", {}).get("content") if provider == "ollama" else out["choices"][0]["message"]["content"]
                if not text:
                    last_error = "Empty response."
                    continue
                return {"ok": True, "text": text}
            err = f"HTTP {resp.status_code}"
            try:
                pj = resp.json()
                em = pj.get("error")
                if em:
                    err = str(em.get("message", em)) if isinstance(em, dict) else str(em)
            except Exception:
                pass
            last_error = err
        except Exception as e:
            last_status = None
            last_error = str(e)
    if last_status == 429:
        last_error += rate_limit_429_hint()
    return {"ok": False, "error": last_error}


def pick_llm_credentials() -> dict:
    creds = []
    if OLLAMA_CLOUD_API_KEY:
        creds.append({"provider": "ollama", "key": OLLAMA_CLOUD_API_KEY})
    if OPENAI_API_KEY:
        creds.append({"provider": "openai", "key": OPENAI_API_KEY})
    return {"ok": len(creds) > 0, "creds": creds}


def llm_chat_messages_with_fallback(messages, creds) -> dict:
    if not creds:
        return {"ok": False, "error": "No configured AI provider.", "provider": None}
    errs = []
    for c in creds:
        out = llm_chat_messages(messages, api_key=c["key"], provider=c["provider"])
        if out.get("ok"):
            return {"ok": True, "text": out["text"], "provider": c["provider"]}
        errs.append(f"{c['provider']}: {out.get('error', 'Unknown error')}")
    return {"ok": False, "error": " | ".join(errs), "provider": None}


def truncate_ai_context(text: str, max_chars: int = 12000) -> str:
    if not text:
        return text
    if len(text) <= max_chars:
        return text
    return text[:max_chars] + "\n\n[Context truncated for length.]"
