"""Daily Market Analysis - Alpha Vantage market workstation (Python Shiny port).

AI pipeline: agentic orchestration (3 agents) + RAG: local corpus data/rag_market_corpus.txt is
retrieved (keyword overlap) and prepended to the app context for all three LLM calls. No LLM tool calling.

Run: `shiny run app.py --reload` (or `shiny run --launch-browser app.py`) from this folder,
with the project virtualenv (.venv) active and dependencies from requirements.txt installed.
"""

import os
import re
import time
from datetime import date
from pathlib import Path

import numpy as np
import pandas as pd
import plotly.graph_objects as go
import requests
from dotenv import load_dotenv

from shiny import App, reactive, render, req, ui
from shinywidgets import output_widget, render_widget

# ---------------------------------------------------------------------------
# Env / config
# ---------------------------------------------------------------------------

APP_DIR = Path(__file__).resolve().parent
load_dotenv(APP_DIR / ".env")


def _env(*names: str) -> str:
    for n in names:
        v = os.environ.get(n, "").strip()
        if v:
            return v
    return ""


API_KEY = _env("ALPHAVANTAGE_API_KEY", "API_KEY")
OLLAMA_CLOUD_API_KEY = _env("OLLAMA_CLOUD_API_KEY", "OLLAMA_API_KEY")
OPENAI_API_KEY = _env("OPENAI_API_KEY")

CHART_ICON_SVG = (
    "<svg width='22' height='22' viewBox='0 0 24 24' fill='none' stroke='currentColor' "
    "stroke-width='2' stroke-linecap='round' stroke-linejoin='round'>"
    "<polyline points='3 17 9 11 13 15 21 6'/><polyline points='14 6 21 6 21 13'/></svg>"
)

# ---------------------------------------------------------------------------
# Alpha Vantage API layer
# ---------------------------------------------------------------------------

BASE_URL = "https://www.alphavantage.co/query"
_av_cache: dict = {}


def is_rate_limited_error(msg) -> bool:
    m = str(msg or "").lower()
    return bool(re.search(r"rate limit|5 requests|min|25/day|429|too many", m))


def safe_fetch_with_cache(cache_key, fetch_fn):
    try:
        out = fetch_fn()
        _av_cache[cache_key] = out
        return {"ok": True, "data": out, "fallback": False, "msg": None}
    except Exception as e:
        msg = str(e)
        if is_rate_limited_error(msg) and cache_key in _av_cache:
            return {
                "ok": True,
                "data": _av_cache[cache_key],
                "fallback": True,
                "msg": "Alpha Vantage limit reached. Showing last cached result for this query.",
            }
        return {"ok": False, "error": msg}


def av_get(params: dict) -> dict:
    if not API_KEY:
        raise RuntimeError("API key not set. Put API_KEY or ALPHAVANTAGE_API_KEY in .env in the app folder.")
    q = {**params, "apikey": API_KEY}
    resp = requests.get(BASE_URL, params=q, timeout=30, headers={"User-Agent": "Python-Shiny-Market-Tool"})
    if resp.status_code != 200:
        raise RuntimeError("Alpha Vantage request failed. Check API key or URL.")
    out = resp.json()
    if isinstance(out, dict) and out.get("Error Message"):
        raise RuntimeError(out["Error Message"])
    if isinstance(out, dict) and out.get("Note"):
        raise RuntimeError(
            f"API rate limit: {out['Note']} Free key: 5 requests/min, ~25/day. Wait a minute or try again tomorrow."
        )
    if isinstance(out, dict) and out.get("Information"):
        raise RuntimeError(f"API: {out['Information']}")
    return out


def av_stock_daily(symbol: str = "AAPL", outputsize: str = "compact") -> pd.DataFrame:
    out = av_get({"function": "TIME_SERIES_DAILY", "symbol": symbol, "outputsize": outputsize})
    ts = out.get("Time Series (Daily)")
    if not ts:
        keys = list(out.keys())
        extra = ""
        if out.get("Error Message"):
            extra = f" API said: {out['Error Message']}"
        elif out.get("Information"):
            extra = f" API said: {out['Information']}"
        elif out.get("Note"):
            extra = f" API said: {out['Note']}"
        elif keys:
            extra = f" Response keys: {', '.join(keys)}"
        raise RuntimeError(f"No time series in response for {symbol}.{extra}")
    rows = []
    for d, v in ts.items():
        rows.append(
            {
                "date": d,
                "open": float(v.get("1. open", "nan")),
                "high": float(v.get("2. high", "nan")),
                "low": float(v.get("3. low", "nan")),
                "close": float(v.get("4. close", "nan")),
                "volume": float(v.get("5. volume", "nan")),
            }
        )
    df = pd.DataFrame(rows)
    df["date"] = pd.to_datetime(df["date"])
    return df.sort_values("date", ascending=False).reset_index(drop=True)


def _list_to_df(x) -> pd.DataFrame:
    if not x:
        return pd.DataFrame()
    return pd.DataFrame(x)


def av_top_gainers_losers() -> dict:
    out = av_get({"function": "TOP_GAINERS_LOSERS"})
    return {
        "top_gainers": _list_to_df(out.get("top_gainers")),
        "top_losers": _list_to_df(out.get("top_losers")),
        "top_activated": _list_to_df(out.get("most_actively_traded")),
    }


def av_news_sentiment(tickers: str = "", limit: int = 50) -> pd.DataFrame:
    params = {"function": "NEWS_SENTIMENT", "limit": limit, "sort": "LATEST"}
    if tickers:
        params["tickers"] = tickers
    out = av_get(params)
    feed = out.get("feed")
    if not feed:
        return pd.DataFrame()
    rows = [
        {
            "title": x.get("title"),
            "url": x.get("url"),
            "time_published": x.get("time_published"),
            "summary": x.get("summary"),
            "overall_sentiment_score": x.get("overall_sentiment_score"),
            "overall_sentiment_label": x.get("overall_sentiment_label"),
            "source": x.get("source"),
        }
        for x in feed
    ]
    return pd.DataFrame(rows)


def av_fx_daily(from_symbol: str = "EUR", to_symbol: str = "USD") -> pd.DataFrame:
    out = av_get({"function": "FX_DAILY", "from_symbol": from_symbol, "to_symbol": to_symbol})
    ts = out.get("Time Series FX (Daily)")
    if not ts:
        return pd.DataFrame()
    rows = []
    for d, v in ts.items():
        rows.append(
            {
                "date": d,
                "open": float(v.get("1. open", "nan")),
                "high": float(v.get("2. high", "nan")),
                "low": float(v.get("3. low", "nan")),
                "close": float(v.get("4. close", "nan")),
            }
        )
    df = pd.DataFrame(rows)
    df["date"] = pd.to_datetime(df["date"])
    return df.sort_values("date", ascending=False).reset_index(drop=True)


def _series_to_df(ts) -> pd.DataFrame:
    if isinstance(ts, list) and ts:
        rows = [{"date": x.get("date"), "value": x.get("value")} for x in ts]
        df = pd.DataFrame(rows)
        df["date"] = pd.to_datetime(df["date"], errors="coerce")
        df["value"] = pd.to_numeric(df["value"], errors="coerce")
        return df.sort_values("date", ascending=False).reset_index(drop=True)
    return pd.DataFrame()


def av_commodity(commodity: str = "WHEAT", interval: str = "monthly") -> pd.DataFrame:
    out = av_get({"function": commodity, "interval": interval})
    ts_key = next((k for k in out if re.search(r"data|time|series", k, re.I)), None)
    if ts_key is None:
        ts_key = next((k for k in out if k not in ("name", "interval", "unit")), None)
    return _series_to_df(out.get(ts_key)) if ts_key else pd.DataFrame()


def av_economic(indicator: str = "CPI", interval: str = "monthly", **extra) -> pd.DataFrame:
    params = {"function": indicator, **extra}
    if indicator in ("CPI", "REAL_GDP", "TREASURY_YIELD", "FEDERAL_FUNDS_RATE") and interval:
        params["interval"] = interval
    out = av_get(params)
    ts_key = next((k for k in out if re.search(r"data|time|series", k, re.I)), None)
    if ts_key is None:
        ts_key = next((k for k in out if k not in ("name", "interval", "unit", "information")), None)
    return _series_to_df(out.get(ts_key)) if ts_key else pd.DataFrame()


def rate_limit_429_hint() -> str:
    return (
        " [429 = rate limit: wait 1-3 min, then retry. The 3-agent pipeline sends several LLM requests per run. "
        "Upgrade API tier or space out runs. Alpha Vantage free tier: 5 requests/minute.]"
    )


# ---------------------------------------------------------------------------
# LLM layer (Ollama Cloud primary, OpenAI fallback)
# ---------------------------------------------------------------------------


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


# ---------------------------------------------------------------------------
# RAG: retrieve chunks from local corpus (data/rag_market_corpus.txt)
# ---------------------------------------------------------------------------

RAG_CORPUS_EMBEDDED = "\n\n---\n\n".join(
    [
        "Stock and index levels move with earnings, rates, and sentiment. When comparing day-over-day moves, "
        "use the same price field (e.g. adjusted close vs close) and note whether the series is trading days only.",
        "Forex pairs quote the value of the base currency in terms of the quote. A rising EUR/USD means euros buy "
        "more dollars. Macro drivers include interest-rate differentials, risk appetite, and surprise data prints.",
        "Commodity futures and spot references differ by contract month and delivery location. Reported latest "
        "values may be month-end or last settlement; state the source cadence when interpreting moves.",
        "Economic indicators are often revised. CPI and jobs releases can move markets on the surprise versus "
        "consensus, not only the level. Treasury yields embed growth and inflation expectations.",
        "News sentiment scores are model-based and noisy. Use them as one signal alongside price action and "
        "fundamentals. Headlines may lag fast markets.",
        "Top gainers and losers lists are snapshots; liquidity and halts can distort one-day percent changes. "
        "Cross-check unusual movers against corporate actions.",
        "Risk: past performance does not guarantee future results. This workstation combines retrieved notes "
        "with live API context; always verify figures against primary sources.",
    ]
)


def rag_corpus_path() -> Path | None:
    p = APP_DIR / "data" / "rag_market_corpus.txt"
    return p if p.exists() else None


def load_rag_chunks() -> list[str]:
    path = rag_corpus_path()
    raw = path.read_text(encoding="utf-8") if path else ""
    if not raw.strip():
        raw = RAG_CORPUS_EMBEDDED
    parts = [p.strip() for p in raw.split("\n---\n")]
    parts = [p for p in parts if p]
    if not parts:
        parts = [p.strip() for p in RAG_CORPUS_EMBEDDED.split("\n---\n") if p.strip()]
    return parts


def retrieve_rag_for_report(report_type, section, context, k: int = 3) -> dict:
    chunks = load_rag_chunks()
    if not chunks:
        return {"text": "", "trace": "RAG: corpus empty (unexpected)."}
    query = f"{report_type} {section} {context}"
    qtok = sorted(set(re.findall(r"[a-zA-Z0-9]{3,}", query.lower())))
    if not qtok:
        qtok = ["market", "data"]

    def score(ch):
        ct = ch.lower()
        return sum(1 for w in qtok if w in ct)

    order = sorted(range(len(chunks)), key=lambda i: score(chunks[i]), reverse=True)
    take = [i for i in order if score(chunks[i]) > 0][:k]
    if not take:
        take = order[:k]
    sel = [chunks[i] for i in take]
    trace = "\n\n---\n\n".join(f"[{i + 1}]\n{c}" for i, c in enumerate(sel))
    return {"text": "\n\n".join(sel), "trace": trace}


def section_display_name(section) -> str:
    return {
        "stock": "Stock Daily",
        "gainers": "Top Gainers/Losers",
        "news": "News & Sentiment",
        "forex": "Forex",
        "commodity": "Commodities",
        "economic": "Economic Indicators",
        "ai": "AI Reporter",
    }.get(section, section or "Market")


def report_type_display_name(rt) -> str:
    return {
        "brief": "Cross-section overview",
        "stock": "Stock snapshot",
        "stock_trend": "Stock trend (computed)",
        "movers": "Top movers insight",
        "news": "News briefing",
        "forex_brief": "Forex overview",
        "forex_snapshot": "Forex snapshot",
        "forex_trend": "Forex trend (computed)",
        "commodity_brief": "Commodity overview",
        "commodity_snapshot": "Commodity snapshot",
        "commodity_trend": "Commodity trend (computed)",
        "economic_brief": "Economic indicator overview",
        "economic_snapshot": "Economic indicator snapshot",
        "economic_trend": "Economic trend (computed)",
    }.get(rt, rt or "Analysis")


# Agent 1 - Orchestration: plan themes, gaps, and delegation (no polished narrative).
AGENT_SYSTEM_ORCHESTRATOR = "\n".join(
    [
        "You are the Orchestration Lead for a market analysis workstation.",
        "The user message may include RETRIEVED KNOWLEDGE (RAG) - short reference notes from a local corpus - "
        "plus APPLICATION / API CONTEXT from the workstation.",
        "Plan the workflow only: prioritize themes, identify gaps in the provided data, and state what "
        "downstream analysis should emphasize.",
        "Do not write a polished narrative for the end user.",
        "Output clearly labeled sections: PRIORITY THEMES (bullets), DATA COVERAGE & GAPS, DELEGATION (what "
        "the Market Analyst should stress-test).",
        "If the context indicates no data was fetched, say so. Never invent prices, rates, or statistics not "
        "present in the context.",
    ]
)

# Agent 2 - Analyst: evidence-based memo from context + plan (RAG notes are guidance, not live prices).
AGENT_SYSTEM_ANALYST = "\n".join(
    [
        "You are the Market Intelligence Analyst.",
        "You receive (1) optional RETRIEVED KNOWLEDGE (RAG) - general reference text, (2) APPLICATION / API "
        "CONTEXT with numbers from the app, and (3) the Orchestration Lead's plan.",
        "Treat RAG as background reading; cite numbers only from the APPLICATION / API CONTEXT unless the RAG "
        "chunk itself contains a numeric fact you clearly label as from the note corpus.",
        "Produce rigorous, evidence-based analysis: cite specific numbers from the application context when "
        "available; separate facts from interpretation.",
        "Use headings: FACTS FROM DATA, INTERPRETATION, RISKS & UNCERTAINTIES, SCENARIOS (or state that "
        "scenarios are not warranted).",
        "If data is missing for a point, write 'Insufficient data in context' for that item. Do not fabricate figures.",
    ]
)

# Agent 3 - Editor: unify plan + analyst memo into the user-facing brief.
AGENT_SYSTEM_EDITOR = "\n".join(
    [
        "You are the Lead Editor.",
        "Combine the Orchestration plan and the Market Analyst memo into one coherent brief for the reader.",
        "Match the requested style: overview briefs use 2-3 short paragraphs; snapshots use 2-4 tight sentences; "
        "stay neutral for news.",
        "Remove redundancy, align tone, and end with one sentence on what to watch next.",
        "Output plain paragraphs only-no JSON, code, or section labels like 'SECTION 1'.",
    ]
)


def run_agentic_pipeline(report_type, section, context, cred, progress_fn=None) -> dict:
    ctx_base = truncate_ai_context(context)
    rag = retrieve_rag_for_report(report_type, section, ctx_base, k=3)
    rag_block = f"RETRIEVED KNOWLEDGE (RAG - local corpus, keyword retrieval):\n\n{rag['text']}" if rag["text"] else ""
    ctx = (f"{rag_block}\n\n---\n\n" if rag_block else "") + f"APPLICATION / API CONTEXT (from the workstation):\n\n{ctx_base}"
    sec_name = section_display_name(section)
    rtp_name = report_type_display_name(report_type)
    creds = cred["creds"]
    providers_used: list[str] = []

    def p(value, detail):
        if progress_fn:
            progress_fn(value, detail)

    u1 = f"Report type code: {report_type} ({rtp_name})\nApp section: {sec_name}\n\n{ctx}"
    p(0.2, "Agent 1 - Orchestrator: planning themes and gaps...")
    r1 = llm_chat_messages_with_fallback(
        [{"role": "system", "content": AGENT_SYSTEM_ORCHESTRATOR}, {"role": "user", "content": u1}], creds
    )
    if not r1["ok"]:
        return {
            "ok": False, "error": r1["error"], "orchestrator": None, "analyst": None, "final": None,
            "rag_trace": rag["trace"], "providers_used": providers_used,
        }
    providers_used.append(f"orchestrator={r1['provider']}")
    orch = r1["text"]
    time.sleep(2.2)

    u2 = f"ORCHESTRATION PLAN:\n{orch}\n\nFULL CONTEXT (same as Orchestrator - RAG + application data):\n{ctx}"
    p(0.55, "Agent 2 - Market Analyst: evidence-based memo...")
    r2 = llm_chat_messages_with_fallback(
        [{"role": "system", "content": AGENT_SYSTEM_ANALYST}, {"role": "user", "content": u2}], creds
    )
    if not r2["ok"]:
        return {
            "ok": False, "error": r2["error"], "orchestrator": orch, "analyst": None, "final": None,
            "rag_trace": rag["trace"], "providers_used": providers_used,
        }
    providers_used.append(f"analyst={r2['provider']}")
    an = r2["text"]
    time.sleep(2.2)

    orch_short = orch if len(orch) <= 1800 else orch[:1800] + "\n[...]"
    u3 = (
        f"Deliverable: {rtp_name} for section {sec_name}.\n\n"
        f"ORCHESTRATION (for alignment; may be shortened):\n{orch_short}\n\n"
        f"MARKET ANALYST OUTPUT:\n{an}"
    )
    p(0.85, "Agent 3 - Lead Editor: final brief...")
    r3 = llm_chat_messages_with_fallback(
        [{"role": "system", "content": AGENT_SYSTEM_EDITOR}, {"role": "user", "content": u3}], creds
    )
    if not r3["ok"]:
        return {
            "ok": False, "error": r3["error"], "orchestrator": orch, "analyst": an, "final": None,
            "rag_trace": rag["trace"], "providers_used": providers_used,
        }
    providers_used.append(f"editor={r3['provider']}")
    p(1, "Done")
    return {
        "ok": True, "error": None, "orchestrator": orch, "analyst": an, "final": r3["text"],
        "rag_trace": rag["trace"], "providers_used": providers_used,
    }


def _fmt_chg(pct: float) -> str:
    return f"+{pct:.2f}%" if pct >= 0 else f"{pct:.2f}%"


def build_stock_trend_report(st: pd.DataFrame | None, symbol: str) -> str:
    if st is None or st.empty:
        return "Fetch stock data first (Stock Daily section)."
    current = float(st["close"].iloc[0])
    if not np.isfinite(current):
        return "No valid price data."
    lines = [f"Stock trend: {symbol}", f"Current close: ${current:.2f}", ""]
    if len(st) >= 2:
        prev = float(st["close"].iloc[1])
        if np.isfinite(prev) and prev != 0:
            lines.append(f"Since yesterday:  {_fmt_chg(100 * (current - prev) / prev)} (was ${prev:.2f})")
    if len(st) >= 6:
        prev = float(st["close"].iloc[5])
        if np.isfinite(prev) and prev != 0:
            lines.append(f"Since last week:   {_fmt_chg(100 * (current - prev) / prev)} (~5 trading days ago, ${prev:.2f})")
    if len(st) >= 22:
        prev = float(st["close"].iloc[21])
        if np.isfinite(prev) and prev != 0:
            lines.append(f"Since last month:  {_fmt_chg(100 * (current - prev) / prev)} (~21 trading days ago, ${prev:.2f})")
    if len(st) >= 253:
        prev = float(st["close"].iloc[252])
        if np.isfinite(prev) and prev != 0:
            lines.append(f"Since last year:   {_fmt_chg(100 * (current - prev) / prev)} (~252 trading days ago, ${prev:.2f})")
    if len(lines) < 7:
        lines += ["", f"Note: Data has {len(st)} trading days. Use Full history for 1-year trend."]
    return "\n".join(lines)


def build_forex_trend_report(fx: pd.DataFrame | None, from_cur: str, to_cur: str) -> str:
    if fx is None or fx.empty:
        return "Fetch forex data first (Forex section)."
    pair = f"{from_cur}/{to_cur}"
    current = float(fx["close"].iloc[0])
    if not np.isfinite(current):
        return "No valid rate."
    lines = [f"Forex trend: {pair}", f"Current close: {current:.4f}", ""]
    if len(fx) >= 2:
        prev = float(fx["close"].iloc[1])
        if np.isfinite(prev) and prev != 0:
            lines.append(f"Since yesterday:  {_fmt_chg(100 * (current - prev) / prev)} (was {prev:.4f})")
    if len(fx) >= 6:
        prev = float(fx["close"].iloc[5])
        if np.isfinite(prev) and prev != 0:
            lines.append(f"Since last week:   {_fmt_chg(100 * (current - prev) / prev)} (~5 days ago, {prev:.4f})")
    if len(fx) >= 22:
        prev = float(fx["close"].iloc[21])
        if np.isfinite(prev) and prev != 0:
            lines.append(f"Since last month:  {_fmt_chg(100 * (current - prev) / prev)} (~21 days ago, {prev:.4f})")
    if len(fx) >= 253:
        prev = float(fx["close"].iloc[252])
        if np.isfinite(prev) and prev != 0:
            lines.append(f"Since last year:   {_fmt_chg(100 * (current - prev) / prev)} (~252 days ago, {prev:.4f})")
    if len(lines) < 7:
        lines += ["", f"Note: Data has {len(fx)} days."]
    return "\n".join(lines)


def build_value_trend_report(df: pd.DataFrame | None, label: str, value_col: str = "value") -> str:
    if df is None or df.empty:
        return f"Fetch {label} data first."
    if value_col not in df.columns:
        value_col = df.columns[1] if len(df.columns) > 1 else df.columns[0]
    current = float(df[value_col].iloc[0])
    if not np.isfinite(current):
        return "No valid value."
    lines = [f"{label} trend", f"Current value: {current:.4f}", ""]
    if len(df) >= 2:
        prev = float(df[value_col].iloc[1])
        if np.isfinite(prev) and prev != 0:
            lines.append(f"Since previous: {_fmt_chg(100 * (current - prev) / prev)} (was {prev:.4f})")
    if len(df) >= 6:
        prev = float(df[value_col].iloc[5])
        if np.isfinite(prev) and prev != 0:
            lines.append(f"Since ~1 week ago: {_fmt_chg(100 * (current - prev) / prev)} ({prev:.4f})")
    if len(df) >= 22:
        prev = float(df[value_col].iloc[21])
        if np.isfinite(prev) and prev != 0:
            lines.append(f"Since ~1 month ago: {_fmt_chg(100 * (current - prev) / prev)} ({prev:.4f})")
    if len(df) >= 253:
        prev = float(df[value_col].iloc[252])
        if np.isfinite(prev) and prev != 0:
            lines.append(f"Since ~1 year ago: {_fmt_chg(100 * (current - prev) / prev)} ({prev:.4f})")
    if len(lines) < 5:
        lines += ["", f"Note: Data has {len(df)} points."]
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Small dataframe / column helpers
# ---------------------------------------------------------------------------


def _norm_cols(df: pd.DataFrame) -> dict:
    return {re.sub(r"[^a-z0-9]", "", str(c).lower()): c for c in df.columns}


def _find_col(df: pd.DataFrame, *names, default_idx: int = 0):
    nm = _norm_cols(df)
    for n in names:
        if n in nm:
            return nm[n]
    cols = list(df.columns)
    return cols[default_idx] if len(cols) > default_idx else None


def qmark(label: str, tip: str):
    return ui.tags.span(
        label,
        ui.tags.span(" ?", title=tip, style="cursor: help; color: #0f766e; font-weight: 700; margin-left: 2px;"),
    )


def _stat(label: str, value: str, value_class: str = "value"):
    return ui.tags.div(ui.tags.h4(label), ui.tags.div(value, class_=value_class), class_="stat-card card-custom")


def _agent_body(text: str):
    return ui.tags.div(ui.markdown(text or ""), class_="agent-body markdown-body")


def _table_card(df: pd.DataFrame | None, height: str = "420px"):
    if df is None or df.empty:
        df = pd.DataFrame({"Message": ["No data yet."]})
    return render.DataGrid(df, width="100%", height=height, filters=False)


# ---------------------------------------------------------------------------
# Plotly theme
# ---------------------------------------------------------------------------

CHART_COLORS = {
    "close": "#0d9488",
    "ma20": "#2563eb",
    "ma50": "#7c3aed",
    "boll": "#94a3b8",
    "boll2": "#64748b",
    "scenario": "#f59e0b",
    "scenario_fx": "#f97316",
    "up": "#059669",
    "down": "#dc2626",
    "trend": "#334155",
}


def market_layout(fig: go.Figure, y_title=None, x_title=None, show_legend: bool = True, subtitle: str | None = None):
    both = bool(subtitle) and show_legend
    top_margin = 68 if both else (44 if (show_legend or subtitle) else 20)
    legend_y = 1.16 if both else 1.02
    fig.update_layout(
        template="plotly_white",
        font=dict(family="Inter, -apple-system, Segoe UI, sans-serif", color="#1e293b", size=12.5),
        title=dict(text=subtitle, font=dict(size=12, color="#64748b"), x=0, xanchor="left", y=0.99, yanchor="top") if subtitle else None,
        margin=dict(l=48, r=16, t=top_margin, b=40),
        plot_bgcolor="white",
        paper_bgcolor="rgba(0,0,0,0)",
        legend=dict(orientation="h", yanchor="bottom", y=legend_y, xanchor="left", x=0, font=dict(size=11.5)) if show_legend else None,
        showlegend=show_legend,
        hovermode="x unified",
        hoverlabel=dict(bgcolor="white", font_size=12, bordercolor="#e2e8f0"),
    )
    fig.update_xaxes(title=x_title, gridcolor="#eef1f6", zeroline=False, showline=True, linecolor="#e2e8f0", tickfont=dict(color="#64748b", size=11))
    fig.update_yaxes(title=y_title, gridcolor="#eef1f6", zeroline=False, showline=True, linecolor="#e2e8f0", tickfont=dict(color="#64748b", size=11))
    return fig


# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

SYNC_TAB_JS = """
$(function(){
  function syncActiveTab(){
    $('#section .radio-inline, #section .form-check').removeClass('active-tab');
    $('#section input:checked').closest('.radio-inline, .form-check').addClass('active-tab');
  }
  syncActiveTab();
  $(document).on('change', '#section input', syncActiveTab);
  $(document).on('shiny:value shiny:visualchange', function(){ syncActiveTab(); });
});
"""

SECTION_CHOICES = {
    "stock": "Stock Daily",
    "gainers": "Top Gainers/Losers",
    "news": "News & Sentiment",
    "forex": "Forex",
    "commodity": "Commodities",
    "economic": "Economic Indicators",
    "ai": "AI Reporter",
}

app_ui = ui.page_fluid(
    ui.include_css(APP_DIR / "www" / "styles.css"),
    ui.tags.head(
        ui.tags.title("Daily Market Analysis"),
        ui.tags.link(rel="preconnect", href="https://fonts.googleapis.com"),
        ui.tags.link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=""),
        ui.tags.link(
            href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap",
            rel="stylesheet",
        ),
        ui.tags.script(SYNC_TAB_JS),
    ),
    ui.tags.div(
        ui.tags.div(
            ui.tags.div(
                ui.tags.div("M", class_="brand-mark"),
                ui.tags.div(
                    ui.tags.div("Daily Market Analysis", class_="brand-name"),
                    ui.tags.div("Stocks · Forex · News · Multi-agent AI", class_="brand-tagline"),
                    class_="brand-text",
                ),
                class_="brand",
            ),
            ui.tags.div(
                ui.input_radio_buttons("section", None, choices=SECTION_CHOICES, selected="stock", inline=True),
                class_="nav-tabs-wrap",
            ),
            ui.tags.div(ui.output_ui("ui_top_status", inline=True), class_="top-nav-status"),
            class_="top-nav-inner",
        ),
        class_="top-nav",
    ),
    ui.tags.div(
        ui.tags.div(ui.output_ui("section_title"), ui.output_ui("ui_section_help"), class_="page-hero-inner"),
        class_="page-hero",
    ),
    ui.tags.div(
        ui.output_ui("api_fallback_ui"),
        ui.output_ui("api_error_ui"),
        ui.tags.div(ui.input_checkbox("show_raw_data", "Show data tables", value=False), class_="utility-row"),
        ui.tags.div(
            ui.tags.div(
                ui.panel_conditional(
                    "input.section === 'ai'",
                    ui.tags.div(
                        ui.tags.p(
                            "AI Reporter is available in the right panel. Fetch any data section first for best "
                            "results, then click Generate report.",
                            style="margin:0; color: var(--text-muted);",
                        ),
                        class_="card-custom",
                    ),
                ),
                ui.panel_conditional(
                    "input.section === 'stock'",
                    ui.tags.div(
                        ui.tags.div(
                            ui.input_text("stock_symbol", "Symbol", value="AAPL", placeholder="e.g. AAPL, MSFT"),
                            class_="control-field",
                        ),
                        ui.tags.div(
                            ui.input_checkbox("stock_full_history", "Full history (1-yr trend)", value=False),
                            class_="form-check",
                        ),
                        ui.input_action_button("fetch_stock", "Fetch", class_="btn-primary"),
                        class_="control-bar",
                    ),
                ),
                ui.panel_conditional(
                    "input.section === 'gainers'",
                    ui.tags.div(ui.input_action_button("fetch_gainers", "Fetch movers", class_="btn-primary"), class_="control-bar"),
                ),
                ui.panel_conditional(
                    "input.section === 'news'",
                    ui.tags.div(
                        ui.tags.div(ui.input_text("news_tickers", "Tickers (optional)", placeholder="AAPL, MSFT"), class_="control-field"),
                        ui.tags.div(ui.input_numeric("news_limit", "Limit", value=20, min=1, max=50), class_="control-field"),
                        ui.input_action_button("fetch_news", "Fetch news", class_="btn-primary"),
                        class_="control-bar",
                    ),
                ),
                ui.panel_conditional(
                    "input.section === 'forex'",
                    ui.tags.div(
                        ui.tags.div(ui.input_text("fx_from", "From", value="EUR"), class_="control-field"),
                        ui.tags.div(ui.input_text("fx_to", "To", value="USD"), class_="control-field"),
                        ui.input_action_button("fetch_fx", "Fetch", class_="btn-primary"),
                        class_="control-bar",
                    ),
                ),
                ui.panel_conditional(
                    "input.section === 'commodity'",
                    ui.tags.div(
                        ui.tags.div(
                            ui.input_select(
                                "commodity", "Commodity",
                                choices=["WHEAT", "CORN", "WTI", "BRENT", "NATURAL_GAS", "COPPER", "COFFEE"],
                            ),
                            class_="control-field",
                        ),
                        ui.tags.div(
                            ui.input_select("commodity_interval", "Interval", choices=["daily", "weekly", "monthly"], selected="monthly"),
                            class_="control-field",
                        ),
                        ui.input_action_button("fetch_commodity", "Fetch", class_="btn-primary"),
                        class_="control-bar",
                    ),
                ),
                ui.panel_conditional(
                    "input.section === 'economic'",
                    ui.tags.div(
                        ui.tags.div(
                            ui.input_select(
                                "economic_indicator", "Indicator",
                                choices=[
                                    "CPI", "INFLATION", "UNEMPLOYMENT", "FEDERAL_FUNDS_RATE",
                                    "TREASURY_YIELD", "REAL_GDP", "RETAIL_SALES", "NONFARM_PAYROLL",
                                ],
                            ),
                            class_="control-field",
                        ),
                        ui.tags.div(
                            ui.input_select("economic_interval", "Interval", choices=["monthly", "quarterly", "annual", "daily"], selected="monthly"),
                            class_="control-field",
                        ),
                        ui.input_action_button("fetch_economic", "Fetch", class_="btn-primary"),
                        class_="control-bar",
                    ),
                ),
                ui.tags.p(
                    "Select a section and click Fetch to load data. View, filter, and Download use already-loaded data (no extra API calls).",
                    class_="api-limit-note",
                ),
                ui.output_ui("ui_content_empty_state"),
                ui.panel_conditional(
                    "input.section === 'stock'",
                    ui.output_ui("ui_stock_toolbar"),
                    ui.output_ui("ui_stock_summary"),
                    ui.tags.div(output_widget("plot_stock", height="380px"), class_="plot-container"),
                    ui.tags.div(output_widget("plot_stock_returns", height="240px"), class_="plot-container"),
                    ui.output_ui("ui_stock_calculator"),
                    ui.panel_conditional("input.show_raw_data", ui.tags.div(ui.output_data_frame("table_stock"), class_="table-card")),
                ),
                ui.panel_conditional(
                    "input.section === 'gainers'",
                    ui.output_ui("ui_gainers_toolbar"),
                    ui.output_ui("ui_gainers_cards"),
                    ui.output_ui("ui_gainers_chart"),
                    ui.panel_conditional(
                        "input.show_raw_data",
                        ui.tags.h4("Top Gainers"), ui.tags.div(ui.output_data_frame("table_gainers"), class_="table-card"),
                        ui.tags.h4("Top Losers"), ui.tags.div(ui.output_data_frame("table_losers"), class_="table-card"),
                        ui.tags.h4("Most Active"), ui.tags.div(ui.output_data_frame("table_active"), class_="table-card"),
                    ),
                ),
                ui.panel_conditional(
                    "input.section === 'news'",
                    ui.output_ui("ui_news_toolbar"),
                    ui.tags.div(output_widget("plot_news_sentiment", height="250px"), class_="plot-container"),
                    ui.output_ui("ui_news_cards"),
                    ui.panel_conditional("input.show_raw_data", ui.tags.div(ui.output_data_frame("table_news"), class_="table-card")),
                ),
                ui.panel_conditional(
                    "input.section === 'forex'",
                    ui.output_ui("ui_fx_toolbar"),
                    ui.output_ui("ui_fx_summary"),
                    ui.tags.div(output_widget("plot_fx", height="380px"), class_="plot-container"),
                    ui.tags.div(output_widget("plot_fx_returns", height="240px"), class_="plot-container"),
                    ui.output_ui("ui_fx_calculator"),
                    ui.panel_conditional("input.show_raw_data", ui.tags.div(ui.output_data_frame("table_fx"), class_="table-card")),
                ),
                ui.panel_conditional(
                    "input.section === 'commodity'",
                    ui.output_ui("ui_commodity_toolbar"),
                    ui.output_ui("ui_commodity_summary"),
                    ui.tags.div(output_widget("plot_commodity", height="380px"), class_="plot-container"),
                    ui.tags.div(output_widget("plot_commodity_changes", height="240px"), class_="plot-container"),
                    ui.output_ui("ui_commodity_calculator"),
                    ui.panel_conditional("input.show_raw_data", ui.tags.div(ui.output_data_frame("table_commodity"), class_="table-card")),
                ),
                ui.panel_conditional(
                    "input.section === 'economic'",
                    ui.output_ui("ui_economic_toolbar"),
                    ui.output_ui("ui_economic_summary"),
                    ui.tags.div(output_widget("plot_economic", height="380px"), class_="plot-container"),
                    ui.panel_conditional("input.show_raw_data", ui.tags.div(ui.output_data_frame("table_economic"), class_="table-card")),
                ),
                class_="content-main",
            ),
            ui.tags.div(ui.output_ui("ui_ai_reporter"), class_="content-rail"),
            class_="content-grid",
        ),
        class_="main-container",
    ),
    title="Daily Market Analysis",
)


# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------


def server(input, output, session):
    err_msg = reactive.value(None)
    fallback_msg = reactive.value(None)
    stock_df = reactive.value(None)
    gainers_data = reactive.value(None)
    news_df = reactive.value(None)
    fx_df = reactive.value(None)
    commodity_df = reactive.value(None)
    economic_df = reactive.value(None)
    ai_report_text = reactive.value(None)
    ai_orchestrator_text = reactive.value(None)
    ai_analyst_text = reactive.value(None)
    ai_rag_trace_text = reactive.value(None)
    ai_provider_trace = reactive.value(None)
    ai_loading = reactive.value(False)

    def clear_err():
        err_msg.set(None)

    def clear_fallback():
        fallback_msg.set(None)

    def set_err(e):
        err_msg.set(f"Error: {e}")

    def _settle_after(source_getter):
        """A reactive.value that bumps one flush cycle after source_getter's value changes.

        Two brand-new Plotly widgets that both receive their first real value in the same
        reactive flush can race on the client (shinywidgets/anywidget) and one renders blank
        when its section was hidden at page load. Staggering the secondary chart's compute to
        the next flush avoids that simultaneous first-mount race.
        """
        tick = reactive.value(0)

        @reactive.effect
        def _():
            source_getter()

            def bump():
                with reactive.isolate():
                    tick.set(tick.get() + 1)

            session.on_flushed(bump, once=True)

        return tick

    stock_settle = _settle_after(stock_df.get)
    fx_settle = _settle_after(fx_df.get)
    commodity_settle = _settle_after(commodity_df.get)

    # ---- Header / hero ----

    @render.ui
    def section_title():
        return ui.tags.h3(SECTION_CHOICES.get(input.section(), input.section()))

    @render.ui
    def ui_top_status():
        api_ok = bool(API_KEY)
        cred = pick_llm_credentials()
        ai_ok = cred["ok"]
        return ui.TagList(
            ui.tags.span(
                ui.tags.span(class_="dot"),
                "Alpha Vantage connected" if api_ok else "API key missing",
                class_=f"nav-pill {'ok' if api_ok else 'warn'}",
            ),
            ui.tags.span(
                ui.tags.span(class_="dot"),
                "AI reporter ready" if ai_ok else "AI not configured",
                class_=f"nav-pill {'ok' if ai_ok else 'warn'}",
            ),
        )

    @render.ui
    def ui_section_help():
        sec = input.section() or "stock"
        msg = {
            "stock": "Enter a symbol (example: AAPL), click Fetch, then use the toolbar to adjust the window.",
            "gainers": "Click Fetch movers to load gainers, losers, and active symbols. Use filter/chart toggles to narrow focus.",
            "news": "Optionally provide tickers (example: AAPL,MSFT), then filter stories by keyword or sentiment.",
            "forex": "Set From/To currencies (example: EUR/USD), fetch data, then inspect the trend and summary cards.",
            "commodity": "Choose a commodity and interval, fetch, then review latest level and range in the chart card.",
            "economic": "Choose an indicator and interval, fetch, then compare current value against period min/max.",
            "ai": "Use the AI Reporter panel to generate a 3-agent analysis from your currently fetched data.",
        }.get(sec, "Fetch data in this section first, then run AI analysis if needed.")
        return ui.tags.p(msg, class_="hero-subtitle")

    @render.ui
    def ui_content_empty_state():
        sec = input.section() or "stock"
        if sec == "ai":
            return None
        has_data = {
            "stock": stock_df.get() is not None,
            "gainers": gainers_data.get() is not None,
            "news": news_df.get() is not None,
            "forex": fx_df.get() is not None,
            "commodity": commodity_df.get() is not None,
            "economic": economic_df.get() is not None,
        }.get(sec, True)
        if has_data:
            return None
        cta = {
            "stock": "Fetch", "gainers": "Fetch movers", "news": "Fetch news",
            "forex": "Fetch", "commodity": "Fetch", "economic": "Fetch",
        }.get(sec, "Fetch")
        return ui.tags.div(
            ui.tags.div(ui.HTML(CHART_ICON_SVG), class_="icon"),
            ui.tags.div(f"No {section_display_name(sec).lower()} data loaded yet", class_="title"),
            ui.tags.p(
                "Set your parameters above and click ", ui.tags.strong(cta),
                " to pull live data from Alpha Vantage. Charts, summary metrics, and calculators will appear here.",
                class_="desc",
            ),
            class_="empty-state-hero",
        )

    @render.ui
    def api_error_ui():
        err = err_msg.get()
        if not err:
            return None
        return ui.tags.div(
            ui.tags.p(err, style="margin: 0; color: #991b1b; font-weight: 600;"),
            class_="card-custom", style="border-left: 4px solid #dc2626; background: #fef2f2;",
        )

    @render.ui
    def api_fallback_ui():
        msg = fallback_msg.get()
        if not msg:
            return None
        return ui.tags.div(
            ui.tags.p(msg, style="margin: 0; color: #92400e; font-weight: 600;"),
            class_="card-custom", style="border-left: 4px solid #f59e0b; background: #fffbeb;",
        )

    # ---- Stock Daily ----

    @reactive.effect
    @reactive.event(input.fetch_stock)
    def _():
        clear_err()
        clear_fallback()
        try:
            sym = (input.stock_symbol() or "").strip()
            if not sym:
                set_err("Please enter a symbol.")
                return
            outsize = "full" if input.stock_full_history() else "compact"
            rk = f"stock|{sym.upper()}|{outsize}"
            res = safe_fetch_with_cache(rk, lambda: av_stock_daily(sym, outputsize=outsize))
            if not res["ok"]:
                set_err(res.get("error") or "Fetch failed.")
                return
            df = res["data"]
            if df is None or df.empty:
                set_err("No data returned. Check API key, symbol, or rate limit (5/min, 25/day).")
                stock_df.set(None)
            else:
                stock_df.set(df)
                if res.get("fallback"):
                    fallback_msg.set(res.get("msg") or "Using cached data.")
        except Exception as e:
            set_err(str(e))

    @render.ui
    def ui_stock_toolbar():
        df = stock_df.get()
        if df is None or df.empty:
            return None
        return ui.tags.div(
            ui.tags.div(
                ui.input_select("stock_days", "View", choices={"5": "Last 5 days", "21": "Last 21 days", "60": "Last 60 days", "90": "Last 90 days"}, selected="90"),
                class_="control-field",
            ),
            ui.tags.div(ui.input_checkbox("stock_show_ma", qmark("MA(20/50)", "Show 20-day and 50-day moving averages."), value=True), class_="form-check"),
            ui.tags.div(ui.input_checkbox("stock_show_bands", qmark("Bollinger", "Show Bollinger upper/lower volatility bands."), value=False), class_="form-check"),
            ui.tags.div(ui.input_numeric("stock_calc_invest", qmark("Invest $", "How much capital to simulate in the position calculator."), value=10000, min=0, step=100), class_="control-field"),
            ui.tags.div(ui.input_numeric("stock_calc_target", qmark("Target %", "Desired percent move used for projected P/L."), value=5, min=-100, max=500, step=0.5), class_="control-field"),
            ui.tags.div(ui.input_numeric("stock_scn_days", qmark("Scenario days", "Number of future days to project on the chart."), value=20, min=1, max=252, step=1), class_="control-field"),
            ui.tags.div(ui.input_numeric("stock_scn_drift", qmark("Scenario daily %", "Assumed daily percent change for scenario path."), value=0.2, min=-10, max=10, step=0.1), class_="control-field"),
            ui.download_button("download_stock_csv", "Download CSV", class_="btn-default"),
            class_="control-bar",
        )

    @render.ui
    def ui_stock_summary():
        df = stock_df.get()
        if df is None or df.empty:
            return None
        days = int(input.stock_days() or 90)
        d = df.head(days)
        r = d.iloc[0]
        prev = float(d["close"].iloc[1]) if len(d) >= 2 else float(r["close"])
        chg = round(100 * (r["close"] - prev) / prev, 2) if prev else None
        chg5 = round(100 * (r["close"] - d["close"].iloc[5]) / d["close"].iloc[5], 2) if len(d) >= 6 and d["close"].iloc[5] else None
        chg21 = round(100 * (r["close"] - d["close"].iloc[21]) / d["close"].iloc[21], 2) if len(d) >= 22 and d["close"].iloc[21] else None
        lret = np.diff(np.log(d["close"].astype(float)))
        lret = lret[np.isfinite(lret)]
        vol_ann = round(np.std(lret) * np.sqrt(252) * 100, 1) if len(lret) >= 5 else None

        def val_class(v):
            return "value" if v is None else ("value positive" if v >= 0 else "value negative")

        def txt(v):
            return "—" if v is None else f"{'+' if v >= 0 else ''}{v}%"

        regime = "Regime unavailable (need 50+ days)"
        if len(d) >= 50:
            ma20 = d["close"].head(20).mean()
            ma50 = d["close"].head(50).mean()
            if np.isfinite(ma20) and np.isfinite(ma50):
                regime = "Bullish trend regime" if ma20 >= ma50 else "Bearish trend regime"
            else:
                regime = "Regime unavailable"

        return ui.TagList(
            ui.tags.div(
                _stat("Last close", f"${r['close']:.2f}"),
                _stat("Change (1d)", txt(chg), val_class(chg)),
                _stat("Change (1w)", txt(chg5), val_class(chg5)),
                _stat("Change (1m)", txt(chg21), val_class(chg21)),
                _stat("High", f"${r.get('high', float('nan')):.2f}"),
                _stat("Low", f"${r.get('low', float('nan')):.2f}"),
                _stat("Ann. volatility", f"{vol_ann}%" if vol_ann is not None else "—"),
                _stat("Volume", f"{r.get('volume', 0):,.0f}"),
                class_="stats-row",
            ),
            ui.tags.div(
                ui.tags.p(
                    f"Signal: {regime}. This combines moving-average structure with recent returns to give quick "
                    "context, not investment advice.",
                    style="margin: 0; color: var(--text-muted); font-size: 0.82rem;",
                ),
                class_="card-custom",
            ),
        )

    @render_widget
    def plot_stock():
        df = stock_df.get()
        req(df is not None and not df.empty)
        days = int(input.stock_days() or 90)
        d = df.head(days).dropna(subset=["date", "close"]).sort_values("date").copy()
        req(not d.empty)
        d["ma20"] = d["close"].rolling(20).mean()
        d["ma50"] = d["close"].rolling(50).mean()
        sd20 = d["close"].rolling(20).std()
        d["bb_up"] = d["ma20"] + 2 * sd20
        d["bb_dn"] = d["ma20"] - 2 * sd20

        fig = go.Figure()
        fig.add_trace(go.Scatter(x=d["date"], y=d["close"], mode="lines", name="Close", line=dict(color=CHART_COLORS["close"], width=2.4)))
        if input.stock_show_ma():
            fig.add_trace(go.Scatter(x=d["date"], y=d["ma20"], mode="lines", name="MA20", line=dict(color=CHART_COLORS["ma20"], width=1.8)))
            fig.add_trace(go.Scatter(x=d["date"], y=d["ma50"], mode="lines", name="MA50", line=dict(color=CHART_COLORS["ma50"], width=1.8)))
        if input.stock_show_bands():
            fig.add_trace(go.Scatter(x=d["date"], y=d["bb_up"], mode="lines", name="Bollinger Upper", line=dict(color=CHART_COLORS["boll"], width=1.4, dash="dash")))
            fig.add_trace(go.Scatter(x=d["date"], y=d["bb_dn"], mode="lines", name="Bollinger Lower", line=dict(color=CHART_COLORS["boll2"], width=1.4, dash="dash")))

        scn_days = int(input.stock_scn_days() or 20)
        scn_drift = float(input.stock_scn_drift() or 0)
        subtitle = None
        if scn_days > 0:
            last_dt = d["date"].max()
            last_px = float(d["close"].iloc[-1])
            if np.isfinite(last_px) and last_px > 0:
                fut_dates = pd.date_range(last_dt + pd.Timedelta(days=1), periods=scn_days, freq="D")
                fut_px = last_px * np.cumprod(np.repeat(1 + scn_drift / 100, scn_days))
                fig.add_trace(go.Scatter(x=fut_dates, y=fut_px, mode="lines+markers", name="Scenario", line=dict(color=CHART_COLORS["scenario"], width=2, dash="dashdot")))
                subtitle = "Orange line: user-defined scenario path"
        market_layout(fig, y_title="Close", x_title="Date", subtitle=subtitle)
        return fig

    @render_widget
    def plot_stock_returns():
        stock_settle.get()
        with reactive.isolate():
            df = stock_df.get()
        req(df is not None and len(df) >= 3)
        days = int(input.stock_days() or 90)
        d = df.head(days).sort_values("date").copy()
        d["ret"] = 100 * (d["close"] / d["close"].shift(1) - 1)
        d = d.dropna(subset=["ret"])
        req(len(d) >= 2)
        colors = [CHART_COLORS["up"] if r >= 0 else CHART_COLORS["down"] for r in d["ret"]]
        fig = go.Figure(go.Bar(x=d["date"], y=d["ret"], marker_color=colors, name="Daily return"))
        fig.add_hline(y=0, line_color="#94a3b8", line_width=1)
        market_layout(fig, y_title="Daily return (%)", x_title="Date", show_legend=False, subtitle="Return bars (up/down days)")
        return fig

    @render.ui
    def ui_stock_calculator():
        df = stock_df.get()
        if df is None or df.empty:
            return None
        px = float(df["close"].iloc[0])
        if not np.isfinite(px) or px <= 0:
            return None
        invest = float(input.stock_calc_invest() or 0)
        tgt = float(input.stock_calc_target() or 0)
        shares = invest / px if invest > 0 else 0
        tgt_px = px * (1 + tgt / 100)
        pnl = shares * (tgt_px - px)
        return ui.tags.div(
            ui.tags.h4("Position calculator"),
            ui.tags.div(
                _stat("Implied shares", f"{shares:.2f}"),
                _stat("Current price", f"${px:.2f}"),
                _stat("Target price", f"${tgt_px:.2f}"),
                _stat("Projected P/L", f"${pnl:.2f}", "value positive" if pnl >= 0 else "value negative"),
                class_="stats-row",
            ),
            ui.tags.p("Educational what-if only; ignores fees, slippage, and taxes.", style="margin: 0; color: var(--text-muted); font-size: 0.8rem;"),
            class_="card-custom",
        )

    @render.data_frame
    def table_stock():
        return _table_card(stock_df.get())

    @render.download_button(filename=lambda: f"stock_{(input.stock_symbol() or 'symbol')}_{date.today()}.csv")
    def download_stock_csv():
        df = stock_df.get()
        if df is not None and not df.empty:
            yield df.to_csv(index=False)

    # ---- Top Gainers / Losers ----

    @reactive.effect
    @reactive.event(input.fetch_gainers)
    def _():
        clear_err()
        clear_fallback()
        try:
            res = safe_fetch_with_cache("gainers|default", av_top_gainers_losers)
            if not res["ok"]:
                set_err(res.get("error") or "Fetch failed.")
                return
            d = res["data"]
            ng = len(d["top_gainers"]) if d.get("top_gainers") is not None else 0
            if ng == 0:
                set_err("No data returned. Check API key or rate limit (5/min, 25/day).")
                gainers_data.set(None)
            else:
                gainers_data.set(d)
                if res.get("fallback"):
                    fallback_msg.set(res.get("msg") or "Using cached data.")
        except Exception as e:
            set_err(str(e))

    @render.ui
    def ui_gainers_toolbar():
        d = gainers_data.get()
        if d is None:
            return None
        return ui.tags.div(
            ui.tags.div(ui.input_select("gainers_n", "Show", choices=["5", "10", "20"], selected="20"), class_="control-field"),
            ui.tags.div(ui.input_text("gainers_filter", "Filter", placeholder="e.g. AAPL"), class_="control-field"),
            ui.tags.div(ui.input_checkbox("gainers_chart", "Bar chart", value=False), class_="form-check"),
            ui.download_button("download_gainers_csv", "Download CSV", class_="btn-default"),
            class_="control-bar",
        )

    @reactive.calc
    def gainers_filtered():
        d = gainers_data.get()
        if d is None:
            return None
        flt = (input.gainers_filter() or "").strip()
        n_show = int(input.gainers_n() or 20)

        def filter_df(df):
            if df is None or df.empty:
                return df
            if not flt:
                return df.head(n_show)
            tk = _find_col(df, "ticker")
            mask = df[tk].astype(str).str.contains(re.escape(flt), case=False, na=False)
            return df[mask].head(n_show)

        return {
            "top_gainers": filter_df(d.get("top_gainers")),
            "top_losers": filter_df(d.get("top_losers")),
            "top_activated": filter_df(d.get("top_activated")),
        }

    def _ticker_cards(df: pd.DataFrame | None, name: str):
        if df is None or df.empty:
            return None
        tk = _find_col(df, "ticker", default_idx=0)
        pr = _find_col(df, "price", default_idx=min(1, len(df.columns) - 1))
        ch = _find_col(df, "changepercentage", "change_percentage", "changepercent")
        vol = _find_col(df, "volume")
        items = []
        for i, (_, row) in enumerate(df.iterrows(), start=1):
            tkr = str(row[tk]) if tk else "—"
            try:
                prc = f"{float(row[pr]):.2f}"
            except Exception:
                prc = str(row[pr]) if pr else "—"
            chg = str(row[ch]) if ch else ""
            vol_txt = None
            if vol:
                try:
                    vol_txt = f"{float(row[vol]):,.0f}"
                except Exception:
                    vol_txt = None
            is_neg = "-" in chg
            cl = "change-neg" if (name == "Top Losers" or is_neg) else "change-pos"
            items.append(
                ui.tags.div(
                    ui.tags.div(f"#{i}", style="font-size: 0.75rem; font-weight: 700; color: var(--text-muted);"),
                    ui.tags.div(tkr, class_="ticker"),
                    ui.tags.div(prc, style="font-size: 0.9rem; color: var(--text-muted);"),
                    ui.tags.div(chg, class_=cl) if chg else None,
                    ui.tags.div(f"Vol: {vol_txt}", style="font-size: 0.75rem; color: var(--text-muted);") if vol_txt else None,
                    class_="ticker-card",
                )
            )
        return ui.TagList(ui.tags.h4(name), ui.tags.div(*items, style="display: flex; flex-wrap: wrap; gap: 0.35rem;"))

    @render.ui
    def ui_gainers_cards():
        d = gainers_filtered()
        if d is None:
            return ui.tags.p("Click 'Fetch movers' to load data.", style="color: var(--text-muted);")
        return ui.TagList(
            ui.tags.div(_ticker_cards(d.get("top_gainers"), "Top Gainers"), class_="card-custom"),
            ui.tags.div(_ticker_cards(d.get("top_losers"), "Top Losers"), class_="card-custom"),
            ui.tags.div(_ticker_cards(d.get("top_activated"), "Most Actively Traded"), class_="card-custom"),
        )

    @render.ui
    def ui_gainers_chart():
        if not input.gainers_chart():
            return None
        d = gainers_filtered()
        tg = d.get("top_gainers") if d else None
        if tg is None or tg.empty:
            return None
        return ui.tags.div(output_widget("plot_gainers_bars", height="420px"), class_="plot-container")

    @render_widget
    def plot_gainers_bars():
        req(input.gainers_chart())
        d = gainers_filtered()
        df = d.get("top_gainers") if d else None
        req(df is not None and not df.empty)
        tk = _find_col(df, "ticker")
        ch = _find_col(df, "changepercentage", "change_percentage", "changepercent")
        req(ch is not None)
        df = df.copy()
        df["ticker_"] = df[tk]
        df["change_"] = pd.to_numeric(df[ch].astype(str).str.replace(r"[^0-9.\-]", "", regex=True), errors="coerce")
        df = df.dropna(subset=["change_"]).head(15).sort_values("change_")
        req(not df.empty)
        colors = [CHART_COLORS["up"] if c > 0 else CHART_COLORS["down"] for c in df["change_"]]
        fig = go.Figure(go.Bar(x=df["change_"], y=df["ticker_"], orientation="h", marker_color=colors))
        market_layout(fig, x_title="Change %", show_legend=False, subtitle="Top gainers")
        return fig

    @render.data_frame
    def table_gainers():
        d = gainers_data.get()
        return _table_card(d.get("top_gainers") if d else None)

    @render.data_frame
    def table_losers():
        d = gainers_data.get()
        return _table_card(d.get("top_losers") if d else None)

    @render.data_frame
    def table_active():
        d = gainers_data.get()
        return _table_card(d.get("top_activated") if d else None)

    @render.download_button(filename=lambda: f"gainers_losers_{date.today()}.csv")
    def download_gainers_csv():
        d = gainers_data.get()
        if d is not None:
            frames = []
            for key, label in (("top_gainers", "gainers"), ("top_losers", "losers"), ("top_activated", "active")):
                df = d.get(key)
                if df is not None and not df.empty:
                    df = df.copy()
                    df["group"] = label
                    frames.append(df)
            if frames:
                yield pd.concat(frames, ignore_index=True).to_csv(index=False)

    # ---- News ----

    @reactive.effect
    @reactive.event(input.fetch_news)
    def _():
        clear_err()
        clear_fallback()
        try:
            tk = (input.news_tickers() or "").strip()
            lim = int(input.news_limit() or 20)
            rk = f"news|{tk.lower()}|{lim}"
            res = safe_fetch_with_cache(rk, lambda: av_news_sentiment(tickers=tk, limit=lim))
            if not res["ok"]:
                set_err(res.get("error") or "Fetch failed.")
                return
            df = res["data"]
            if df is None or df.empty:
                set_err("No news returned. Check API key or rate limit (5/min, 25/day).")
                news_df.set(None)
            else:
                news_df.set(df)
                if res.get("fallback"):
                    fallback_msg.set(res.get("msg") or "Using cached data.")
        except Exception as e:
            set_err(str(e))

    @render.ui
    def ui_news_toolbar():
        df = news_df.get()
        if df is None or df.empty:
            return None
        return ui.tags.div(
            ui.tags.div(ui.input_select("news_n", qmark("Show", "Maximum number of headlines to display."), choices=["10", "25", "50"], selected="50"), class_="control-field"),
            ui.tags.div(ui.input_text("news_keyword", qmark("Keyword", "Filter headlines containing this text."), placeholder="e.g. Fed"), class_="control-field"),
            ui.tags.div(ui.input_select("news_sentiment_filter", qmark("Sentiment", "Filter by bullish, bearish, or neutral labels."), choices=["All", "Bullish", "Bearish", "Neutral"], selected="All"), class_="control-field"),
            ui.tags.div(ui.input_checkbox("news_show_dist", qmark("Show sentiment mix", "Toggle sentiment distribution chart."), value=True), class_="form-check"),
            ui.download_button("download_news_csv", "Download CSV", class_="btn-default"),
            class_="control-bar",
        )

    @reactive.calc
    def news_filtered():
        df = news_df.get()
        if df is None or df.empty:
            return df
        kw = (input.news_keyword() or "").strip()
        sent = input.news_sentiment_filter() or "All"
        n_show = int(input.news_n() or 50)
        out = df
        if kw:
            mask = out["title"].astype(str).str.contains(re.escape(kw), case=False, na=False) | out["summary"].astype(str).str.contains(re.escape(kw), case=False, na=False)
            out = out[mask]
        if sent != "All":
            lbl = out["overall_sentiment_label"].astype(str).str.lower()
            if sent == "Bullish":
                idx = lbl.str.contains("bullish|positive", regex=True)
            elif sent == "Bearish":
                idx = lbl.str.contains("bearish|negative", regex=True)
            else:
                idx = lbl.str.contains("neutral", regex=True)
            out = out[idx]
        return out.head(n_show)

    def _sentiment_class(lbl) -> str:
        if not lbl:
            return "sentiment-neutral"
        l = str(lbl).lower()
        if "bullish" in l or "positive" in l:
            return "sentiment-bullish"
        if "bearish" in l or "negative" in l:
            return "sentiment-bearish"
        return "sentiment-neutral"

    @render.ui
    def ui_news_cards():
        df = news_filtered()
        if df is None or df.empty:
            return ui.tags.p("Click 'Fetch news' or loosen filters.", style="color: var(--text-muted);")
        cards = []
        for _, r in df.iterrows():
            title = str(r.get("title") or "—")
            url = str(r.get("url") or "")
            src = str(r.get("source") or "")
            t = str(r.get("time_published") or "")
            if len(t) >= 8:
                t = t[:10]
            sent = str(r.get("overall_sentiment_label") or "")
            title_el = (
                ui.tags.a(title, href=url, target="_blank", rel="noopener")
                if url else ui.tags.p(title, style="margin: 0 0 0.5rem 0; font-weight: 600;")
            )
            meta = ui.tags.div(
                ui.tags.span(f" {src}") if src else None,
                ui.tags.span(f" · {t}") if t else None,
                ui.tags.span(sent, class_=f"sentiment-badge {_sentiment_class(sent)}", style="margin-left: 0.5rem;") if sent else None,
                style="font-size: 0.85rem; color: var(--text-muted);",
            )
            cards.append(ui.tags.div(title_el, meta, class_="news-card"))
        return ui.TagList(*cards)

    @render_widget
    def plot_news_sentiment():
        req(input.news_show_dist())
        df = news_filtered()
        req(df is not None and not df.empty and "overall_sentiment_label" in df.columns)
        lbl = df["overall_sentiment_label"].astype(str).str.lower()
        cls = np.where(
            lbl.str.contains("bullish|positive", regex=True), "Bullish",
            np.where(lbl.str.contains("bearish|negative", regex=True), "Bearish", "Neutral"),
        )
        counts = pd.Series(cls).value_counts().reindex(["Bullish", "Neutral", "Bearish"]).fillna(0)
        req(counts.sum() > 0)
        colors = {"Bullish": CHART_COLORS["up"], "Neutral": "#64748b", "Bearish": CHART_COLORS["down"]}
        fig = go.Figure(
            go.Bar(x=counts.index, y=counts.values, marker_color=[colors[k] for k in counts.index], text=counts.values.astype(int), textposition="outside")
        )
        market_layout(fig, y_title="Headline count", show_legend=False, subtitle="Sentiment distribution (filtered set)")
        return fig

    @render.data_frame
    def table_news():
        df = news_df.get()
        if df is not None and not df.empty:
            cols = [c for c in ["title", "time_published", "overall_sentiment_label", "source", "url"] if c in df.columns]
            if cols:
                df = df[cols]
        return _table_card(df)

    @render.download_button(filename=lambda: f"news_{date.today()}.csv")
    def download_news_csv():
        df = news_df.get()
        if df is not None and not df.empty:
            yield df.to_csv(index=False)

    # ---- Forex ----

    @reactive.effect
    @reactive.event(input.fetch_fx)
    def _():
        clear_err()
        clear_fallback()
        try:
            frm = (input.fx_from() or "").strip()
            to = (input.fx_to() or "").strip()
            if not frm or not to:
                set_err("Enter From and To currencies.")
                return
            rk = f"fx|{frm.upper()}|{to.upper()}"
            res = safe_fetch_with_cache(rk, lambda: av_fx_daily(from_symbol=frm, to_symbol=to))
            if not res["ok"]:
                set_err(res.get("error") or "Fetch failed.")
                return
            df = res["data"]
            if df is None or df.empty:
                set_err("No FX data returned. Check API key or rate limit (5/min, 25/day).")
                fx_df.set(None)
            else:
                fx_df.set(df)
                if res.get("fallback"):
                    fallback_msg.set(res.get("msg") or "Using cached data.")
        except Exception as e:
            set_err(str(e))

    @render.ui
    def ui_fx_toolbar():
        df = fx_df.get()
        if df is None or df.empty:
            return None
        return ui.tags.div(
            ui.tags.div(ui.input_select("fx_points", qmark("Chart points", "How many historical points to display in charts."), choices=["30", "60", "90"], selected="90"), class_="control-field"),
            ui.tags.div(ui.input_numeric("fx_calc_amount", qmark("Amount", "Base currency amount for conversion calculator."), value=1000, min=0, step=100), class_="control-field"),
            ui.tags.div(ui.input_numeric("fx_calc_shift", qmark("Rate shock %", "One-time percentage shift used in stress calculation."), value=1, min=-50, max=50, step=0.1), class_="control-field"),
            ui.tags.div(ui.input_numeric("fx_scn_days", qmark("Scenario days", "Number of future periods in scenario projection."), value=15, min=1, max=180, step=1), class_="control-field"),
            ui.tags.div(ui.input_numeric("fx_scn_drift", qmark("Scenario daily %", "Assumed daily % move for projected path."), value=0.05, min=-5, max=5, step=0.01), class_="control-field"),
            ui.download_button("download_fx_csv", "Download CSV", class_="btn-default"),
            class_="control-bar",
        )

    @render.ui
    def ui_fx_summary():
        df = fx_df.get()
        if df is None or df.empty:
            return None
        rate = float(df["close"].iloc[0])
        if not np.isfinite(rate):
            return None
        return ui.tags.div(
            _stat("Latest rate", f"{rate:.4f}", "hero-rate"),
            _stat("Low (period)", f"{df['close'].min():.4f}"),
            _stat("High (period)", f"{df['close'].max():.4f}"),
            _stat("Days shown", f"{len(df)}"),
            class_="stats-row",
        )

    @render_widget
    def plot_fx():
        df = fx_df.get()
        req(df is not None and not df.empty)
        pts = int(input.fx_points() or 90)
        d = df.head(pts).dropna(subset=["date", "close"]).sort_values("date")
        req(not d.empty)
        fig = go.Figure(go.Scatter(x=d["date"], y=d["close"], mode="lines", name="Observed", line=dict(color=CHART_COLORS["close"], width=2.4)))
        scn_days = int(input.fx_scn_days() or 15)
        scn_drift = float(input.fx_scn_drift() or 0)
        subtitle = None
        if scn_days > 0:
            last_dt = d["date"].max()
            last_px = float(d["close"].iloc[-1])
            if np.isfinite(last_px) and last_px > 0:
                fut_dates = pd.date_range(last_dt + pd.Timedelta(days=1), periods=scn_days, freq="D")
                fut_px = last_px * np.cumprod(np.repeat(1 + scn_drift / 100, scn_days))
                fig.add_trace(go.Scatter(x=fut_dates, y=fut_px, mode="lines+markers", name="Scenario", line=dict(color=CHART_COLORS["scenario_fx"], width=2, dash="dashdot")))
                subtitle = "Orange path: user-defined FX scenario"
        market_layout(fig, y_title="Close", x_title="Date", subtitle=subtitle)
        return fig

    @render_widget
    def plot_fx_returns():
        fx_settle.get()
        with reactive.isolate():
            df = fx_df.get()
        req(df is not None and len(df) >= 3)
        pts = int(input.fx_points() or 90)
        d = df.head(pts).sort_values("date").copy()
        d["ret"] = 100 * (d["close"] / d["close"].shift(1) - 1)
        d = d.dropna(subset=["ret"])
        req(len(d) >= 2)
        colors = [CHART_COLORS["up"] if r >= 0 else CHART_COLORS["scenario_fx"] for r in d["ret"]]
        fig = go.Figure()
        fig.add_trace(go.Bar(x=d["date"], y=d["ret"], marker_color=colors, name="Return"))
        window = max(2, len(d) // 8)
        trend = d["ret"].rolling(window, min_periods=1, center=True).mean()
        fig.add_trace(go.Scatter(x=d["date"], y=trend, mode="lines", name="Trend", line=dict(color=CHART_COLORS["trend"], width=2)))
        market_layout(fig, y_title="Return (%)", x_title="Date", subtitle="FX return momentum with smooth trend")
        return fig

    @render.ui
    def ui_fx_calculator():
        df = fx_df.get()
        if df is None or df.empty:
            return None
        rate = float(df["close"].iloc[0])
        if not np.isfinite(rate) or rate <= 0:
            return None
        amt = float(input.fx_calc_amount() or 0)
        shock = float(input.fx_calc_shift() or 0)
        converted = amt * rate
        shocked_rate = rate * (1 + shock / 100)
        shocked_value = amt * shocked_rate
        delta = shocked_value - converted
        pair = f"{(input.fx_from() or 'FROM').upper()}/{(input.fx_to() or 'TO').upper()}"
        return ui.tags.div(
            ui.tags.h4("FX conversion and stress calculator"),
            ui.tags.div(
                _stat("Pair", pair),
                _stat("Current conversion", f"{converted:.2f}"),
                _stat("Shocked value", f"{shocked_value:.2f}"),
                _stat("Value change", f"{delta:.2f}", "value positive" if delta >= 0 else "value negative"),
                class_="stats-row",
            ),
            ui.tags.p("Stress result applies a simple percentage shock to latest close rate.", style="margin: 0; color: var(--text-muted); font-size: 0.8rem;"),
            class_="card-custom",
        )

    @render.data_frame
    def table_fx():
        return _table_card(fx_df.get())

    @render.download_button(filename=lambda: f"fx_{(input.fx_from() or 'from')}_{(input.fx_to() or 'to')}_{date.today()}.csv")
    def download_fx_csv():
        df = fx_df.get()
        if df is not None and not df.empty:
            yield df.to_csv(index=False)

    # ---- Commodities ----

    @reactive.effect
    @reactive.event(input.fetch_commodity)
    def _():
        clear_err()
        clear_fallback()
        try:
            com = input.commodity() or "WHEAT"
            interval = input.commodity_interval() or "monthly"
            rk = f"commodity|{com}|{interval}"
            res = safe_fetch_with_cache(rk, lambda: av_commodity(commodity=com, interval=interval))
            if not res["ok"]:
                set_err(res.get("error") or "Fetch failed.")
                return
            commodity_df.set(res["data"])
            if res.get("fallback"):
                fallback_msg.set(res.get("msg") or "Using cached data.")
        except Exception as e:
            set_err(str(e))

    @render.ui
    def ui_commodity_toolbar():
        df = commodity_df.get()
        if df is None or df.empty:
            return None
        return ui.tags.div(
            ui.tags.div(ui.input_select("commodity_points", qmark("Chart points", "How many commodity observations to display."), choices=["30", "60", "90"], selected="60"), class_="control-field"),
            ui.tags.div(ui.input_numeric("commodity_calc_units", qmark("Units", "Quantity used in notional and P/L scenario calculator."), value=100, min=0, step=1), class_="control-field"),
            ui.tags.div(ui.input_numeric("commodity_calc_move", qmark("Price move %", "One-time price change used in scenario notional."), value=3, min=-80, max=200, step=0.5), class_="control-field"),
            ui.tags.div(ui.input_numeric("commodity_scn_steps", qmark("Scenario steps", "Future periods projected on scenario line."), value=12, min=1, max=120, step=1), class_="control-field"),
            ui.tags.div(ui.input_numeric("commodity_scn_drift", qmark("Scenario step %", "Assumed percent move per projected step."), value=0.5, min=-30, max=30, step=0.1), class_="control-field"),
            ui.download_button("download_commodity_csv", "Download CSV", class_="btn-default"),
            class_="control-bar",
        )

    @render.ui
    def ui_commodity_summary():
        df = commodity_df.get()
        if df is None or df.empty:
            return None
        vv = df["value"].dropna()
        if vv.empty:
            return None
        return ui.tags.div(
            _stat(f"{input.commodity()} — Latest", f"{vv.iloc[0]:,.2f}"),
            _stat("Period low", f"{vv.min():,.2f}"),
            _stat("Period high", f"{vv.max():,.2f}"),
            _stat("Data points", f"{len(df)}"),
            class_="stats-row",
        )

    @render_widget
    def plot_commodity():
        df = commodity_df.get()
        req(df is not None and not df.empty)
        pts = int(input.commodity_points() or 60)
        d = df.head(pts).dropna(subset=["date", "value"]).sort_values("date")
        req(not d.empty)
        fig = go.Figure(go.Scatter(x=d["date"], y=d["value"], mode="lines", name="Observed", line=dict(color=CHART_COLORS["close"], width=2.4)))
        scn_steps = int(input.commodity_scn_steps() or 12)
        scn_drift = float(input.commodity_scn_drift() or 0)
        subtitle = None
        if scn_steps > 0:
            last_dt = d["date"].max()
            last_v = float(d["value"].iloc[-1])
            if np.isfinite(last_v) and last_v > 0:
                fut_dates = pd.date_range(last_dt + pd.Timedelta(days=1), periods=scn_steps, freq="D")
                fut_vals = last_v * np.cumprod(np.repeat(1 + scn_drift / 100, scn_steps))
                fig.add_trace(go.Scatter(x=fut_dates, y=fut_vals, mode="lines+markers", name="Scenario", line=dict(color=CHART_COLORS["scenario"], width=2, dash="dashdot")))
                subtitle = "Orange path: user-defined commodity scenario"
        market_layout(fig, y_title="Value", x_title="Date", subtitle=subtitle)
        return fig

    @render_widget
    def plot_commodity_changes():
        commodity_settle.get()
        with reactive.isolate():
            df = commodity_df.get()
        req(df is not None and len(df) >= 3)
        pts = int(input.commodity_points() or 60)
        d = df.head(pts).dropna(subset=["date", "value"]).sort_values("date").copy()
        d["chg"] = 100 * (d["value"] / d["value"].shift(1) - 1)
        d = d.dropna(subset=["chg"])
        req(not d.empty)
        colors = [CHART_COLORS["up"] if c >= 0 else CHART_COLORS["down"] for c in d["chg"]]
        fig = go.Figure(go.Bar(x=d["date"], y=d["chg"], marker_color=colors))
        fig.add_hline(y=0, line_color="#94a3b8", line_width=1)
        market_layout(fig, y_title="Period change (%)", x_title="Date", show_legend=False, subtitle="Commodity change bars")
        return fig

    @render.ui
    def ui_commodity_calculator():
        df = commodity_df.get()
        if df is None or df.empty:
            return None
        px = float(df["value"].iloc[0])
        if not np.isfinite(px) or px <= 0:
            return None
        units = float(input.commodity_calc_units() or 0)
        move = float(input.commodity_calc_move() or 0)
        base = units * px
        shock_px = px * (1 + move / 100)
        shock_val = units * shock_px
        pnl = shock_val - base
        return ui.tags.div(
            ui.tags.h4("Commodity scenario calculator"),
            ui.tags.div(
                _stat("Units", f"{units:,.0f}"),
                _stat("Current notional", f"{base:.2f}"),
                _stat("Scenario notional", f"{shock_val:.2f}"),
                _stat("Scenario P/L", f"{pnl:.2f}", "value positive" if pnl >= 0 else "value negative"),
                class_="stats-row",
            ),
            ui.tags.p("Simple sensitivity estimate using latest displayed value and user-defined percentage move.", style="margin: 0; color: var(--text-muted); font-size: 0.8rem;"),
            class_="card-custom",
        )

    @render.data_frame
    def table_commodity():
        return _table_card(commodity_df.get())

    @render.download_button(filename=lambda: f"commodity_{(input.commodity() or '')}_{date.today()}.csv")
    def download_commodity_csv():
        df = commodity_df.get()
        if df is not None and not df.empty:
            yield df.to_csv(index=False)

    # ---- Economic ----

    @reactive.effect
    @reactive.event(input.fetch_economic)
    def _():
        clear_err()
        clear_fallback()
        try:
            ind = input.economic_indicator()
            interval = input.economic_interval()
            rk = f"economic|{ind}|{interval}"

            def fetch():
                if ind == "TREASURY_YIELD":
                    return av_economic(indicator=ind, interval=interval, maturity="10year")
                if ind == "REAL_GDP":
                    return av_economic(indicator=ind, interval=interval if interval in ("quarterly", "annual") else "annual")
                return av_economic(indicator=ind, interval=interval)

            res = safe_fetch_with_cache(rk, fetch)
            if not res["ok"]:
                set_err(res.get("error") or "Fetch failed.")
                return
            economic_df.set(res["data"])
            if res.get("fallback"):
                fallback_msg.set(res.get("msg") or "Using cached data.")
        except Exception as e:
            set_err(str(e))

    @render.ui
    def ui_economic_toolbar():
        df = economic_df.get()
        if df is None or df.empty:
            return None
        return ui.tags.div(
            ui.tags.div(ui.input_select("economic_points", "Chart points", choices=["30", "60", "90"], selected="60"), class_="control-field"),
            ui.download_button("download_economic_csv", "Download CSV", class_="btn-default"),
            class_="control-bar",
        )

    @render.ui
    def ui_economic_summary():
        df = economic_df.get()
        if df is None or df.empty:
            return None
        ycol = "value" if "value" in df.columns else df.columns[1]
        vv = pd.to_numeric(df[ycol], errors="coerce").dropna()
        if vv.empty:
            return None
        return ui.tags.div(
            _stat(f"{input.economic_indicator()} — Latest", f"{vv.iloc[0]:,.4f}"),
            _stat("Period min", f"{vv.min():,.4f}"),
            _stat("Period max", f"{vv.max():,.4f}"),
            _stat("Observations", f"{len(df)}"),
            class_="stats-row",
        )

    @render_widget
    def plot_economic():
        df = economic_df.get()
        req(df is not None and not df.empty)
        pts = int(input.economic_points() or 60)
        xcol = "date" if "date" in df.columns else df.columns[0]
        ycol = "value" if "value" in df.columns else df.columns[1]
        d = df.head(pts).copy()
        d[ycol] = pd.to_numeric(d[ycol], errors="coerce")
        d = d.dropna(subset=[xcol, ycol])
        req(not d.empty)
        fig = go.Figure(go.Scatter(x=d[xcol], y=d[ycol], mode="lines", line=dict(color=CHART_COLORS["close"], width=2.4)))
        market_layout(fig, y_title=ycol, x_title=xcol, show_legend=False)
        return fig

    @render.data_frame
    def table_economic():
        return _table_card(economic_df.get())

    @render.download_button(filename=lambda: f"economic_{(input.economic_indicator() or '')}_{date.today()}.csv")
    def download_economic_csv():
        df = economic_df.get()
        if df is not None and not df.empty:
            yield df.to_csv(index=False)

    # ---- AI Reporter ----

    def report_choices() -> dict:
        sec = input.section()
        mapping = {
            "stock": {"brief": "Brief on current data", "stock": "Stock snapshot", "stock_trend": "Stock trend (day/week/month/year)"},
            "gainers": {"movers": "Top movers insight"},
            "news": {"news": "News briefing"},
            "forex": {"forex_brief": "Brief on current forex data", "forex_snapshot": "Forex snapshot", "forex_trend": "Forex trend (day/week/month/year)"},
            "commodity": {"commodity_brief": "Brief on current commodity data", "commodity_snapshot": "Commodity snapshot", "commodity_trend": "Commodity trend (day/week/month/year)"},
            "economic": {"economic_brief": "Brief on indicator data", "economic_snapshot": "Indicator snapshot", "economic_trend": "Indicator trend (day/week/month/year)"},
        }
        return mapping.get(sec, {"brief": "Brief on current data"})

    def build_ai_context(report_type: str) -> str:
        parts = []
        if report_type in ("movers", "brief"):
            d = gainers_data.get()
            if d is not None:
                g = d.get("top_gainers")
                if g is not None and not g.empty:
                    g5 = g.head(5)
                    tk = _find_col(g5, "ticker")
                    parts.append(f"Top gainers (sample): {', '.join(g5[tk].astype(str))}")
                l = d.get("top_losers")
                if l is not None and not l.empty:
                    l5 = l.head(5)
                    tk = _find_col(l5, "ticker")
                    parts.append(f"Top losers (sample): {', '.join(l5[tk].astype(str))}")
        if report_type in ("news", "brief"):
            nw = news_df.get()
            if nw is not None and not nw.empty:
                parts.append("Recent headlines: " + " | ".join(nw["title"].astype(str).head(8)))
        if report_type in ("stock", "brief"):
            st = stock_df.get()
            if st is not None and not st.empty:
                r = st.iloc[0]
                parts.append(f"Latest stock: close {r['close']:.2f}, high {r.get('high', float('nan')):.2f}, low {r.get('low', float('nan')):.2f}")
        if report_type in ("forex_brief", "forex_snapshot"):
            fx = fx_df.get()
            if fx is not None and not fx.empty:
                parts.append(
                    f"Forex {input.fx_from() or 'From'}/{input.fx_to() or 'To'}: latest close {float(fx['close'].iloc[0]):.4f}, "
                    f"high {float(fx['close'].max()):.4f}, low {float(fx['close'].min()):.4f} (over {len(fx)} days)"
                )
        if report_type in ("commodity_brief", "commodity_snapshot"):
            co = commodity_df.get()
            if co is not None and not co.empty:
                v = co["value"].dropna()
                if len(v):
                    parts.append(f"Commodity {input.commodity() or ''}: latest {v.iloc[0]:.2f}, period min {v.min():.2f}, max {v.max():.2f} ({len(co)} points)")
        if report_type in ("economic_brief", "economic_snapshot"):
            ec = economic_df.get()
            if ec is not None and not ec.empty:
                v = pd.to_numeric(ec.get("value"), errors="coerce").dropna()
                if len(v):
                    parts.append(f"Indicator {input.economic_indicator() or ''}: latest {v.iloc[0]:.4f}, period min {v.min():.4f}, max {v.max():.4f} ({len(ec)} points)")
        if not parts:
            return "No data for this section yet. Fetch data in the current section first."
        return "\n\n".join(parts)

    @reactive.effect
    def _():
        ch = report_choices()
        ui.update_select("ai_report_type", choices=ch, selected=next(iter(ch)))

    @render.ui
    def ui_ai_reporter():
        cred = pick_llm_credentials()
        has_key = cred["ok"]
        loading = ai_loading.get()
        report = ai_report_text.get()
        orch = ai_orchestrator_text.get()
        an = ai_analyst_text.get()
        ragtr = ai_rag_trace_text.get()
        ch = report_choices()
        rt = input.ai_report_type() if "ai_report_type" in input else None
        needs_ai = (rt or "") in (
            "brief", "stock", "movers", "news", "forex_brief", "forex_snapshot",
            "commodity_brief", "commodity_snapshot", "economic_brief", "economic_snapshot",
        )
        providers = sorted({c["provider"] for c in cred["creds"]}) if has_key else []
        prov_lab = f" (available: {', '.join(providers)})" if providers else ""

        if needs_ai and not has_key:
            return ui.tags.div(
                ui.tags.div("Multi-agent AI analysis", class_="ai-title"),
                ui.tags.div(
                    "Generate a concise market summary from the data you have fetched, including key moves, "
                    "context, and takeaways in plain language. To enable AI reports, add an API key in your .env file.",
                    class_="ai-desc",
                ),
                ui.tags.div(
                    ui.tags.p(
                        "Ollama Cloud: ", ui.tags.a("ollama.com/settings", href="https://ollama.com/settings", target="_blank"),
                        ". OpenAI: OPENAI_API_KEY=sk-... in .env.", style="margin: 0; font-size: 0.9rem;",
                    ),
                    class_="ai-reporter-connect",
                ),
                class_="ai-reporter-card",
            )

        selected = rt if rt in ch else next(iter(ch))
        parts = [
            ui.tags.div(
                ui.tags.div("Multi-agent AI analysis", class_="ai-title"),
                ui.tags.div(
                    f"Create a user-friendly market brief from the section data you fetched{prov_lab}. The report "
                    "highlights important trends, possible drivers, and what to watch next. Choose a report type, "
                    "then click Generate report. You can view the full RAG retrieval details by scrolling down to "
                    "the RAG retrieval section.",
                    class_="ai-desc",
                ),
                ui.input_select("ai_report_type", "Report type", choices=ch, selected=selected),
                ui.tags.div(
                    ui.tags.p(
                        "RAG: edit ", ui.tags.code("data/rag_market_corpus.txt"), " (chunks separated by ",
                        ui.tags.code("---"), ") to change reference notes. No LLM tool calling.",
                        style="margin: 0; font-size: 0.78rem; color: var(--text-muted);",
                    ),
                    class_="ai-reporter-connect", style="margin-top: 0.5rem;",
                ) if (needs_ai and has_key) else None,
                ui.input_action_button("ai_generate", "Generate report", class_="btn-primary"),
                class_="ai-reporter-card",
            )
        ]
        if loading:
            parts.append(ui.tags.div(ui.tags.p("Running 3-agent pipeline…", style="margin: 0; color: var(--text-muted);"), class_="card-custom pulse"))
        if not loading and report:
            parts.append(ui.tags.div(ui.tags.h4("Final brief"), ui.tags.div(report, class_="ai-report-output"), class_="card-custom", style="margin-top: 1rem;"))
        if not loading and (orch or an or ragtr):
            steps = [ui.tags.p("Agent traces (intermediate outputs)", style="margin: 0 0 0.5rem 0; font-size: 0.8rem; color: var(--text-muted);")]
            if orch:
                steps.append(ui.tags.details(ui.tags.summary("1. Orchestrator — plan & gaps"), _agent_body(orch), class_="agent-step"))
            if an:
                steps.append(ui.tags.details(ui.tags.summary("2. Market Analyst — evidence memo"), _agent_body(an), class_="agent-step"))
            if ragtr:
                steps.append(ui.tags.details(ui.tags.summary("RAG retrieval (local corpus)"), _agent_body(ragtr), class_="agent-step"))
            parts.append(ui.tags.div(*steps, class_="agent-pipeline"))
        return ui.TagList(*parts)

    @reactive.effect
    @reactive.event(input.ai_generate)
    def _():
        report_type = input.ai_report_type() or "brief"
        ai_loading.set(True)
        ai_provider_trace.set(None)
        ai_report_text.set(None)
        ai_orchestrator_text.set(None)
        ai_analyst_text.set(None)
        ai_rag_trace_text.set(None)

        if report_type == "stock_trend":
            ai_loading.set(False)
            ai_report_text.set(build_stock_trend_report(stock_df.get(), input.stock_symbol() or "Stock"))
            return
        if report_type == "forex_trend":
            ai_loading.set(False)
            ai_report_text.set(build_forex_trend_report(fx_df.get(), input.fx_from() or "From", input.fx_to() or "To"))
            return
        if report_type == "commodity_trend":
            ai_loading.set(False)
            ai_report_text.set(build_value_trend_report(commodity_df.get(), f"Commodity ({input.commodity() or ''})", "value"))
            return
        if report_type == "economic_trend":
            ai_loading.set(False)
            ai_report_text.set(build_value_trend_report(economic_df.get(), f"Indicator ({input.economic_indicator() or ''})", "value"))
            return

        cred = pick_llm_credentials()
        if not cred["ok"]:
            ai_loading.set(False)
            return
        context = build_ai_context(report_type)

        with ui.Progress(min=0, max=1) as prog:
            prog.set(0, message="Multi-agent analysis")

            def progress_fn(value, detail):
                prog.set(value, message="Multi-agent analysis", detail=detail)

            try:
                res = run_agentic_pipeline(report_type, input.section() or "", context, cred, progress_fn=progress_fn)
            except Exception as e:
                res = {"ok": False, "error": str(e), "orchestrator": None, "analyst": None, "final": None, "rag_trace": None}

        ai_loading.set(False)
        if not res.get("ok"):
            if res.get("orchestrator"):
                ai_orchestrator_text.set(res["orchestrator"])
            if res.get("analyst"):
                ai_analyst_text.set(res["analyst"])
            rt_err = res.get("rag_trace")
            ai_rag_trace_text.set(rt_err if rt_err else None)
            set_err(res.get("error") or "Analysis failed.")
            return
        if res.get("providers_used"):
            ai_provider_trace.set(" | ".join(res["providers_used"]))
        ai_orchestrator_text.set(res["orchestrator"])
        ai_analyst_text.set(res["analyst"])
        ai_rag_trace_text.set(res.get("rag_trace") or None)
        ai_report_text.set(res["final"])


app = App(app_ui, server)
