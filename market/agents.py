"""3-agent pipeline (Orchestrator, Analyst, Editor) and section/report display labels."""

import time

from .llm import llm_chat_messages_with_fallback, truncate_ai_context
from .rag import retrieve_rag_for_report

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
