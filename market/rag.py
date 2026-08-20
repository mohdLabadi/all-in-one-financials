"""RAG: retrieve reference chunks from the local corpus (data/rag_market_corpus.txt)."""

import re
from pathlib import Path

from .config import APP_DIR

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
