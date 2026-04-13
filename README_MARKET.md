# Daily Market Analysis - Assignment Documentation

## Instructor Quick Links (for submission .docx)

- **GitHub repository main page**: `<PASTE GITHUB REPO MAIN PAGE LINK>`
- **Live app link (deployed and accessible)**: `<PASTE LIVE APP LINK>`
- **Deployment platform**: Posit Connect / Posit Cloud / DigitalOcean (fill in)
- **Access / password (if applicable)**: Public or `<PASTE PASSWORD>`

---

## Rubric Mapping (App V2 Requirements)

### 1) Functional App (60 pts)

This app provides meaningful market analysis across:

- Stocks (`TIME_SERIES_DAILY`)
- Top movers (`TOP_GAINERS_LOSERS`)
- News sentiment (`NEWS_SENTIMENT`)
- Forex (`FX_DAILY`)
- Commodities (e.g., `WTI`, `BRENT`, `WHEAT`)
- Economic indicators (e.g., `CPI`, `UNEMPLOYMENT`, `TREASURY_YIELD`)

Value-added analysis includes:

- Multi-plot visual analytics per section
- What-if scenario projections on charts
- Interactive calculators (position, FX conversion stress, commodity scenario)
- AI narrative reporting integrated with fetched data

### 2) Agentic Orchestration (25 pts)

Implemented as a **3-agent pipeline** with explicit role/system prompts:

1. `AGENT_SYSTEM_ORCHESTRATOR` - plans themes, identifies gaps, delegates focus.
2. `AGENT_SYSTEM_ANALYST` - writes evidence-based memo from context.
3. `AGENT_SYSTEM_EDITOR` - consolidates output into final user-facing brief.

Workflow integration:

- Function: `run_agentic_pipeline(report_type, section, context, cred, progress_fn)`
- Agent outputs are surfaced in UI (final report + optional trace panels).

### 3) RAG OR Tool Calling (25 pts)

This project satisfies the requirement using **RAG**.

- Custom data source: `data/rag_market_corpus.txt`
- Retrieval function: `retrieve_rag_for_report(report_type, section, context, k = 3L)`
- Supporting functions: `rag_corpus_path()`, `load_rag_chunks()`
- Retrieved chunks are injected into AI context before orchestration.

Note: model-native tool calling is not used; app-side API/tool usage is done in R.

### 4) Effective UI / Visual Design (10 pts)

Implemented UI improvements:

- responsive card-based layout and styled controls
- hover-help tooltips (`?`) on key controls
- legends on color-coded graphs
- multiple charts per section (not single-plot only)
- what-if scenario overlays with user-controlled parameters
- dedicated AI Reporter section and panel

### 5) Deployed App Link (10 pts)

- Add final live link at top under "Instructor Quick Links".
- App must be accessible online at submission time.

### 6) Documentation (30 pts)

This file + `writeup.txt` + `PROCESS_DIAGRAM.md` provide:

- app description and stakeholder value
- process diagram (data flow + orchestration + RAG + fallback)
- technical architecture, implementation, and usage details

---

## 3-5 Paragraph App Description (in own words)

Daily Market Analysis is a deployed Shiny dashboard that extends our App V1 into an AI-assisted decision-support workflow. It brings live market and macro data into one interface using Alpha Vantage, then helps users interpret that data with interactive visualizations and structured AI reporting. The target stakeholders are students, analysts, and other users who need fast, explainable market updates.

The core product flow is: fetch data, inspect visuals, run what-if scenarios, and optionally generate an AI brief. The app supports stocks, gainers/losers, news sentiment, forex, commodities, and economic indicators. Compared to a basic dashboard, it now includes richer interaction through calculators and scenario inputs that directly update projected chart paths.

The key AI enhancement is agentic orchestration. Instead of one prompt, the app runs three agents in sequence: an Orchestrator, a Market Analyst, and a Lead Editor. This separation improves output quality and transparency because planning, analysis, and writing are explicitly staged and auditable.

The app also uses retrieval-augmented generation (RAG). Before AI generation, the system retrieves context from a custom local corpus (`data/rag_market_corpus.txt`) and merges it with live API context. This helps the model stay grounded in domain-specific guidance and reduces unsupported statements.

Finally, the app includes reliability fallback behavior for deployment conditions. If Alpha Vantage rate limits are hit, the app serves cached results for the same query and shows a fallback banner. AI provider fallback is also supported. These resilience features keep the app usable under throttling and improve demo/deployment robustness.

---

## Process Diagram

See `PROCESS_DIAGRAM.md` (root) for the digital diagram.

---

## Technical Documentation

### A) System Architecture

- **Frontend/UI**: `app_market.R` Shiny UI with section-based workflow and AI Reporter panel.
- **Data layer**: Alpha Vantage fetch + normalization functions (`av_*` helpers).
- **AI layer**: 3-agent orchestration pipeline + RAG retrieval context.
- **Resilience layer**:
  - Alpha Vantage cache fallback (`safe_fetch_with_cache`)
  - LLM provider fallback (`llm_chat_messages_with_fallback`)

### B) Agent Roles and Workflow

- `AGENT_SYSTEM_ORCHESTRATOR`: planning + coverage gaps.
- `AGENT_SYSTEM_ANALYST`: evidence-based memo.
- `AGENT_SYSTEM_EDITOR`: concise final output.

Pipeline:

1. Build app context from fetched data.
2. Retrieve RAG chunks (`retrieve_rag_for_report`).
3. Run Orchestrator -> Analyst -> Editor.
4. Show final report and optional traces.

### C) RAG Implementation (required option)

- **Data source**: `data/rag_market_corpus.txt`
- **Chunking**: delimiter `---`
- **Search method**: lightweight keyword overlap ranking
- **Function**: `retrieve_rag_for_report(report_type, section, context, k = 3L)`
- **Return**: retrieved text + retrieval trace for transparency

### D) External APIs, Endpoints, and Keys

#### Alpha Vantage

- Endpoint: `https://www.alphavantage.co/query`
- Key: `ALPHAVANTAGE_API_KEY` (or fallback `API_KEY`)
- Common functions: `TIME_SERIES_DAILY`, `TOP_GAINERS_LOSERS`, `NEWS_SENTIMENT`, `FX_DAILY`, commodity/economic function codes.

#### LLM providers (AI Reporter)

- Ollama Cloud: `https://ollama.com/api/chat` with `OLLAMA_CLOUD_API_KEY`
- OpenAI fallback: `https://api.openai.com/v1/chat/completions` with `OPENAI_API_KEY`

### E) Fallback / Rate-Limit Plan

#### Alpha Vantage fallback

- Function: `safe_fetch_with_cache(cache_key, fetch_fn)`
- Strategy:
  - On success: store result in cache.
  - On rate-limit error: return cached result for same key if available.
  - UI displays fallback banner so users know data is cached.

#### AI provider fallback

- Function: `llm_chat_messages_with_fallback(messages, creds)`
- Strategy:
  - Try primary provider first.
  - If call fails, try next configured provider automatically.

### F) Packages and Runtime

Required packages:

```r
install.packages(c("shiny","httr2","httr","jsonlite","ggplot2","DT","dplyr"))
```

### G) File Structure

- `app_market.R` - main app and core logic
- `run_market.R` - launcher
- `data/rag_market_corpus.txt` - RAG corpus
- `PROCESS_DIAGRAM.md` - process diagram
- `writeup.txt` - submission narrative

### H) Usage Instructions

1. Add keys to `.env`:

```text
ALPHAVANTAGE_API_KEY=...
OLLAMA_CLOUD_API_KEY=...   # optional
OPENAI_API_KEY=...         # optional
```

2. Run:

```r
source("run_market.R")
```

3. In app:

- Select a section and click **Fetch**.
- Use interactive controls (tooltips available via `?`).
- Use calculators and scenario inputs for what-if analysis.
- Open AI Reporter and click **Generate report** for multi-agent analysis.

---

## Team Members by Role (fill in)

- Project Manager:
- Backend Developer:
- Frontend Developer:
- Prompt Engineer:
- Agent Orchestration Engineer:
- Database/RAG Engineer:
- Data Engineer:
- DevOps Engineer:
- Full Stack Developer:
- QA Engineer:

---

## Pre-Submission Checklist

- [ ] Live app link pasted above and works anonymously.
- [ ] GitHub repo link pasted above.
- [ ] `PROCESS_DIAGRAM.md` included in repo root.
- [ ] `writeup.txt` updated and aligned with this doc.
- [ ] AI orchestration demonstrated in deployed app.
- [ ] RAG demonstrated in deployed app.
- [ ] App remains accessible at submission time.

