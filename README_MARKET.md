# Daily Market App - Technical README

## What This App Does

`Daily Market App` is a Shiny application for quick market monitoring and lightweight analysis.
It combines market data, visual analytics, scenario tools, and optional AI-generated summaries.

Main sections:
- Stocks (daily series and indicators)
- Top gainers/losers
- News sentiment
- Forex
- Commodities
- Economic indicators
- AI Reporter

## Key Features

- Multi-section dashboard with section-specific controls.
- Multiple charts and data tables per section.
- What-if scenario overlays for trend exploration.
- Small calculators (for example: position sizing and conversion checks).
- AI Reporter with a 3-step pipeline:
  1. Orchestrator
  2. Analyst
  3. Editor
- Local RAG context support from `data/rag_market_corpus.txt`.
- Fallback logic for API and model failures.

## Stakeholder Value

- **Retail investors and learners**: quickly review market snapshots, compare scenarios, and read plain-language AI summaries to support learning and decision prep.
- **Analysts and researchers**: speed up first-pass market scans across stocks, forex, commodities, and macro indicators in one interface.
- **Portfolio and risk teams**: use scenario controls and indicator views to identify potential exposure changes and discussion points.
- **Instructors and students**: use the app as a practical example of Shiny + API integration + RAG-based reporting in a single project.
- **Product and operations teams**: rely on fallback behavior (cache and provider failover) to keep workflows usable during API outages or rate limits.

## Design Choices

- **Single main source file**: `app_market.R` keeps app logic in one place for easier grading/review.
- **Deployment compatibility**: root `app.R` is a thin wrapper that loads and returns `app_market`.
- **Simple RAG approach**: local text corpus + lightweight retrieval to avoid extra infrastructure.
- **Provider flexibility**: supports Ollama Cloud and OpenAI credentials.
- **Graceful degradation**: cached data and provider fallback reduce hard failures.

## Project Structure

- `app.R` - deployment entrypoint required by Shiny hosting.
- `app_market.R` - main UI and server logic.
- `run_market.R` - local launcher script.
- `run.R` - container-style launcher (uses `PORT` and host `0.0.0.0`).
- `data/rag_market_corpus.txt` - RAG reference corpus.
- `scripts/` - helper scripts for data/environment workflows.
- `manifest.json` - dependency/environment manifest used by deployment tooling.
- `PROCESS_DIAGRAM.md` - flow/architecture diagram.

## Requirements

- R 4.x
- Packages:

```r
install.packages(c("shiny", "httr2", "httr", "jsonlite", "ggplot2", "DT", "dplyr"))
```

- API keys in `.env` (recommended):

```text
ALPHAVANTAGE_API_KEY=your_key
OLLAMA_CLOUD_API_KEY=your_key   # optional
OPENAI_API_KEY=your_key         # optional
```

## How To Run Locally

### Option A (recommended)

```r
source("run_market.R")
```

### Option B (direct Shiny)

```r
shiny::runApp("app.R")
```

## How To Reproduce Results

1. Clone the repository.
2. Install required R packages.
3. Add `.env` with keys.
4. Launch with `source("run_market.R")`.
5. In each section, click the relevant **Fetch** button before analysis.
6. For AI reports, open **AI Reporter** and click **Generate report**.
7. For RAG behavior, edit `data/rag_market_corpus.txt` and regenerate reports.

## Usage Notes

- Data freshness depends on Alpha Vantage rate limits and endpoint availability.
- AI reports are optional; core charts/tables work without AI keys.
- Trend-style reports may run from fetched data only (without LLM calls).
- This app is educational and not financial advice.

## Fallback Plan

### Market Data Fallback

- Data requests use cache-aware fetch logic.
- If an API request fails or rate limits, the app can reuse cached results.
- UI messages indicate when fallback/cached data is being shown.

### AI Fallback

- AI calls attempt configured providers in sequence.
- If primary provider fails, the next provider is tried automatically.
- If no provider is available, AI panel remains informative and non-blocking.

## Deployment Notes

- Shiny deployment requires `app.R` or `server.R` in the project root.
- This project uses root `app.R` that sources `app_market.R`.
- Deploy from the repository root so `app.R` is detected correctly.
