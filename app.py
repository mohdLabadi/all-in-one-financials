"""Daily Market Analysis - Alpha Vantage market workstation (Python Shiny).

AI pipeline: agentic orchestration (3 agents) + RAG: local corpus data/rag_market_corpus.txt is
retrieved (keyword overlap) and prepended to the app context for all three LLM calls. No LLM tool calling.

Run: `shiny run app.py --reload` (or `shiny run --launch-browser app.py`) from this folder,
with the project virtualenv (.venv) active and dependencies from requirements.txt installed.
Production: `uvicorn app:app --host 0.0.0.0 --port $PORT` (see render.yaml).
"""

from shiny import App

from market.layout import app_ui
from market.server import server

app = App(app_ui, server)
