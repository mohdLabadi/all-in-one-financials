"""Environment configuration: API keys and the project root path."""

import os
from pathlib import Path

from dotenv import load_dotenv

APP_DIR = Path(__file__).resolve().parent.parent
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
