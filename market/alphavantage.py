"""Alpha Vantage API client: fetch + normalize market data, with rate-limit-aware caching."""

import re

import pandas as pd
import requests

from .config import API_KEY

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


