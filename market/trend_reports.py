"""Computed (non-AI) trend reports: simple day/week/month/year change summaries."""

import numpy as np
import pandas as pd

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
