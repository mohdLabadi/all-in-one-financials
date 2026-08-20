"""Small reusable UI building blocks and dataframe/column helpers."""

import re

import pandas as pd
from shiny import render, ui

CHART_ICON_SVG = (
    "<svg width='22' height='22' viewBox='0 0 24 24' fill='none' stroke='currentColor' "
    "stroke-width='2' stroke-linecap='round' stroke-linejoin='round'>"
    "<polyline points='3 17 9 11 13 15 21 6'/><polyline points='14 6 21 6 21 13'/></svg>"
)


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
