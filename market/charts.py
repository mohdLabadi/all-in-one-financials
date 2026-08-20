"""Plotly chart theme: shared color palette and layout styling."""

import plotly.graph_objects as go

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
