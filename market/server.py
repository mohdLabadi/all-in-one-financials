"""Reactive server logic: data fetching, filtering, charts, calculators, and the AI reporter."""

import re
from datetime import date

import numpy as np
import pandas as pd
import plotly.graph_objects as go
from shiny import reactive, render, req, ui
from shinywidgets import output_widget, render_widget

from .agents import run_agentic_pipeline, section_display_name
from .alphavantage import (
    av_commodity,
    av_economic,
    av_fx_daily,
    av_news_sentiment,
    av_stock_daily,
    av_top_gainers_losers,
    safe_fetch_with_cache,
)
from .charts import CHART_COLORS, market_layout
from .config import API_KEY
from .layout import SECTION_CHOICES
from .llm import pick_llm_credentials
from .trend_reports import build_forex_trend_report, build_stock_trend_report, build_value_trend_report
from .ui_helpers import CHART_ICON_SVG, _agent_body, _find_col, _stat, _table_card, qmark

def server(input, output, session):
    err_msg = reactive.value(None)
    fallback_msg = reactive.value(None)
    stock_df = reactive.value(None)
    gainers_data = reactive.value(None)
    news_df = reactive.value(None)
    fx_df = reactive.value(None)
    commodity_df = reactive.value(None)
    economic_df = reactive.value(None)
    ai_report_text = reactive.value(None)
    ai_orchestrator_text = reactive.value(None)
    ai_analyst_text = reactive.value(None)
    ai_rag_trace_text = reactive.value(None)
    ai_provider_trace = reactive.value(None)
    ai_loading = reactive.value(False)

    def clear_err():
        err_msg.set(None)

    def clear_fallback():
        fallback_msg.set(None)

    def set_err(e):
        err_msg.set(f"Error: {e}")

    def _settle_after(source_getter):
        """A reactive.value that bumps one flush cycle after source_getter's value changes.

        Two brand-new Plotly widgets that both receive their first real value in the same
        reactive flush can race on the client (shinywidgets/anywidget) and one renders blank
        when its section was hidden at page load. Staggering the secondary chart's compute to
        the next flush avoids that simultaneous first-mount race.
        """
        tick = reactive.value(0)

        @reactive.effect
        def _():
            source_getter()

            def bump():
                with reactive.isolate():
                    tick.set(tick.get() + 1)

            session.on_flushed(bump, once=True)

        return tick

    stock_settle = _settle_after(stock_df.get)
    fx_settle = _settle_after(fx_df.get)
    commodity_settle = _settle_after(commodity_df.get)

    # ---- Header / hero ----

    @render.ui
    def section_title():
        return ui.tags.h3(SECTION_CHOICES.get(input.section(), input.section()))

    @render.ui
    def ui_top_status():
        api_ok = bool(API_KEY)
        cred = pick_llm_credentials()
        ai_ok = cred["ok"]
        return ui.TagList(
            ui.tags.span(
                ui.tags.span(class_="dot"),
                "Alpha Vantage connected" if api_ok else "API key missing",
                class_=f"nav-pill {'ok' if api_ok else 'warn'}",
            ),
            ui.tags.span(
                ui.tags.span(class_="dot"),
                "AI reporter ready" if ai_ok else "AI not configured",
                class_=f"nav-pill {'ok' if ai_ok else 'warn'}",
            ),
        )

    @render.ui
    def ui_section_help():
        sec = input.section() or "stock"
        msg = {
            "stock": "Enter a symbol (example: AAPL), click Fetch, then use the toolbar to adjust the window.",
            "gainers": "Click Fetch movers to load gainers, losers, and active symbols. Use filter/chart toggles to narrow focus.",
            "news": "Optionally provide tickers (example: AAPL,MSFT), then filter stories by keyword or sentiment.",
            "forex": "Set From/To currencies (example: EUR/USD), fetch data, then inspect the trend and summary cards.",
            "commodity": "Choose a commodity and interval, fetch, then review latest level and range in the chart card.",
            "economic": "Choose an indicator and interval, fetch, then compare current value against period min/max.",
            "ai": "Use the AI Reporter panel to generate a 3-agent analysis from your currently fetched data.",
        }.get(sec, "Fetch data in this section first, then run AI analysis if needed.")
        return ui.tags.p(msg, class_="hero-subtitle")

    @render.ui
    def ui_content_empty_state():
        sec = input.section() or "stock"
        if sec == "ai":
            return None
        has_data = {
            "stock": stock_df.get() is not None,
            "gainers": gainers_data.get() is not None,
            "news": news_df.get() is not None,
            "forex": fx_df.get() is not None,
            "commodity": commodity_df.get() is not None,
            "economic": economic_df.get() is not None,
        }.get(sec, True)
        if has_data:
            return None
        cta = {
            "stock": "Fetch", "gainers": "Fetch movers", "news": "Fetch news",
            "forex": "Fetch", "commodity": "Fetch", "economic": "Fetch",
        }.get(sec, "Fetch")
        return ui.tags.div(
            ui.tags.div(ui.HTML(CHART_ICON_SVG), class_="icon"),
            ui.tags.div(f"No {section_display_name(sec).lower()} data loaded yet", class_="title"),
            ui.tags.p(
                "Set your parameters above and click ", ui.tags.strong(cta),
                " to pull live data from Alpha Vantage. Charts, summary metrics, and calculators will appear here.",
                class_="desc",
            ),
            class_="empty-state-hero",
        )

    @render.ui
    def api_error_ui():
        err = err_msg.get()
        if not err:
            return None
        return ui.tags.div(
            ui.tags.p(err, style="margin: 0; color: #991b1b; font-weight: 600;"),
            class_="card-custom", style="border-left: 4px solid #dc2626; background: #fef2f2;",
        )

    @render.ui
    def api_fallback_ui():
        msg = fallback_msg.get()
        if not msg:
            return None
        return ui.tags.div(
            ui.tags.p(msg, style="margin: 0; color: #92400e; font-weight: 600;"),
            class_="card-custom", style="border-left: 4px solid #f59e0b; background: #fffbeb;",
        )

    # ---- Stock Daily ----

    @reactive.effect
    @reactive.event(input.fetch_stock)
    def _():
        clear_err()
        clear_fallback()
        try:
            sym = (input.stock_symbol() or "").strip()
            if not sym:
                set_err("Please enter a symbol.")
                return
            outsize = "full" if input.stock_full_history() else "compact"
            rk = f"stock|{sym.upper()}|{outsize}"
            res = safe_fetch_with_cache(rk, lambda: av_stock_daily(sym, outputsize=outsize))
            if not res["ok"]:
                set_err(res.get("error") or "Fetch failed.")
                return
            df = res["data"]
            if df is None or df.empty:
                set_err("No data returned. Check API key, symbol, or rate limit (5/min, 25/day).")
                stock_df.set(None)
            else:
                stock_df.set(df)
                if res.get("fallback"):
                    fallback_msg.set(res.get("msg") or "Using cached data.")
        except Exception as e:
            set_err(str(e))

    @render.ui
    def ui_stock_toolbar():
        df = stock_df.get()
        if df is None or df.empty:
            return None
        return ui.tags.div(
            ui.tags.div(
                ui.input_select("stock_days", "View", choices={"5": "Last 5 days", "21": "Last 21 days", "60": "Last 60 days", "90": "Last 90 days"}, selected="90"),
                class_="control-field",
            ),
            ui.tags.div(ui.input_checkbox("stock_show_ma", qmark("MA(20/50)", "Show 20-day and 50-day moving averages."), value=True), class_="form-check"),
            ui.tags.div(ui.input_checkbox("stock_show_bands", qmark("Bollinger", "Show Bollinger upper/lower volatility bands."), value=False), class_="form-check"),
            ui.tags.div(ui.input_numeric("stock_calc_invest", qmark("Invest $", "How much capital to simulate in the position calculator."), value=10000, min=0, step=100), class_="control-field"),
            ui.tags.div(ui.input_numeric("stock_calc_target", qmark("Target %", "Desired percent move used for projected P/L."), value=5, min=-100, max=500, step=0.5), class_="control-field"),
            ui.tags.div(ui.input_numeric("stock_scn_days", qmark("Scenario days", "Number of future days to project on the chart."), value=20, min=1, max=252, step=1), class_="control-field"),
            ui.tags.div(ui.input_numeric("stock_scn_drift", qmark("Scenario daily %", "Assumed daily percent change for scenario path."), value=0.2, min=-10, max=10, step=0.1), class_="control-field"),
            ui.download_button("download_stock_csv", "Download CSV", class_="btn-default"),
            class_="control-bar",
        )

    @render.ui
    def ui_stock_summary():
        df = stock_df.get()
        if df is None or df.empty:
            return None
        days = int(input.stock_days() or 90)
        d = df.head(days)
        r = d.iloc[0]
        prev = float(d["close"].iloc[1]) if len(d) >= 2 else float(r["close"])
        chg = round(100 * (r["close"] - prev) / prev, 2) if prev else None
        chg5 = round(100 * (r["close"] - d["close"].iloc[5]) / d["close"].iloc[5], 2) if len(d) >= 6 and d["close"].iloc[5] else None
        chg21 = round(100 * (r["close"] - d["close"].iloc[21]) / d["close"].iloc[21], 2) if len(d) >= 22 and d["close"].iloc[21] else None
        lret = np.diff(np.log(d["close"].astype(float)))
        lret = lret[np.isfinite(lret)]
        vol_ann = round(np.std(lret) * np.sqrt(252) * 100, 1) if len(lret) >= 5 else None

        def val_class(v):
            return "value" if v is None else ("value positive" if v >= 0 else "value negative")

        def txt(v):
            return "—" if v is None else f"{'+' if v >= 0 else ''}{v}%"

        regime = "Regime unavailable (need 50+ days)"
        if len(d) >= 50:
            ma20 = d["close"].head(20).mean()
            ma50 = d["close"].head(50).mean()
            if np.isfinite(ma20) and np.isfinite(ma50):
                regime = "Bullish trend regime" if ma20 >= ma50 else "Bearish trend regime"
            else:
                regime = "Regime unavailable"

        return ui.TagList(
            ui.tags.div(
                _stat("Last close", f"${r['close']:.2f}"),
                _stat("Change (1d)", txt(chg), val_class(chg)),
                _stat("Change (1w)", txt(chg5), val_class(chg5)),
                _stat("Change (1m)", txt(chg21), val_class(chg21)),
                _stat("High", f"${r.get('high', float('nan')):.2f}"),
                _stat("Low", f"${r.get('low', float('nan')):.2f}"),
                _stat("Ann. volatility", f"{vol_ann}%" if vol_ann is not None else "—"),
                _stat("Volume", f"{r.get('volume', 0):,.0f}"),
                class_="stats-row",
            ),
            ui.tags.div(
                ui.tags.p(
                    f"Signal: {regime}. This combines moving-average structure with recent returns to give quick "
                    "context, not investment advice.",
                    style="margin: 0; color: var(--text-muted); font-size: 0.82rem;",
                ),
                class_="card-custom",
            ),
        )

    @render_widget
    def plot_stock():
        df = stock_df.get()
        req(df is not None and not df.empty)
        days = int(input.stock_days() or 90)
        d = df.head(days).dropna(subset=["date", "close"]).sort_values("date").copy()
        req(not d.empty)
        d["ma20"] = d["close"].rolling(20).mean()
        d["ma50"] = d["close"].rolling(50).mean()
        sd20 = d["close"].rolling(20).std()
        d["bb_up"] = d["ma20"] + 2 * sd20
        d["bb_dn"] = d["ma20"] - 2 * sd20

        fig = go.Figure()
        fig.add_trace(go.Scatter(x=d["date"], y=d["close"], mode="lines", name="Close", line=dict(color=CHART_COLORS["close"], width=2.4)))
        if input.stock_show_ma():
            fig.add_trace(go.Scatter(x=d["date"], y=d["ma20"], mode="lines", name="MA20", line=dict(color=CHART_COLORS["ma20"], width=1.8)))
            fig.add_trace(go.Scatter(x=d["date"], y=d["ma50"], mode="lines", name="MA50", line=dict(color=CHART_COLORS["ma50"], width=1.8)))
        if input.stock_show_bands():
            fig.add_trace(go.Scatter(x=d["date"], y=d["bb_up"], mode="lines", name="Bollinger Upper", line=dict(color=CHART_COLORS["boll"], width=1.4, dash="dash")))
            fig.add_trace(go.Scatter(x=d["date"], y=d["bb_dn"], mode="lines", name="Bollinger Lower", line=dict(color=CHART_COLORS["boll2"], width=1.4, dash="dash")))

        scn_days = int(input.stock_scn_days() or 20)
        scn_drift = float(input.stock_scn_drift() or 0)
        subtitle = None
        if scn_days > 0:
            last_dt = d["date"].max()
            last_px = float(d["close"].iloc[-1])
            if np.isfinite(last_px) and last_px > 0:
                fut_dates = pd.date_range(last_dt + pd.Timedelta(days=1), periods=scn_days, freq="D")
                fut_px = last_px * np.cumprod(np.repeat(1 + scn_drift / 100, scn_days))
                fig.add_trace(go.Scatter(x=fut_dates, y=fut_px, mode="lines+markers", name="Scenario", line=dict(color=CHART_COLORS["scenario"], width=2, dash="dashdot")))
                subtitle = "Orange line: user-defined scenario path"
        market_layout(fig, y_title="Close", x_title="Date", subtitle=subtitle)
        return fig

    @render_widget
    def plot_stock_returns():
        stock_settle.get()
        with reactive.isolate():
            df = stock_df.get()
        req(df is not None and len(df) >= 3)
        days = int(input.stock_days() or 90)
        d = df.head(days).sort_values("date").copy()
        d["ret"] = 100 * (d["close"] / d["close"].shift(1) - 1)
        d = d.dropna(subset=["ret"])
        req(len(d) >= 2)
        colors = [CHART_COLORS["up"] if r >= 0 else CHART_COLORS["down"] for r in d["ret"]]
        fig = go.Figure(go.Bar(x=d["date"], y=d["ret"], marker_color=colors, name="Daily return"))
        fig.add_hline(y=0, line_color="#94a3b8", line_width=1)
        market_layout(fig, y_title="Daily return (%)", x_title="Date", show_legend=False, subtitle="Return bars (up/down days)")
        return fig

    @render.ui
    def ui_stock_calculator():
        df = stock_df.get()
        if df is None or df.empty:
            return None
        px = float(df["close"].iloc[0])
        if not np.isfinite(px) or px <= 0:
            return None
        invest = float(input.stock_calc_invest() or 0)
        tgt = float(input.stock_calc_target() or 0)
        shares = invest / px if invest > 0 else 0
        tgt_px = px * (1 + tgt / 100)
        pnl = shares * (tgt_px - px)
        return ui.tags.div(
            ui.tags.h4("Position calculator"),
            ui.tags.div(
                _stat("Implied shares", f"{shares:.2f}"),
                _stat("Current price", f"${px:.2f}"),
                _stat("Target price", f"${tgt_px:.2f}"),
                _stat("Projected P/L", f"${pnl:.2f}", "value positive" if pnl >= 0 else "value negative"),
                class_="stats-row",
            ),
            ui.tags.p("Educational what-if only; ignores fees, slippage, and taxes.", style="margin: 0; color: var(--text-muted); font-size: 0.8rem;"),
            class_="card-custom",
        )

    @render.data_frame
    def table_stock():
        return _table_card(stock_df.get())

    @render.download_button(filename=lambda: f"stock_{(input.stock_symbol() or 'symbol')}_{date.today()}.csv")
    def download_stock_csv():
        df = stock_df.get()
        if df is not None and not df.empty:
            yield df.to_csv(index=False)

    # ---- Top Gainers / Losers ----

    @reactive.effect
    @reactive.event(input.fetch_gainers)
    def _():
        clear_err()
        clear_fallback()
        try:
            res = safe_fetch_with_cache("gainers|default", av_top_gainers_losers)
            if not res["ok"]:
                set_err(res.get("error") or "Fetch failed.")
                return
            d = res["data"]
            ng = len(d["top_gainers"]) if d.get("top_gainers") is not None else 0
            if ng == 0:
                set_err("No data returned. Check API key or rate limit (5/min, 25/day).")
                gainers_data.set(None)
            else:
                gainers_data.set(d)
                if res.get("fallback"):
                    fallback_msg.set(res.get("msg") or "Using cached data.")
        except Exception as e:
            set_err(str(e))

    @render.ui
    def ui_gainers_toolbar():
        d = gainers_data.get()
        if d is None:
            return None
        return ui.tags.div(
            ui.tags.div(ui.input_select("gainers_n", "Show", choices=["5", "10", "20"], selected="20"), class_="control-field"),
            ui.tags.div(ui.input_text("gainers_filter", "Filter", placeholder="e.g. AAPL"), class_="control-field"),
            ui.tags.div(ui.input_checkbox("gainers_chart", "Bar chart", value=False), class_="form-check"),
            ui.download_button("download_gainers_csv", "Download CSV", class_="btn-default"),
            class_="control-bar",
        )

    @reactive.calc
    def gainers_filtered():
        d = gainers_data.get()
        if d is None:
            return None
        flt = (input.gainers_filter() or "").strip()
        n_show = int(input.gainers_n() or 20)

        def filter_df(df):
            if df is None or df.empty:
                return df
            if not flt:
                return df.head(n_show)
            tk = _find_col(df, "ticker")
            mask = df[tk].astype(str).str.contains(re.escape(flt), case=False, na=False)
            return df[mask].head(n_show)

        return {
            "top_gainers": filter_df(d.get("top_gainers")),
            "top_losers": filter_df(d.get("top_losers")),
            "top_activated": filter_df(d.get("top_activated")),
        }

    def _ticker_cards(df: pd.DataFrame | None, name: str):
        if df is None or df.empty:
            return None
        tk = _find_col(df, "ticker", default_idx=0)
        pr = _find_col(df, "price", default_idx=min(1, len(df.columns) - 1))
        ch = _find_col(df, "changepercentage", "change_percentage", "changepercent")
        vol = _find_col(df, "volume")
        items = []
        for i, (_, row) in enumerate(df.iterrows(), start=1):
            tkr = str(row[tk]) if tk else "—"
            try:
                prc = f"{float(row[pr]):.2f}"
            except Exception:
                prc = str(row[pr]) if pr else "—"
            chg = str(row[ch]) if ch else ""
            vol_txt = None
            if vol:
                try:
                    vol_txt = f"{float(row[vol]):,.0f}"
                except Exception:
                    vol_txt = None
            is_neg = "-" in chg
            cl = "change-neg" if (name == "Top Losers" or is_neg) else "change-pos"
            items.append(
                ui.tags.div(
                    ui.tags.div(f"#{i}", style="font-size: 0.75rem; font-weight: 700; color: var(--text-muted);"),
                    ui.tags.div(tkr, class_="ticker"),
                    ui.tags.div(prc, style="font-size: 0.9rem; color: var(--text-muted);"),
                    ui.tags.div(chg, class_=cl) if chg else None,
                    ui.tags.div(f"Vol: {vol_txt}", style="font-size: 0.75rem; color: var(--text-muted);") if vol_txt else None,
                    class_="ticker-card",
                )
            )
        return ui.TagList(ui.tags.h4(name), ui.tags.div(*items, style="display: flex; flex-wrap: wrap; gap: 0.35rem;"))

    @render.ui
    def ui_gainers_cards():
        d = gainers_filtered()
        if d is None:
            return ui.tags.p("Click 'Fetch movers' to load data.", style="color: var(--text-muted);")
        return ui.TagList(
            ui.tags.div(_ticker_cards(d.get("top_gainers"), "Top Gainers"), class_="card-custom"),
            ui.tags.div(_ticker_cards(d.get("top_losers"), "Top Losers"), class_="card-custom"),
            ui.tags.div(_ticker_cards(d.get("top_activated"), "Most Actively Traded"), class_="card-custom"),
        )

    @render.ui
    def ui_gainers_chart():
        if not input.gainers_chart():
            return None
        d = gainers_filtered()
        tg = d.get("top_gainers") if d else None
        if tg is None or tg.empty:
            return None
        return ui.tags.div(output_widget("plot_gainers_bars", height="420px"), class_="plot-container")

    @render_widget
    def plot_gainers_bars():
        req(input.gainers_chart())
        d = gainers_filtered()
        df = d.get("top_gainers") if d else None
        req(df is not None and not df.empty)
        tk = _find_col(df, "ticker")
        ch = _find_col(df, "changepercentage", "change_percentage", "changepercent")
        req(ch is not None)
        df = df.copy()
        df["ticker_"] = df[tk]
        df["change_"] = pd.to_numeric(df[ch].astype(str).str.replace(r"[^0-9.\-]", "", regex=True), errors="coerce")
        df = df.dropna(subset=["change_"]).head(15).sort_values("change_")
        req(not df.empty)
        colors = [CHART_COLORS["up"] if c > 0 else CHART_COLORS["down"] for c in df["change_"]]
        fig = go.Figure(go.Bar(x=df["change_"], y=df["ticker_"], orientation="h", marker_color=colors))
        market_layout(fig, x_title="Change %", show_legend=False, subtitle="Top gainers")
        return fig

    @render.data_frame
    def table_gainers():
        d = gainers_data.get()
        return _table_card(d.get("top_gainers") if d else None)

    @render.data_frame
    def table_losers():
        d = gainers_data.get()
        return _table_card(d.get("top_losers") if d else None)

    @render.data_frame
    def table_active():
        d = gainers_data.get()
        return _table_card(d.get("top_activated") if d else None)

    @render.download_button(filename=lambda: f"gainers_losers_{date.today()}.csv")
    def download_gainers_csv():
        d = gainers_data.get()
        if d is not None:
            frames = []
            for key, label in (("top_gainers", "gainers"), ("top_losers", "losers"), ("top_activated", "active")):
                df = d.get(key)
                if df is not None and not df.empty:
                    df = df.copy()
                    df["group"] = label
                    frames.append(df)
            if frames:
                yield pd.concat(frames, ignore_index=True).to_csv(index=False)

    # ---- News ----

    @reactive.effect
    @reactive.event(input.fetch_news)
    def _():
        clear_err()
        clear_fallback()
        try:
            tk = (input.news_tickers() or "").strip()
            lim = int(input.news_limit() or 20)
            rk = f"news|{tk.lower()}|{lim}"
            res = safe_fetch_with_cache(rk, lambda: av_news_sentiment(tickers=tk, limit=lim))
            if not res["ok"]:
                set_err(res.get("error") or "Fetch failed.")
                return
            df = res["data"]
            if df is None or df.empty:
                set_err("No news returned. Check API key or rate limit (5/min, 25/day).")
                news_df.set(None)
            else:
                news_df.set(df)
                if res.get("fallback"):
                    fallback_msg.set(res.get("msg") or "Using cached data.")
        except Exception as e:
            set_err(str(e))

    @render.ui
    def ui_news_toolbar():
        df = news_df.get()
        if df is None or df.empty:
            return None
        return ui.tags.div(
            ui.tags.div(ui.input_select("news_n", qmark("Show", "Maximum number of headlines to display."), choices=["10", "25", "50"], selected="50"), class_="control-field"),
            ui.tags.div(ui.input_text("news_keyword", qmark("Keyword", "Filter headlines containing this text."), placeholder="e.g. Fed"), class_="control-field"),
            ui.tags.div(ui.input_select("news_sentiment_filter", qmark("Sentiment", "Filter by bullish, bearish, or neutral labels."), choices=["All", "Bullish", "Bearish", "Neutral"], selected="All"), class_="control-field"),
            ui.tags.div(ui.input_checkbox("news_show_dist", qmark("Show sentiment mix", "Toggle sentiment distribution chart."), value=True), class_="form-check"),
            ui.download_button("download_news_csv", "Download CSV", class_="btn-default"),
            class_="control-bar",
        )

    @reactive.calc
    def news_filtered():
        df = news_df.get()
        if df is None or df.empty:
            return df
        kw = (input.news_keyword() or "").strip()
        sent = input.news_sentiment_filter() or "All"
        n_show = int(input.news_n() or 50)
        out = df
        if kw:
            mask = out["title"].astype(str).str.contains(re.escape(kw), case=False, na=False) | out["summary"].astype(str).str.contains(re.escape(kw), case=False, na=False)
            out = out[mask]
        if sent != "All":
            lbl = out["overall_sentiment_label"].astype(str).str.lower()
            if sent == "Bullish":
                idx = lbl.str.contains("bullish|positive", regex=True)
            elif sent == "Bearish":
                idx = lbl.str.contains("bearish|negative", regex=True)
            else:
                idx = lbl.str.contains("neutral", regex=True)
            out = out[idx]
        return out.head(n_show)

    def _sentiment_class(lbl) -> str:
        if not lbl:
            return "sentiment-neutral"
        l = str(lbl).lower()
        if "bullish" in l or "positive" in l:
            return "sentiment-bullish"
        if "bearish" in l or "negative" in l:
            return "sentiment-bearish"
        return "sentiment-neutral"

    @render.ui
    def ui_news_cards():
        df = news_filtered()
        if df is None or df.empty:
            return ui.tags.p("Click 'Fetch news' or loosen filters.", style="color: var(--text-muted);")
        cards = []
        for _, r in df.iterrows():
            title = str(r.get("title") or "—")
            url = str(r.get("url") or "")
            src = str(r.get("source") or "")
            t = str(r.get("time_published") or "")
            if len(t) >= 8:
                t = t[:10]
            sent = str(r.get("overall_sentiment_label") or "")
            title_el = (
                ui.tags.a(title, href=url, target="_blank", rel="noopener")
                if url else ui.tags.p(title, style="margin: 0 0 0.5rem 0; font-weight: 600;")
            )
            meta = ui.tags.div(
                ui.tags.span(f" {src}") if src else None,
                ui.tags.span(f" · {t}") if t else None,
                ui.tags.span(sent, class_=f"sentiment-badge {_sentiment_class(sent)}", style="margin-left: 0.5rem;") if sent else None,
                style="font-size: 0.85rem; color: var(--text-muted);",
            )
            cards.append(ui.tags.div(title_el, meta, class_="news-card"))
        return ui.TagList(*cards)

    @render_widget
    def plot_news_sentiment():
        req(input.news_show_dist())
        df = news_filtered()
        req(df is not None and not df.empty and "overall_sentiment_label" in df.columns)
        lbl = df["overall_sentiment_label"].astype(str).str.lower()
        cls = np.where(
            lbl.str.contains("bullish|positive", regex=True), "Bullish",
            np.where(lbl.str.contains("bearish|negative", regex=True), "Bearish", "Neutral"),
        )
        counts = pd.Series(cls).value_counts().reindex(["Bullish", "Neutral", "Bearish"]).fillna(0)
        req(counts.sum() > 0)
        colors = {"Bullish": CHART_COLORS["up"], "Neutral": "#64748b", "Bearish": CHART_COLORS["down"]}
        fig = go.Figure(
            go.Bar(x=counts.index, y=counts.values, marker_color=[colors[k] for k in counts.index], text=counts.values.astype(int), textposition="outside")
        )
        market_layout(fig, y_title="Headline count", show_legend=False, subtitle="Sentiment distribution (filtered set)")
        return fig

    @render.data_frame
    def table_news():
        df = news_df.get()
        if df is not None and not df.empty:
            cols = [c for c in ["title", "time_published", "overall_sentiment_label", "source", "url"] if c in df.columns]
            if cols:
                df = df[cols]
        return _table_card(df)

    @render.download_button(filename=lambda: f"news_{date.today()}.csv")
    def download_news_csv():
        df = news_df.get()
        if df is not None and not df.empty:
            yield df.to_csv(index=False)

    # ---- Forex ----

    @reactive.effect
    @reactive.event(input.fetch_fx)
    def _():
        clear_err()
        clear_fallback()
        try:
            frm = (input.fx_from() or "").strip()
            to = (input.fx_to() or "").strip()
            if not frm or not to:
                set_err("Enter From and To currencies.")
                return
            rk = f"fx|{frm.upper()}|{to.upper()}"
            res = safe_fetch_with_cache(rk, lambda: av_fx_daily(from_symbol=frm, to_symbol=to))
            if not res["ok"]:
                set_err(res.get("error") or "Fetch failed.")
                return
            df = res["data"]
            if df is None or df.empty:
                set_err("No FX data returned. Check API key or rate limit (5/min, 25/day).")
                fx_df.set(None)
            else:
                fx_df.set(df)
                if res.get("fallback"):
                    fallback_msg.set(res.get("msg") or "Using cached data.")
        except Exception as e:
            set_err(str(e))

    @render.ui
    def ui_fx_toolbar():
        df = fx_df.get()
        if df is None or df.empty:
            return None
        return ui.tags.div(
            ui.tags.div(ui.input_select("fx_points", qmark("Chart points", "How many historical points to display in charts."), choices=["30", "60", "90"], selected="90"), class_="control-field"),
            ui.tags.div(ui.input_numeric("fx_calc_amount", qmark("Amount", "Base currency amount for conversion calculator."), value=1000, min=0, step=100), class_="control-field"),
            ui.tags.div(ui.input_numeric("fx_calc_shift", qmark("Rate shock %", "One-time percentage shift used in stress calculation."), value=1, min=-50, max=50, step=0.1), class_="control-field"),
            ui.tags.div(ui.input_numeric("fx_scn_days", qmark("Scenario days", "Number of future periods in scenario projection."), value=15, min=1, max=180, step=1), class_="control-field"),
            ui.tags.div(ui.input_numeric("fx_scn_drift", qmark("Scenario daily %", "Assumed daily % move for projected path."), value=0.05, min=-5, max=5, step=0.01), class_="control-field"),
            ui.download_button("download_fx_csv", "Download CSV", class_="btn-default"),
            class_="control-bar",
        )

    @render.ui
    def ui_fx_summary():
        df = fx_df.get()
        if df is None or df.empty:
            return None
        rate = float(df["close"].iloc[0])
        if not np.isfinite(rate):
            return None
        return ui.tags.div(
            _stat("Latest rate", f"{rate:.4f}", "hero-rate"),
            _stat("Low (period)", f"{df['close'].min():.4f}"),
            _stat("High (period)", f"{df['close'].max():.4f}"),
            _stat("Days shown", f"{len(df)}"),
            class_="stats-row",
        )

    @render_widget
    def plot_fx():
        df = fx_df.get()
        req(df is not None and not df.empty)
        pts = int(input.fx_points() or 90)
        d = df.head(pts).dropna(subset=["date", "close"]).sort_values("date")
        req(not d.empty)
        fig = go.Figure(go.Scatter(x=d["date"], y=d["close"], mode="lines", name="Observed", line=dict(color=CHART_COLORS["close"], width=2.4)))
        scn_days = int(input.fx_scn_days() or 15)
        scn_drift = float(input.fx_scn_drift() or 0)
        subtitle = None
        if scn_days > 0:
            last_dt = d["date"].max()
            last_px = float(d["close"].iloc[-1])
            if np.isfinite(last_px) and last_px > 0:
                fut_dates = pd.date_range(last_dt + pd.Timedelta(days=1), periods=scn_days, freq="D")
                fut_px = last_px * np.cumprod(np.repeat(1 + scn_drift / 100, scn_days))
                fig.add_trace(go.Scatter(x=fut_dates, y=fut_px, mode="lines+markers", name="Scenario", line=dict(color=CHART_COLORS["scenario_fx"], width=2, dash="dashdot")))
                subtitle = "Orange path: user-defined FX scenario"
        market_layout(fig, y_title="Close", x_title="Date", subtitle=subtitle)
        return fig

    @render_widget
    def plot_fx_returns():
        fx_settle.get()
        with reactive.isolate():
            df = fx_df.get()
        req(df is not None and len(df) >= 3)
        pts = int(input.fx_points() or 90)
        d = df.head(pts).sort_values("date").copy()
        d["ret"] = 100 * (d["close"] / d["close"].shift(1) - 1)
        d = d.dropna(subset=["ret"])
        req(len(d) >= 2)
        colors = [CHART_COLORS["up"] if r >= 0 else CHART_COLORS["scenario_fx"] for r in d["ret"]]
        fig = go.Figure()
        fig.add_trace(go.Bar(x=d["date"], y=d["ret"], marker_color=colors, name="Return"))
        window = max(2, len(d) // 8)
        trend = d["ret"].rolling(window, min_periods=1, center=True).mean()
        fig.add_trace(go.Scatter(x=d["date"], y=trend, mode="lines", name="Trend", line=dict(color=CHART_COLORS["trend"], width=2)))
        market_layout(fig, y_title="Return (%)", x_title="Date", subtitle="FX return momentum with smooth trend")
        return fig

    @render.ui
    def ui_fx_calculator():
        df = fx_df.get()
        if df is None or df.empty:
            return None
        rate = float(df["close"].iloc[0])
        if not np.isfinite(rate) or rate <= 0:
            return None
        amt = float(input.fx_calc_amount() or 0)
        shock = float(input.fx_calc_shift() or 0)
        converted = amt * rate
        shocked_rate = rate * (1 + shock / 100)
        shocked_value = amt * shocked_rate
        delta = shocked_value - converted
        pair = f"{(input.fx_from() or 'FROM').upper()}/{(input.fx_to() or 'TO').upper()}"
        return ui.tags.div(
            ui.tags.h4("FX conversion and stress calculator"),
            ui.tags.div(
                _stat("Pair", pair),
                _stat("Current conversion", f"{converted:.2f}"),
                _stat("Shocked value", f"{shocked_value:.2f}"),
                _stat("Value change", f"{delta:.2f}", "value positive" if delta >= 0 else "value negative"),
                class_="stats-row",
            ),
            ui.tags.p("Stress result applies a simple percentage shock to latest close rate.", style="margin: 0; color: var(--text-muted); font-size: 0.8rem;"),
            class_="card-custom",
        )

    @render.data_frame
    def table_fx():
        return _table_card(fx_df.get())

    @render.download_button(filename=lambda: f"fx_{(input.fx_from() or 'from')}_{(input.fx_to() or 'to')}_{date.today()}.csv")
    def download_fx_csv():
        df = fx_df.get()
        if df is not None and not df.empty:
            yield df.to_csv(index=False)

    # ---- Commodities ----

    @reactive.effect
    @reactive.event(input.fetch_commodity)
    def _():
        clear_err()
        clear_fallback()
        try:
            com = input.commodity() or "WHEAT"
            interval = input.commodity_interval() or "monthly"
            rk = f"commodity|{com}|{interval}"
            res = safe_fetch_with_cache(rk, lambda: av_commodity(commodity=com, interval=interval))
            if not res["ok"]:
                set_err(res.get("error") or "Fetch failed.")
                return
            commodity_df.set(res["data"])
            if res.get("fallback"):
                fallback_msg.set(res.get("msg") or "Using cached data.")
        except Exception as e:
            set_err(str(e))

    @render.ui
    def ui_commodity_toolbar():
        df = commodity_df.get()
        if df is None or df.empty:
            return None
        return ui.tags.div(
            ui.tags.div(ui.input_select("commodity_points", qmark("Chart points", "How many commodity observations to display."), choices=["30", "60", "90"], selected="60"), class_="control-field"),
            ui.tags.div(ui.input_numeric("commodity_calc_units", qmark("Units", "Quantity used in notional and P/L scenario calculator."), value=100, min=0, step=1), class_="control-field"),
            ui.tags.div(ui.input_numeric("commodity_calc_move", qmark("Price move %", "One-time price change used in scenario notional."), value=3, min=-80, max=200, step=0.5), class_="control-field"),
            ui.tags.div(ui.input_numeric("commodity_scn_steps", qmark("Scenario steps", "Future periods projected on scenario line."), value=12, min=1, max=120, step=1), class_="control-field"),
            ui.tags.div(ui.input_numeric("commodity_scn_drift", qmark("Scenario step %", "Assumed percent move per projected step."), value=0.5, min=-30, max=30, step=0.1), class_="control-field"),
            ui.download_button("download_commodity_csv", "Download CSV", class_="btn-default"),
            class_="control-bar",
        )

    @render.ui
    def ui_commodity_summary():
        df = commodity_df.get()
        if df is None or df.empty:
            return None
        vv = df["value"].dropna()
        if vv.empty:
            return None
        return ui.tags.div(
            _stat(f"{input.commodity()} — Latest", f"{vv.iloc[0]:,.2f}"),
            _stat("Period low", f"{vv.min():,.2f}"),
            _stat("Period high", f"{vv.max():,.2f}"),
            _stat("Data points", f"{len(df)}"),
            class_="stats-row",
        )

    @render_widget
    def plot_commodity():
        df = commodity_df.get()
        req(df is not None and not df.empty)
        pts = int(input.commodity_points() or 60)
        d = df.head(pts).dropna(subset=["date", "value"]).sort_values("date")
        req(not d.empty)
        fig = go.Figure(go.Scatter(x=d["date"], y=d["value"], mode="lines", name="Observed", line=dict(color=CHART_COLORS["close"], width=2.4)))
        scn_steps = int(input.commodity_scn_steps() or 12)
        scn_drift = float(input.commodity_scn_drift() or 0)
        subtitle = None
        if scn_steps > 0:
            last_dt = d["date"].max()
            last_v = float(d["value"].iloc[-1])
            if np.isfinite(last_v) and last_v > 0:
                fut_dates = pd.date_range(last_dt + pd.Timedelta(days=1), periods=scn_steps, freq="D")
                fut_vals = last_v * np.cumprod(np.repeat(1 + scn_drift / 100, scn_steps))
                fig.add_trace(go.Scatter(x=fut_dates, y=fut_vals, mode="lines+markers", name="Scenario", line=dict(color=CHART_COLORS["scenario"], width=2, dash="dashdot")))
                subtitle = "Orange path: user-defined commodity scenario"
        market_layout(fig, y_title="Value", x_title="Date", subtitle=subtitle)
        return fig

    @render_widget
    def plot_commodity_changes():
        commodity_settle.get()
        with reactive.isolate():
            df = commodity_df.get()
        req(df is not None and len(df) >= 3)
        pts = int(input.commodity_points() or 60)
        d = df.head(pts).dropna(subset=["date", "value"]).sort_values("date").copy()
        d["chg"] = 100 * (d["value"] / d["value"].shift(1) - 1)
        d = d.dropna(subset=["chg"])
        req(not d.empty)
        colors = [CHART_COLORS["up"] if c >= 0 else CHART_COLORS["down"] for c in d["chg"]]
        fig = go.Figure(go.Bar(x=d["date"], y=d["chg"], marker_color=colors))
        fig.add_hline(y=0, line_color="#94a3b8", line_width=1)
        market_layout(fig, y_title="Period change (%)", x_title="Date", show_legend=False, subtitle="Commodity change bars")
        return fig

    @render.ui
    def ui_commodity_calculator():
        df = commodity_df.get()
        if df is None or df.empty:
            return None
        px = float(df["value"].iloc[0])
        if not np.isfinite(px) or px <= 0:
            return None
        units = float(input.commodity_calc_units() or 0)
        move = float(input.commodity_calc_move() or 0)
        base = units * px
        shock_px = px * (1 + move / 100)
        shock_val = units * shock_px
        pnl = shock_val - base
        return ui.tags.div(
            ui.tags.h4("Commodity scenario calculator"),
            ui.tags.div(
                _stat("Units", f"{units:,.0f}"),
                _stat("Current notional", f"{base:.2f}"),
                _stat("Scenario notional", f"{shock_val:.2f}"),
                _stat("Scenario P/L", f"{pnl:.2f}", "value positive" if pnl >= 0 else "value negative"),
                class_="stats-row",
            ),
            ui.tags.p("Simple sensitivity estimate using latest displayed value and user-defined percentage move.", style="margin: 0; color: var(--text-muted); font-size: 0.8rem;"),
            class_="card-custom",
        )

    @render.data_frame
    def table_commodity():
        return _table_card(commodity_df.get())

    @render.download_button(filename=lambda: f"commodity_{(input.commodity() or '')}_{date.today()}.csv")
    def download_commodity_csv():
        df = commodity_df.get()
        if df is not None and not df.empty:
            yield df.to_csv(index=False)

    # ---- Economic ----

    @reactive.effect
    @reactive.event(input.fetch_economic)
    def _():
        clear_err()
        clear_fallback()
        try:
            ind = input.economic_indicator()
            interval = input.economic_interval()
            rk = f"economic|{ind}|{interval}"

            def fetch():
                if ind == "TREASURY_YIELD":
                    return av_economic(indicator=ind, interval=interval, maturity="10year")
                if ind == "REAL_GDP":
                    return av_economic(indicator=ind, interval=interval if interval in ("quarterly", "annual") else "annual")
                return av_economic(indicator=ind, interval=interval)

            res = safe_fetch_with_cache(rk, fetch)
            if not res["ok"]:
                set_err(res.get("error") or "Fetch failed.")
                return
            economic_df.set(res["data"])
            if res.get("fallback"):
                fallback_msg.set(res.get("msg") or "Using cached data.")
        except Exception as e:
            set_err(str(e))

    @render.ui
    def ui_economic_toolbar():
        df = economic_df.get()
        if df is None or df.empty:
            return None
        return ui.tags.div(
            ui.tags.div(ui.input_select("economic_points", "Chart points", choices=["30", "60", "90"], selected="60"), class_="control-field"),
            ui.download_button("download_economic_csv", "Download CSV", class_="btn-default"),
            class_="control-bar",
        )

    @render.ui
    def ui_economic_summary():
        df = economic_df.get()
        if df is None or df.empty:
            return None
        ycol = "value" if "value" in df.columns else df.columns[1]
        vv = pd.to_numeric(df[ycol], errors="coerce").dropna()
        if vv.empty:
            return None
        return ui.tags.div(
            _stat(f"{input.economic_indicator()} — Latest", f"{vv.iloc[0]:,.4f}"),
            _stat("Period min", f"{vv.min():,.4f}"),
            _stat("Period max", f"{vv.max():,.4f}"),
            _stat("Observations", f"{len(df)}"),
            class_="stats-row",
        )

    @render_widget
    def plot_economic():
        df = economic_df.get()
        req(df is not None and not df.empty)
        pts = int(input.economic_points() or 60)
        xcol = "date" if "date" in df.columns else df.columns[0]
        ycol = "value" if "value" in df.columns else df.columns[1]
        d = df.head(pts).copy()
        d[ycol] = pd.to_numeric(d[ycol], errors="coerce")
        d = d.dropna(subset=[xcol, ycol])
        req(not d.empty)
        fig = go.Figure(go.Scatter(x=d[xcol], y=d[ycol], mode="lines", line=dict(color=CHART_COLORS["close"], width=2.4)))
        market_layout(fig, y_title=ycol, x_title=xcol, show_legend=False)
        return fig

    @render.data_frame
    def table_economic():
        return _table_card(economic_df.get())

    @render.download_button(filename=lambda: f"economic_{(input.economic_indicator() or '')}_{date.today()}.csv")
    def download_economic_csv():
        df = economic_df.get()
        if df is not None and not df.empty:
            yield df.to_csv(index=False)

    # ---- AI Reporter ----

    def report_choices() -> dict:
        sec = input.section()
        mapping = {
            "stock": {"brief": "Brief on current data", "stock": "Stock snapshot", "stock_trend": "Stock trend (day/week/month/year)"},
            "gainers": {"movers": "Top movers insight"},
            "news": {"news": "News briefing"},
            "forex": {"forex_brief": "Brief on current forex data", "forex_snapshot": "Forex snapshot", "forex_trend": "Forex trend (day/week/month/year)"},
            "commodity": {"commodity_brief": "Brief on current commodity data", "commodity_snapshot": "Commodity snapshot", "commodity_trend": "Commodity trend (day/week/month/year)"},
            "economic": {"economic_brief": "Brief on indicator data", "economic_snapshot": "Indicator snapshot", "economic_trend": "Indicator trend (day/week/month/year)"},
        }
        return mapping.get(sec, {"brief": "Brief on current data"})

    def build_ai_context(report_type: str) -> str:
        parts = []
        if report_type in ("movers", "brief"):
            d = gainers_data.get()
            if d is not None:
                g = d.get("top_gainers")
                if g is not None and not g.empty:
                    g5 = g.head(5)
                    tk = _find_col(g5, "ticker")
                    parts.append(f"Top gainers (sample): {', '.join(g5[tk].astype(str))}")
                l = d.get("top_losers")
                if l is not None and not l.empty:
                    l5 = l.head(5)
                    tk = _find_col(l5, "ticker")
                    parts.append(f"Top losers (sample): {', '.join(l5[tk].astype(str))}")
        if report_type in ("news", "brief"):
            nw = news_df.get()
            if nw is not None and not nw.empty:
                parts.append("Recent headlines: " + " | ".join(nw["title"].astype(str).head(8)))
        if report_type in ("stock", "brief"):
            st = stock_df.get()
            if st is not None and not st.empty:
                r = st.iloc[0]
                parts.append(f"Latest stock: close {r['close']:.2f}, high {r.get('high', float('nan')):.2f}, low {r.get('low', float('nan')):.2f}")
        if report_type in ("forex_brief", "forex_snapshot"):
            fx = fx_df.get()
            if fx is not None and not fx.empty:
                parts.append(
                    f"Forex {input.fx_from() or 'From'}/{input.fx_to() or 'To'}: latest close {float(fx['close'].iloc[0]):.4f}, "
                    f"high {float(fx['close'].max()):.4f}, low {float(fx['close'].min()):.4f} (over {len(fx)} days)"
                )
        if report_type in ("commodity_brief", "commodity_snapshot"):
            co = commodity_df.get()
            if co is not None and not co.empty:
                v = co["value"].dropna()
                if len(v):
                    parts.append(f"Commodity {input.commodity() or ''}: latest {v.iloc[0]:.2f}, period min {v.min():.2f}, max {v.max():.2f} ({len(co)} points)")
        if report_type in ("economic_brief", "economic_snapshot"):
            ec = economic_df.get()
            if ec is not None and not ec.empty:
                v = pd.to_numeric(ec.get("value"), errors="coerce").dropna()
                if len(v):
                    parts.append(f"Indicator {input.economic_indicator() or ''}: latest {v.iloc[0]:.4f}, period min {v.min():.4f}, max {v.max():.4f} ({len(ec)} points)")
        if not parts:
            return "No data for this section yet. Fetch data in the current section first."
        return "\n\n".join(parts)

    @reactive.effect
    def _():
        ch = report_choices()
        ui.update_select("ai_report_type", choices=ch, selected=next(iter(ch)))

    @render.ui
    def ui_ai_reporter():
        cred = pick_llm_credentials()
        has_key = cred["ok"]
        loading = ai_loading.get()
        report = ai_report_text.get()
        orch = ai_orchestrator_text.get()
        an = ai_analyst_text.get()
        ragtr = ai_rag_trace_text.get()
        ch = report_choices()
        rt = input.ai_report_type() if "ai_report_type" in input else None
        needs_ai = (rt or "") in (
            "brief", "stock", "movers", "news", "forex_brief", "forex_snapshot",
            "commodity_brief", "commodity_snapshot", "economic_brief", "economic_snapshot",
        )
        providers = sorted({c["provider"] for c in cred["creds"]}) if has_key else []
        prov_lab = f" (available: {', '.join(providers)})" if providers else ""

        if needs_ai and not has_key:
            return ui.tags.div(
                ui.tags.div("Multi-agent AI analysis", class_="ai-title"),
                ui.tags.div(
                    "Generate a concise market summary from the data you have fetched, including key moves, "
                    "context, and takeaways in plain language. To enable AI reports, add an API key in your .env file.",
                    class_="ai-desc",
                ),
                ui.tags.div(
                    ui.tags.p(
                        "Ollama Cloud: ", ui.tags.a("ollama.com/settings", href="https://ollama.com/settings", target="_blank"),
                        ". OpenAI: OPENAI_API_KEY=sk-... in .env.", style="margin: 0; font-size: 0.9rem;",
                    ),
                    class_="ai-reporter-connect",
                ),
                class_="ai-reporter-card",
            )

        selected = rt if rt in ch else next(iter(ch))
        parts = [
            ui.tags.div(
                ui.tags.div("Multi-agent AI analysis", class_="ai-title"),
                ui.tags.div(
                    f"Create a user-friendly market brief from the section data you fetched{prov_lab}. The report "
                    "highlights important trends, possible drivers, and what to watch next. Choose a report type, "
                    "then click Generate report. You can view the full RAG retrieval details by scrolling down to "
                    "the RAG retrieval section.",
                    class_="ai-desc",
                ),
                ui.input_select("ai_report_type", "Report type", choices=ch, selected=selected),
                ui.tags.div(
                    ui.tags.p(
                        "RAG: edit ", ui.tags.code("data/rag_market_corpus.txt"), " (chunks separated by ",
                        ui.tags.code("---"), ") to change reference notes. No LLM tool calling.",
                        style="margin: 0; font-size: 0.78rem; color: var(--text-muted);",
                    ),
                    class_="ai-reporter-connect", style="margin-top: 0.5rem;",
                ) if (needs_ai and has_key) else None,
                ui.input_action_button("ai_generate", "Generate report", class_="btn-primary"),
                class_="ai-reporter-card",
            )
        ]
        if loading:
            parts.append(ui.tags.div(ui.tags.p("Running 3-agent pipeline…", style="margin: 0; color: var(--text-muted);"), class_="card-custom pulse"))
        if not loading and report:
            parts.append(ui.tags.div(ui.tags.h4("Final brief"), ui.tags.div(report, class_="ai-report-output"), class_="card-custom", style="margin-top: 1rem;"))
        if not loading and (orch or an or ragtr):
            steps = [ui.tags.p("Agent traces (intermediate outputs)", style="margin: 0 0 0.5rem 0; font-size: 0.8rem; color: var(--text-muted);")]
            if orch:
                steps.append(ui.tags.details(ui.tags.summary("1. Orchestrator — plan & gaps"), _agent_body(orch), class_="agent-step"))
            if an:
                steps.append(ui.tags.details(ui.tags.summary("2. Market Analyst — evidence memo"), _agent_body(an), class_="agent-step"))
            if ragtr:
                steps.append(ui.tags.details(ui.tags.summary("RAG retrieval (local corpus)"), _agent_body(ragtr), class_="agent-step"))
            parts.append(ui.tags.div(*steps, class_="agent-pipeline"))
        return ui.TagList(*parts)

    @reactive.effect
    @reactive.event(input.ai_generate)
    def _():
        report_type = input.ai_report_type() or "brief"
        ai_loading.set(True)
        ai_provider_trace.set(None)
        ai_report_text.set(None)
        ai_orchestrator_text.set(None)
        ai_analyst_text.set(None)
        ai_rag_trace_text.set(None)

        if report_type == "stock_trend":
            ai_loading.set(False)
            ai_report_text.set(build_stock_trend_report(stock_df.get(), input.stock_symbol() or "Stock"))
            return
        if report_type == "forex_trend":
            ai_loading.set(False)
            ai_report_text.set(build_forex_trend_report(fx_df.get(), input.fx_from() or "From", input.fx_to() or "To"))
            return
        if report_type == "commodity_trend":
            ai_loading.set(False)
            ai_report_text.set(build_value_trend_report(commodity_df.get(), f"Commodity ({input.commodity() or ''})", "value"))
            return
        if report_type == "economic_trend":
            ai_loading.set(False)
            ai_report_text.set(build_value_trend_report(economic_df.get(), f"Indicator ({input.economic_indicator() or ''})", "value"))
            return

        cred = pick_llm_credentials()
        if not cred["ok"]:
            ai_loading.set(False)
            return
        context = build_ai_context(report_type)

        with ui.Progress(min=0, max=1) as prog:
            prog.set(0, message="Multi-agent analysis")

            def progress_fn(value, detail):
                prog.set(value, message="Multi-agent analysis", detail=detail)

            try:
                res = run_agentic_pipeline(report_type, input.section() or "", context, cred, progress_fn=progress_fn)
            except Exception as e:
                res = {"ok": False, "error": str(e), "orchestrator": None, "analyst": None, "final": None, "rag_trace": None}

        ai_loading.set(False)
        if not res.get("ok"):
            if res.get("orchestrator"):
                ai_orchestrator_text.set(res["orchestrator"])
            if res.get("analyst"):
                ai_analyst_text.set(res["analyst"])
            rt_err = res.get("rag_trace")
            ai_rag_trace_text.set(rt_err if rt_err else None)
            set_err(res.get("error") or "Analysis failed.")
            return
        if res.get("providers_used"):
            ai_provider_trace.set(" | ".join(res["providers_used"]))
        ai_orchestrator_text.set(res["orchestrator"])
        ai_analyst_text.set(res["analyst"])
        ai_rag_trace_text.set(res.get("rag_trace") or None)
        ai_report_text.set(res["final"])
