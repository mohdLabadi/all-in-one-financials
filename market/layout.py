"""Top-level page layout: nav, section tabs, and content/AI-rail grid."""

from shiny import ui
from shinywidgets import output_widget

from .config import APP_DIR

SYNC_TAB_JS = """
$(function(){
  function syncActiveTab(){
    $('#section .radio-inline, #section .form-check').removeClass('active-tab');
    $('#section input:checked').closest('.radio-inline, .form-check').addClass('active-tab');
  }
  syncActiveTab();
  $(document).on('change', '#section input', syncActiveTab);
  $(document).on('shiny:value shiny:visualchange', function(){ syncActiveTab(); });
});
"""

SECTION_CHOICES = {
    "stock": "Stock Daily",
    "gainers": "Top Gainers/Losers",
    "news": "News & Sentiment",
    "forex": "Forex",
    "commodity": "Commodities",
    "economic": "Economic Indicators",
    "ai": "AI Reporter",
}

app_ui = ui.page_fluid(
    ui.include_css(APP_DIR / "static" / "styles.css"),
    ui.tags.head(
        ui.tags.title("Daily Market Analysis"),
        ui.tags.link(rel="preconnect", href="https://fonts.googleapis.com"),
        ui.tags.link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=""),
        ui.tags.link(
            href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap",
            rel="stylesheet",
        ),
        ui.tags.script(SYNC_TAB_JS),
    ),
    ui.tags.div(
        ui.tags.div(
            ui.tags.div(
                ui.tags.div("M", class_="brand-mark"),
                ui.tags.div(
                    ui.tags.div("Daily Market Analysis", class_="brand-name"),
                    ui.tags.div("Stocks · Forex · News · Multi-agent AI", class_="brand-tagline"),
                    class_="brand-text",
                ),
                class_="brand",
            ),
            ui.tags.div(
                ui.input_radio_buttons("section", None, choices=SECTION_CHOICES, selected="stock", inline=True),
                class_="nav-tabs-wrap",
            ),
            ui.tags.div(ui.output_ui("ui_top_status", inline=True), class_="top-nav-status"),
            class_="top-nav-inner",
        ),
        class_="top-nav",
    ),
    ui.tags.div(
        ui.tags.div(ui.output_ui("section_title"), ui.output_ui("ui_section_help"), class_="page-hero-inner"),
        class_="page-hero",
    ),
    ui.tags.div(
        ui.output_ui("api_fallback_ui"),
        ui.output_ui("api_error_ui"),
        ui.tags.div(ui.input_checkbox("show_raw_data", "Show data tables", value=False), class_="utility-row"),
        ui.tags.div(
            ui.tags.div(
                ui.panel_conditional(
                    "input.section === 'ai'",
                    ui.tags.div(
                        ui.tags.p(
                            "AI Reporter is available in the right panel. Fetch any data section first for best "
                            "results, then click Generate report.",
                            style="margin:0; color: var(--text-muted);",
                        ),
                        class_="card-custom",
                    ),
                ),
                ui.panel_conditional(
                    "input.section === 'stock'",
                    ui.tags.div(
                        ui.tags.div(
                            ui.input_text("stock_symbol", "Symbol", value="AAPL", placeholder="e.g. AAPL, MSFT"),
                            class_="control-field",
                        ),
                        ui.tags.div(
                            ui.input_checkbox("stock_full_history", "Full history (1-yr trend)", value=False),
                            class_="form-check",
                        ),
                        ui.input_action_button("fetch_stock", "Fetch", class_="btn-primary"),
                        class_="control-bar",
                    ),
                ),
                ui.panel_conditional(
                    "input.section === 'gainers'",
                    ui.tags.div(ui.input_action_button("fetch_gainers", "Fetch movers", class_="btn-primary"), class_="control-bar"),
                ),
                ui.panel_conditional(
                    "input.section === 'news'",
                    ui.tags.div(
                        ui.tags.div(ui.input_text("news_tickers", "Tickers (optional)", placeholder="AAPL, MSFT"), class_="control-field"),
                        ui.tags.div(ui.input_numeric("news_limit", "Limit", value=20, min=1, max=50), class_="control-field"),
                        ui.input_action_button("fetch_news", "Fetch news", class_="btn-primary"),
                        class_="control-bar",
                    ),
                ),
                ui.panel_conditional(
                    "input.section === 'forex'",
                    ui.tags.div(
                        ui.tags.div(ui.input_text("fx_from", "From", value="EUR"), class_="control-field"),
                        ui.tags.div(ui.input_text("fx_to", "To", value="USD"), class_="control-field"),
                        ui.input_action_button("fetch_fx", "Fetch", class_="btn-primary"),
                        class_="control-bar",
                    ),
                ),
                ui.panel_conditional(
                    "input.section === 'commodity'",
                    ui.tags.div(
                        ui.tags.div(
                            ui.input_select(
                                "commodity", "Commodity",
                                choices=["WHEAT", "CORN", "WTI", "BRENT", "NATURAL_GAS", "COPPER", "COFFEE"],
                            ),
                            class_="control-field",
                        ),
                        ui.tags.div(
                            ui.input_select("commodity_interval", "Interval", choices=["daily", "weekly", "monthly"], selected="monthly"),
                            class_="control-field",
                        ),
                        ui.input_action_button("fetch_commodity", "Fetch", class_="btn-primary"),
                        class_="control-bar",
                    ),
                ),
                ui.panel_conditional(
                    "input.section === 'economic'",
                    ui.tags.div(
                        ui.tags.div(
                            ui.input_select(
                                "economic_indicator", "Indicator",
                                choices=[
                                    "CPI", "INFLATION", "UNEMPLOYMENT", "FEDERAL_FUNDS_RATE",
                                    "TREASURY_YIELD", "REAL_GDP", "RETAIL_SALES", "NONFARM_PAYROLL",
                                ],
                            ),
                            class_="control-field",
                        ),
                        ui.tags.div(
                            ui.input_select("economic_interval", "Interval", choices=["monthly", "quarterly", "annual", "daily"], selected="monthly"),
                            class_="control-field",
                        ),
                        ui.input_action_button("fetch_economic", "Fetch", class_="btn-primary"),
                        class_="control-bar",
                    ),
                ),
                ui.tags.p(
                    "Select a section and click Fetch to load data. View, filter, and Download use already-loaded data (no extra API calls).",
                    class_="api-limit-note",
                ),
                ui.output_ui("ui_content_empty_state"),
                ui.panel_conditional(
                    "input.section === 'stock'",
                    ui.output_ui("ui_stock_toolbar"),
                    ui.output_ui("ui_stock_summary"),
                    ui.tags.div(output_widget("plot_stock", height="380px"), class_="plot-container"),
                    ui.tags.div(output_widget("plot_stock_returns", height="240px"), class_="plot-container"),
                    ui.output_ui("ui_stock_calculator"),
                    ui.panel_conditional("input.show_raw_data", ui.tags.div(ui.output_data_frame("table_stock"), class_="table-card")),
                ),
                ui.panel_conditional(
                    "input.section === 'gainers'",
                    ui.output_ui("ui_gainers_toolbar"),
                    ui.output_ui("ui_gainers_cards"),
                    ui.output_ui("ui_gainers_chart"),
                    ui.panel_conditional(
                        "input.show_raw_data",
                        ui.tags.h4("Top Gainers"), ui.tags.div(ui.output_data_frame("table_gainers"), class_="table-card"),
                        ui.tags.h4("Top Losers"), ui.tags.div(ui.output_data_frame("table_losers"), class_="table-card"),
                        ui.tags.h4("Most Active"), ui.tags.div(ui.output_data_frame("table_active"), class_="table-card"),
                    ),
                ),
                ui.panel_conditional(
                    "input.section === 'news'",
                    ui.output_ui("ui_news_toolbar"),
                    ui.tags.div(output_widget("plot_news_sentiment", height="250px"), class_="plot-container"),
                    ui.output_ui("ui_news_cards"),
                    ui.panel_conditional("input.show_raw_data", ui.tags.div(ui.output_data_frame("table_news"), class_="table-card")),
                ),
                ui.panel_conditional(
                    "input.section === 'forex'",
                    ui.output_ui("ui_fx_toolbar"),
                    ui.output_ui("ui_fx_summary"),
                    ui.tags.div(output_widget("plot_fx", height="380px"), class_="plot-container"),
                    ui.tags.div(output_widget("plot_fx_returns", height="240px"), class_="plot-container"),
                    ui.output_ui("ui_fx_calculator"),
                    ui.panel_conditional("input.show_raw_data", ui.tags.div(ui.output_data_frame("table_fx"), class_="table-card")),
                ),
                ui.panel_conditional(
                    "input.section === 'commodity'",
                    ui.output_ui("ui_commodity_toolbar"),
                    ui.output_ui("ui_commodity_summary"),
                    ui.tags.div(output_widget("plot_commodity", height="380px"), class_="plot-container"),
                    ui.tags.div(output_widget("plot_commodity_changes", height="240px"), class_="plot-container"),
                    ui.output_ui("ui_commodity_calculator"),
                    ui.panel_conditional("input.show_raw_data", ui.tags.div(ui.output_data_frame("table_commodity"), class_="table-card")),
                ),
                ui.panel_conditional(
                    "input.section === 'economic'",
                    ui.output_ui("ui_economic_toolbar"),
                    ui.output_ui("ui_economic_summary"),
                    ui.tags.div(output_widget("plot_economic", height="380px"), class_="plot-container"),
                    ui.panel_conditional("input.show_raw_data", ui.tags.div(ui.output_data_frame("table_economic"), class_="table-card")),
                ),
                class_="content-main",
            ),
            ui.tags.div(ui.output_ui("ui_ai_reporter"), class_="content-rail"),
            class_="content-grid",
        ),
        class_="main-container",
    ),
    title="Daily Market Analysis",
)
