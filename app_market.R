# Daily Market Analysis Tool – Alpha Vantage
# Based on 06_alphavantage_ai_report.R: readRenviron(.env), API_KEY / ALPHAVANTAGE_API_KEY, httr2.
# Run: setwd("tool 1"); shiny::runApp("app_market.R")  or  source("run_market.R")
# Requires: httr2, jsonlite, shiny, ggplot2, DT, dplyr

# Load jsonlite before shiny so it doesn't mask shiny's validate() (which breaks reactivity)
library(jsonlite)
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
# Use httr2 like reference script (fallback to httr if httr2 not installed)
use_httr2 <- suppressWarnings(requireNamespace("httr2", quietly = TRUE))
if (use_httr2) library(httr2) else suppressPackageStartupMessages(library(httr))

# ---- Load .env (same pattern as 06_alphavantage_ai_report.R: readRenviron) ----
env_path <- NULL
if (file.exists(".env")) {
  env_path <- ".env"
} else if (file.exists("tool 1/.env")) {
  env_path <- "tool 1/.env"
} else if (file.exists("../.env")) {
  env_path <- "../.env"
} else if (file.exists(file.path(getwd(), ".env"))) {
  env_path <- file.path(getwd(), ".env")
} else if (file.exists(file.path(getwd(), "tool 1", ".env"))) {
  env_path <- file.path(getwd(), "tool 1", ".env")
}
if (!is.null(env_path)) readRenviron(env_path)

# API key: support both names (reference uses API_KEY, this project uses ALPHAVANTAGE_API_KEY)
API_KEY <- trimws(Sys.getenv("ALPHAVANTAGE_API_KEY"))
if (!nzchar(API_KEY)) API_KEY <- trimws(Sys.getenv("API_KEY"))

# AI Reporter: optional API keys (set in .env). Ollama Cloud preferred when set.
OLLAMA_CLOUD_API_KEY <- trimws(Sys.getenv("OLLAMA_CLOUD_API_KEY"))
if (!nzchar(OLLAMA_CLOUD_API_KEY)) OLLAMA_CLOUD_API_KEY <- trimws(Sys.getenv("OLLAMA_API_KEY"))
OPENAI_API_KEY <- trimws(Sys.getenv("OPENAI_API_KEY"))

# ---- Alpha Vantage API (httr2 like reference, else httr) ----
`%||%` <- function(x, y) if (is.null(x)) y else x
base_url <- "https://www.alphavantage.co/query"

av_get <- function(params) {
  if (!nzchar(API_KEY)) stop("API key not set. Put API_KEY or ALPHAVANTAGE_API_KEY in .env in the app folder.")
  params$apikey <- API_KEY
  if (use_httr2) {
    req <- request(base_url)
    req <- do.call(req_url_query, c(list(req), params))
    req <- req_method(req, "GET")
    resp <- req_perform(req)
    if (resp_status(resp) != 200L) stop("Alpha Vantage request failed. Check API key or URL.")
    out <- resp_body_json(resp)
  } else {
    r <- GET(base_url, query = params, user_agent("R-Shiny-Market-Tool"))
    if (http_error(r)) stop("Alpha Vantage request failed: ", status_code(r))
    out <- content(r, as = "parsed", type = "application/json")
  }
  if (is.list(out) && !is.null(out$`Error Message`)) stop(out$`Error Message`)
  if (is.list(out) && !is.null(out$`Note`)) {
    msg <- out$`Note`
    stop("API rate limit: ", msg, " Free key: 5 requests/min, ~25/day. Wait a minute or try again tomorrow.")
  }
  if (is.list(out) && !is.null(out$Information)) stop("API: ", out$Information)
  out
}

# ---- AI Reporter: Ollama Cloud or OpenAI (optional) ----
# Ollama Cloud: https://ollama.com/api/chat, returns message.content
# Use a cloud-supported model; docs recommend gpt-oss:120b for direct cloud API.
ollama_chat <- function(prompt, api_key, model = "gpt-oss:120b") {
  if (!nzchar(api_key)) return(list(ok = FALSE, error = "No API key provided."))
  body <- list(model = model, messages = list(list(role = "user", content = prompt)), stream = FALSE)
  if (use_httr2) {
    tryCatch({
      req <- request("https://ollama.com/api/chat") %>%
        req_headers(Authorization = paste0("Bearer ", api_key), `Content-Type` = "application/json") %>%
        req_body_json(body) %>% req_method("POST")
      resp <- req_perform(req)
      if (resp_status(resp) != 200L) return(list(ok = FALSE, error = paste0("API status ", resp_status(resp))))
      out <- resp_body_json(resp)
      text <- out$message$content
      if (is.null(text)) return(list(ok = FALSE, error = "Empty response."))
      return(list(ok = TRUE, text = text))
    }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  } else {
    tryCatch({
      r <- POST("https://ollama.com/api/chat",
        add_headers(Authorization = paste0("Bearer ", api_key), `Content-Type` = "application/json"),
        body = body, encode = "json")
      if (status_code(r) != 200L) return(list(ok = FALSE, error = paste0("API status ", status_code(r))))
      out <- content(r, as = "parsed")
      text <- out$message$content
      if (is.null(text)) return(list(ok = FALSE, error = "Empty response."))
      return(list(ok = TRUE, text = text))
    }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  }
}

openai_chat <- function(prompt, api_key, model = "gpt-4o-mini") {
  if (!nzchar(api_key)) return(list(ok = FALSE, error = "No API key provided."))
  body <- list(model = model, messages = list(list(role = "user", content = prompt)), max_tokens = 1024L)
  if (use_httr2) {
    tryCatch({
      req <- request("https://api.openai.com/v1/chat/completions") %>%
        req_headers(Authorization = paste0("Bearer ", api_key), `Content-Type` = "application/json") %>%
        req_body_json(body) %>% req_method("POST")
      resp <- req_perform(req)
      if (resp_status(resp) != 200L) return(list(ok = FALSE, error = paste0("API status ", resp_status(resp))))
      out <- resp_body_json(resp)
      text <- out$choices[[1]]$message$content
      if (is.null(text)) return(list(ok = FALSE, error = "Empty response."))
      return(list(ok = TRUE, text = text))
    }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  } else {
    tryCatch({
      r <- POST("https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste0("Bearer ", api_key), `Content-Type` = "application/json"),
        body = body, encode = "json")
      if (status_code(r) != 200L) return(list(ok = FALSE, error = paste0("API status ", status_code(r))))
      out <- content(r, as = "parsed")
      text <- out$choices[[1]]$message$content
      if (is.null(text)) return(list(ok = FALSE, error = "Empty response."))
      return(list(ok = TRUE, text = text))
    }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  }
}

# TIME_SERIES_DAILY: daily OHLCV. Build rows explicitly to avoid date/format issues (like 06_alphavantage_ai_report.R).
av_stock_daily <- function(symbol = "AAPL", outputsize = "compact") {
  out <- av_get(list(`function` = "TIME_SERIES_DAILY", symbol = symbol, outputsize = outputsize))
  ts <- out[["Time Series (Daily)"]]
  if (is.null(ts) || length(ts) == 0L) {
    # Surface what the API actually returned (helps debug invalid key, bad symbol, etc.)
    keys <- names(out)
    extra <- ""
    if (length(keys) > 0L) {
      if (!is.null(out$`Error Message`)) extra <- paste0(" API said: ", out$`Error Message`)
      else if (!is.null(out$Information)) extra <- paste0(" API said: ", out$Information)
      else if (!is.null(out$Note)) extra <- paste0(" API said: ", out$Note)
      else extra <- paste0(" Response keys: ", paste(keys, collapse = ", "))
    }
    stop("No time series in response for ", symbol, ".", extra)
  }
  dates_char <- names(ts)
  if (is.null(dates_char) || length(dates_char) != length(ts)) return(data.frame())
  n <- length(dates_char)
  out_list <- vector("list", n)
  for (i in seq_len(n)) {
    d <- ts[[i]]
    out_list[[i]] <- data.frame(
      date = dates_char[i],
      open = as.numeric(d[["1. open"]] %||% NA),
      high = as.numeric(d[["2. high"]] %||% NA),
      low = as.numeric(d[["3. low"]] %||% NA),
      close = as.numeric(d[["4. close"]] %||% NA),
      volume = as.numeric(d[["5. volume"]] %||% NA),
      stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, out_list)
  df$date <- as.Date(df$date, format = "%Y-%m-%d")
  df <- df[order(df$date, decreasing = TRUE), ]
  rownames(df) <- NULL
  df
}

list_to_df <- function(x) {
  if (is.null(x) || length(x) == 0L) return(data.frame())
  if (is.data.frame(x)) return(x)
  if (!is.list(x)) return(as.data.frame(x))
  first <- x[[1L]]
  if (length(x) > 0L && (is.vector(first) || is.factor(first)) && !is.list(first))
    return(as.data.frame(x, stringsAsFactors = FALSE))
  tryCatch(
    dplyr::bind_rows(lapply(x, function(e) {
      if (is.list(e) && !is.data.frame(e)) as.data.frame(e, stringsAsFactors = FALSE) else as.data.frame(as.list(e), stringsAsFactors = FALSE)
    })), error = function(e) as.data.frame(x))
}

av_top_gainers_losers <- function() {
  out <- av_get(list(`function` = "TOP_GAINERS_LOSERS"))
  list(metadata = out$metadata, top_gainers = list_to_df(out$top_gainers),
    top_losers = list_to_df(out$top_losers), top_activated = list_to_df(out$most_actively_traded))
}

av_news_sentiment <- function(tickers = "", limit = 50) {
  params <- list(`function` = "NEWS_SENTIMENT", limit = limit, sort = "LATEST")
  if (nzchar(tickers)) params$tickers <- tickers
  out <- av_get(params)
  feed <- out$feed
  if (is.null(feed) || length(feed) == 0) return(data.frame())
  df <- do.call(rbind, lapply(feed, function(x) data.frame(title = as.character(x$title %||% NA), url = as.character(x$url %||% NA),
    time_published = as.character(x$time_published %||% NA), summary = as.character(x$summary %||% NA),
    overall_sentiment_score = as.numeric(x$overall_sentiment_score %||% NA), overall_sentiment_label = as.character(x$overall_sentiment_label %||% NA),
    source = as.character(x$source %||% NA), stringsAsFactors = FALSE)))
  if (nrow(df) > 0) df else data.frame()
}

# FX_DAILY: same explicit row-by-row parsing to avoid date/format and orientation issues.
av_fx_daily <- function(from_symbol = "EUR", to_symbol = "USD") {
  out <- av_get(list(`function` = "FX_DAILY", from_symbol = from_symbol, to_symbol = to_symbol))
  ts <- out[["Time Series FX (Daily)"]]
  if (is.null(ts) || length(ts) == 0L) return(data.frame())
  dates_char <- names(ts)
  if (is.null(dates_char) || length(dates_char) != length(ts)) return(data.frame())
  n <- length(dates_char)
  out_list <- vector("list", n)
  for (i in seq_len(n)) {
    d <- ts[[i]]
    out_list[[i]] <- data.frame(
      date = dates_char[i],
      open = as.numeric(d[["1. open"]] %||% NA),
      high = as.numeric(d[["2. high"]] %||% NA),
      low = as.numeric(d[["3. low"]] %||% NA),
      close = as.numeric(d[["4. close"]] %||% NA),
      stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, out_list)
  df$date <- as.Date(df$date, format = "%Y-%m-%d")
  df <- df[order(df$date, decreasing = TRUE), ]
  rownames(df) <- NULL
  df
}

# Realtime/latest exchange rate. CURRENCY_EXCHANGE_RATE is premium-only; free keys fall back to FX_DAILY (forex only).
av_currency_exchange_rate <- function(from_currency = "USD", to_currency = "EUR") {
  from_currency <- toupper(trimws(from_currency))
  to_currency <- toupper(trimws(to_currency))
  # Try premium endpoint first (works with premium API key)
  tryCatch({
    out <- av_get(list(
      `function` = "CURRENCY_EXCHANGE_RATE",
      from_currency = from_currency,
      to_currency = to_currency
    ))
    block <- out[["Realtime Currency Exchange Rate"]]
    if (is.null(block)) stop("No rate in response")
    return(data.frame(
      from_code = as.character(block[["1. From_Currency Code"]] %||% NA),
      from_name = as.character(block[["2. From_Currency Name"]] %||% NA),
      to_code = as.character(block[["3. To_Currency Code"]] %||% NA),
      to_name = as.character(block[["4. To_Currency Name"]] %||% NA),
      exchange_rate = as.numeric(block[["5. Exchange Rate"]] %||% NA),
      last_refreshed = as.character(block[["6. Last Refreshed"]] %||% NA),
      time_zone = as.character(block[["7. Time Zone"]] %||% NA),
      bid_price = as.numeric(block[["8. Bid Price"]] %||% NA),
      ask_price = as.numeric(block[["9. Ask Price"]] %||% NA),
      note = "",
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    msg <- conditionMessage(e)
    # CURRENCY_EXCHANGE_RATE is premium-only; for forex pairs use FX_DAILY (free) as fallback
    if (!grepl("Invalid API call|premium|Premium", msg, ignore.case = TRUE)) stop(e)
    # Fallback: FX_DAILY supports physical currency pairs only (e.g. EUR, USD, GBP, JPY)
    crypto <- c("BTC", "ETH", "XRP", "LTC", "BCH", "ADA", "DOGE", "SOL", "USDT", "USDC")
    is_crypto <- function(c) toupper(c) %in% crypto
    if (is_crypto(from_currency) || is_crypto(to_currency)) {
      stop("CURRENCY_EXCHANGE_RATE (including crypto) requires a premium Alpha Vantage key. Use the 'Forex' section for physical currency pairs (e.g. EUR/USD).")
    }
    fx <- tryCatch(av_fx_daily(from_symbol = from_currency, to_symbol = to_currency), error = function(e2) NULL)
    if (is.null(fx) || nrow(fx) == 0L) {
      stop("This pair is not available with a free key. CURRENCY_EXCHANGE_RATE requires premium. Try the 'Forex' section for pairs like EUR/USD.")
    }
    latest <- fx[1L, ]
    data.frame(
      from_code = from_currency,
      from_name = from_currency,
      to_code = to_currency,
      to_name = to_currency,
      exchange_rate = as.numeric(latest$close),
      last_refreshed = as.character(latest$date),
      time_zone = "",
      bid_price = NA_real_,
      ask_price = NA_real_,
      note = "Latest close from FX_DAILY (free tier; not realtime)",
      stringsAsFactors = FALSE
    )
  })
}

av_commodity <- function(commodity = "WHEAT", interval = "monthly") {
  out <- av_get(list(`function` = commodity, interval = interval))
  nm <- names(out)
  ts_key <- nm[grepl("data|time|series", nm, ignore.case = TRUE)][1]
  if (is.null(ts_key)) ts_key <- nm[!nm %in% c("name", "interval", "unit")][1]
  ts <- out[[ts_key]]
  if (is.null(ts)) return(data.frame())
  if (is.data.frame(ts)) {
    if ("date" %in% names(ts)) ts$date <- as.Date(ts$date)
    if ("value" %in% names(ts)) ts$value <- as.numeric(ts$value)
    return(ts)
  }
  if (is.list(ts) && !is.data.frame(ts) && length(ts) > 0) {
    df <- do.call(rbind, lapply(ts, function(x) data.frame(date = as.character(x$date %||% x$Date %||% NA), value = as.numeric(x$value %||% x$Value %||% NA), stringsAsFactors = FALSE)))
    df$date <- as.Date(df$date)
    return(df[order(df$date, decreasing = TRUE), ])
  }
  df <- data.frame(date = as.Date(rownames(as.data.frame(ts))), value = as.numeric(unlist(as.data.frame(ts)[, 1])), stringsAsFactors = FALSE)
  df[order(df$date, decreasing = TRUE), ]
}

av_economic <- function(indicator = "CPI", interval = "monthly", ...) {
  params <- list(`function` = indicator, ...)
  if (indicator %in% c("CPI", "REAL_GDP", "TREASURY_YIELD", "FEDERAL_FUNDS_RATE") && nzchar(interval)) params$interval <- interval
  out <- av_get(params)
  nm <- names(out)
  ts_key <- nm[grepl("data|time|series", nm, ignore.case = TRUE)][1]
  if (is.null(ts_key)) ts_key <- nm[!nm %in% c("name", "interval", "unit", "information")][1]
  ts <- out[[ts_key]]
  if (is.null(ts)) return(data.frame())
  if (is.data.frame(ts)) {
    if ("date" %in% names(ts)) ts$date <- as.Date(ts$date)
    return(ts)
  }
  if (is.list(ts) && !is.data.frame(ts) && length(ts) > 0) {
    df <- do.call(rbind, lapply(ts, function(x) data.frame(date = as.character(x$date %||% x$Date %||% NA), value = as.numeric(x$value %||% x$Value %||% NA), stringsAsFactors = FALSE)))
    df$date <- as.Date(df$date)
    return(df[order(df$date, decreasing = TRUE), ])
  }
  df <- data.frame(date = as.Date(rownames(as.data.frame(ts))), value = as.numeric(unlist(as.data.frame(ts)[, 1])), stringsAsFactors = FALSE)
  df[order(df$date, decreasing = TRUE), ]
}

app_css <- "
  :root { --bg-main: #f0f4f8; --bg-card: #ffffff; --accent: #0f766e; --text: #1e293b; --text-muted: #64748b; --border: #e2e8f0; --green: #059669; --red: #dc2626; }
  body { font-family: 'Segoe UI', system-ui, sans-serif; background: var(--bg-main); color: var(--text); }
  .app-header { background: linear-gradient(135deg, #0f766e 0%, #0d9488 100%); padding: 0.75rem 1.5rem; margin: -1rem -1rem 1rem -1rem; border-radius: 0 0 12px 12px; box-shadow: 0 4px 14px rgba(15,118,110,.25); }
  .app-header .title { color: #fff; font-weight: 800; font-size: 1.35rem; }
  .app-header .subtitle { color: rgba(255,255,255,.88); font-size: 0.85rem; margin-top: 0.2rem; }
  .sidebar { background: #fff; border-right: 1px solid var(--border); box-shadow: 2px 0 12px rgba(0,0,0,.04); }
  .main-content { padding: 1.5rem 2rem; max-width: 1200px; }
  .card-custom { background: var(--bg-card); border-radius: 12px; padding: 1.25rem; margin-bottom: 1rem; box-shadow: 0 1px 3px rgba(0,0,0,.06); border: 1px solid var(--border); }
  .card-custom h4 { margin: 0 0 0.75rem 0; font-size: 0.9rem; font-weight: 600; color: var(--text-muted); text-transform: uppercase; letter-spacing: 0.03em; }
  .card-custom .value { font-size: 1.75rem; font-weight: 700; color: var(--text); }
  .card-custom .value.positive { color: var(--green); }
  .card-custom .value.negative { color: var(--red); }
  .stats-row { display: flex; flex-wrap: wrap; gap: 1rem; margin-bottom: 1.25rem; }
  .stat-card { flex: 1 1 140px; min-width: 120px; }
  .ticker-card { display: inline-block; background: var(--bg-card); border-radius: 10px; padding: 0.9rem 1.1rem; margin: 0.35rem; border: 1px solid var(--border); box-shadow: 0 1px 2px rgba(0,0,0,.04); }
  .ticker-card .ticker { font-weight: 700; }
  .ticker-card .change-pos { color: var(--green); font-weight: 600; }
  .ticker-card .change-neg { color: var(--red); font-weight: 600; }
  .news-card { background: var(--bg-card); border-radius: 10px; padding: 1rem 1.25rem; margin-bottom: 0.75rem; border-left: 4px solid var(--accent); box-shadow: 0 1px 2px rgba(0,0,0,.05); }
  .news-card a { color: var(--accent); font-weight: 600; text-decoration: none; }
  .sentiment-badge { display: inline-block; padding: 0.2rem 0.5rem; border-radius: 6px; font-size: 0.75rem; font-weight: 600; }
  .sentiment-bullish { background: #d1fae5; color: #065f46; }
  .sentiment-bearish { background: #fee2e2; color: #991b1b; }
  .sentiment-neutral { background: #e2e8f0; color: #475569; }
  .hero-rate { font-size: 2rem; font-weight: 800; color: var(--accent); }
  .plot-container { border-radius: 12px; overflow: hidden; background: var(--bg-card); padding: 1rem; margin-bottom: 1rem; border: 1px solid var(--border); }
  .btn-primary { background: linear-gradient(135deg, #0f766e, #0d9488) !important; border: none !important; font-weight: 600 !important; }
  .api-limit-note { font-size: 0.8rem; color: var(--text-muted); margin-bottom: 1rem; }
  .toolbar { display: flex; flex-wrap: wrap; align-items: stretch; gap: 0 1.25rem; row-gap: 0.75rem; margin-bottom: 1rem; padding: 0.875rem 1rem; background: var(--bg-card); border-radius: 10px; border: 1px solid var(--border); min-height: 48px; }
  .toolbar-item { display: inline-flex; align-items: center; gap: 0.5rem; flex-shrink: 0; min-height: 36px; }
  .toolbar-item-wide select.form-control { min-width: 170px; }
  .toolbar-item .toolbar-label { font-size: 0.875rem; font-weight: 600; color: var(--text-muted); white-space: nowrap; margin: 0; }
  .toolbar .form-group { margin: 0 !important; padding: 0 !important; display: inline-flex !important; align-items: center !important; min-height: 36px; }
  .toolbar .form-group label { margin: 0 0.35rem 0 0 !important; font-size: 0.875rem !important; font-weight: 600 !important; color: var(--text-muted) !important; white-space: nowrap !important; }
  .toolbar select.form-control { height: 36px !important; min-width: 100px; width: auto !important; max-width: 220px; box-sizing: border-box !important; padding-right: 1.75rem !important; }
  .toolbar input[type=\"text\"].form-control { height: 36px !important; width: 130px !important; min-width: 90px; box-sizing: border-box !important; }
  .toolbar .form-control { height: 36px !important; box-sizing: border-box !important; }
  .toolbar-item input[type=\"checkbox\"] { margin: 0 0.35rem 0 0 !important; flex-shrink: 0; }
  .toolbar .btn { height: 36px !important; line-height: 1 !important; display: inline-flex !important; align-items: center !important; padding: 0 1rem !important; box-sizing: border-box !important; flex-shrink: 0; white-space: nowrap; }
  .toolbar .shiny-download-link { display: inline-flex !important; align-items: center !important; }
  .btn-default { border: 1px solid var(--border) !important; background: #fff !important; color: var(--text) !important; }
  @media (max-width: 640px) { .toolbar { flex-direction: column; align-items: flex-start; } .toolbar-item { width: 100%; max-width: 100%; } .toolbar-item select.form-control { max-width: 100%; } }
  .ai-reporter-card { background: linear-gradient(145deg, #fff 0%, #f0fdfa 100%); border: 2px solid var(--accent); border-radius: 16px; padding: 2rem; margin-bottom: 1.5rem; box-shadow: 0 4px 20px rgba(15,118,110,.12); }
  .ai-reporter-card .ai-title { font-size: 1.5rem; font-weight: 800; color: var(--accent); margin-bottom: 0.5rem; }
  .ai-reporter-connect { background: #f8fafc; border: 2px dashed var(--border); border-radius: 12px; padding: 1.5rem; margin-top: 1rem; }
  .ai-report-output { white-space: pre-wrap; font-size: 0.9rem; line-height: 1.6; padding: 1rem; background: #f8fafc; border-radius: 8px; border-left: 4px solid var(--accent); }
  .pulse { animation: pulse 1.5s ease-in-out infinite; }
  @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.6; } }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  titlePanel(windowTitle = "Daily Market", title = div(class = "app-header", div(class = "title", "Daily Market Analysis"), div(class = "subtitle", "Stocks · Forex · News · AI Insights"))),
  sidebarLayout(
    sidebarPanel(class = "sidebar", width = 3,
      selectInput("section", "Section",
        choices = c("Stock Daily" = "stock", "Top Gainers/Losers" = "gainers", "News & Sentiment" = "news", "Forex" = "forex",
          "Commodities" = "commodity", "Economic Indicators" = "economic"),
        selected = "stock"),
      conditionalPanel("input.section == 'stock'", textInput("stock_symbol", "Symbol", value = "AAPL", placeholder = "e.g. AAPL, MSFT"), checkboxInput("stock_full_history", "Full history (for 1-year trend)", value = FALSE), actionButton("fetch_stock", "Fetch", class = "btn-primary")),
      conditionalPanel("input.section == 'gainers'", actionButton("fetch_gainers", "Fetch movers", class = "btn-primary")),
      conditionalPanel("input.section == 'news'", textInput("news_tickers", "Tickers (optional)", placeholder = "AAPL, MSFT"), numericInput("news_limit", "Limit", value = 20, min = 1, max = 50), actionButton("fetch_news", "Fetch news", class = "btn-primary")),
      conditionalPanel("input.section == 'forex'", textInput("fx_from", "From", value = "EUR"), textInput("fx_to", "To", value = "USD"), actionButton("fetch_fx", "Fetch", class = "btn-primary")),
      conditionalPanel("input.section == 'commodity'", selectInput("commodity", "Commodity", choices = c("WHEAT" = "WHEAT", "CORN" = "CORN", "WTI" = "WTI", "BRENT" = "BRENT", "NATURAL_GAS" = "NATURAL_GAS", "COPPER" = "COPPER", "COFFEE" = "COFFEE")), selectInput("commodity_interval", "Interval", choices = c("daily", "weekly", "monthly"), selected = "monthly"), actionButton("fetch_commodity", "Fetch", class = "btn-primary")),
      conditionalPanel("input.section == 'economic'", selectInput("economic_indicator", "Indicator", choices = c("CPI" = "CPI", "INFLATION" = "INFLATION", "UNEMPLOYMENT" = "UNEMPLOYMENT", "FEDERAL_FUNDS_RATE" = "FEDERAL_FUNDS_RATE", "TREASURY_YIELD" = "TREASURY_YIELD", "REAL_GDP" = "REAL_GDP", "RETAIL_SALES" = "RETAIL_SALES", "NONFARM_PAYROLL" = "NONFARM_PAYROLL")), selectInput("economic_interval", "Interval", choices = c("monthly", "quarterly", "annual", "daily"), selected = "monthly"), actionButton("fetch_economic", "Fetch", class = "btn-primary")),
      hr(), checkboxInput("show_raw_data", "Show data tables", value = FALSE)
    ),
    mainPanel(class = "main-content", width = 9,
      fluidRow(
        column(
          width = 8,
          uiOutput("section_title"),
          p("Select a section and click Fetch to load data. View, filter, and Download use already-loaded data (no extra API calls).", class = "api-limit-note"),
          uiOutput("api_error_ui"),
          conditionalPanel(
            "input.section == 'stock'",
            uiOutput("ui_stock_toolbar"),
            uiOutput("ui_stock_summary"),
            div(class = "plot-container", plotOutput("plot_stock")),
            conditionalPanel("input.show_raw_data", DT::dataTableOutput("table_stock"))
          ),
          conditionalPanel(
            "input.section == 'gainers'",
            uiOutput("ui_gainers_toolbar"),
            uiOutput("ui_gainers_cards"),
            uiOutput("ui_gainers_chart"),
            conditionalPanel(
              "input.show_raw_data",
              h4("Top Gainers"), DT::dataTableOutput("table_gainers"),
              h4("Top Losers"), DT::dataTableOutput("table_losers"),
              h4("Most Active"), DT::dataTableOutput("table_active")
            )
          ),
          conditionalPanel(
            "input.section == 'news'",
            uiOutput("ui_news_toolbar"),
            uiOutput("ui_news_cards"),
            conditionalPanel("input.show_raw_data", DT::dataTableOutput("table_news"))
          ),
          conditionalPanel(
            "input.section == 'forex'",
            uiOutput("ui_fx_toolbar"),
            uiOutput("ui_fx_summary"),
            div(class = "plot-container", plotOutput("plot_fx")),
            conditionalPanel("input.show_raw_data", DT::dataTableOutput("table_fx"))
          ),
          conditionalPanel(
            "input.section == 'commodity'",
            uiOutput("ui_commodity_toolbar"),
            uiOutput("ui_commodity_summary"),
            div(class = "plot-container", plotOutput("plot_commodity")),
            conditionalPanel("input.show_raw_data", DT::dataTableOutput("table_commodity"))
          ),
          conditionalPanel(
            "input.section == 'economic'",
            uiOutput("ui_economic_toolbar"),
            uiOutput("ui_economic_summary"),
            div(class = "plot-container", plotOutput("plot_economic")),
            conditionalPanel("input.show_raw_data", DT::dataTableOutput("table_economic"))
          )
        ),
        column(
          width = 4,
          uiOutput("ui_ai_reporter")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  err_msg <- reactiveVal(NULL)
  stock_df <- reactiveVal(NULL)
  gainers_data <- reactiveVal(NULL)
  news_df <- reactiveVal(NULL)
  fx_df <- reactiveVal(NULL)
  currency_rate_df <- reactiveVal(NULL)
  commodity_df <- reactiveVal(NULL)
  economic_df <- reactiveVal(NULL)
  ai_report_text <- reactiveVal(NULL)
  ai_loading <- reactiveVal(FALSE)
  ai_effective_key <- reactive(OLLAMA_CLOUD_API_KEY)

  clear_err <- function() err_msg(NULL)
  set_err <- function(e) err_msg(paste0("Error: ", conditionMessage(e)))

  output$section_title <- renderUI({
    titles <- c(
      stock = "Stock Daily",
      gainers = "Top Gainers/Losers",
      news = "News & Sentiment",
      forex = "Forex",
      commodity = "Commodities",
      economic = "Economic Indicators"
    )
    tit <- titles[input$section]
    if (is.null(tit) || length(tit) == 0L || (length(tit) == 1L && is.na(tit))) tit <- input$section
    h3(tit, style = "margin-top: 0; color: var(--text);")
  })
  output$api_error_ui <- renderUI({
    err <- err_msg()
    if (is.null(err) || length(err) == 0L || !nzchar(err)) return(NULL)
    div(class = "card-custom", style = "border-left: 4px solid #dc2626; background: #fef2f2;", p(style = "margin: 0; color: #991b1b; font-weight: 600;", err))
  })

  # ---- Stock Daily ----
  observeEvent(input$fetch_stock, {
    clear_err()
    tryCatch({
      sym <- trimws(input$stock_symbol %||% "")
      if (length(sym) == 0L || !nzchar(sym)) { set_err(simpleError("Please enter a symbol.")); return() }
      outsize <- if (isTRUE(input$stock_full_history)) "full" else "compact"
      df <- av_stock_daily(sym, outputsize = outsize)
      if (is.null(df) || nrow(df) == 0) {
        set_err(simpleError("No data returned. Check API key, symbol, or rate limit (5/min, 25/day)."))
        stock_df(NULL)
      } else {
        stock_df(df)
      }
    }, error = set_err)
  })
  output$ui_stock_toolbar <- renderUI({
    if (is.null(stock_df()) || nrow(stock_df()) == 0) return(NULL)
    div(class = "toolbar",
        div(class = "toolbar-item toolbar-item-wide", span(class = "toolbar-label", "View"), selectInput("stock_days", NULL, choices = c("Last 5 days" = 5, "Last 21 days" = 21, "Last 60 days" = 60, "Last 90 days" = 90), selected = 90)),
        div(class = "toolbar-item", downloadButton("download_stock_csv", "Download CSV", class = "btn btn-default")))
  })
  output$ui_stock_summary <- renderUI({
    df <- stock_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    days <- as.integer(input$stock_days %||% 90)
    df <- head(df, days)
    r <- df[1L, ]
    prev <- if (nrow(df) >= 2L) df$close[2L] else r$close
    chg <- if (is.finite(prev) && prev != 0) round(100 * (r$close - prev) / prev, 2) else NA
    cl_class <- if (!is.na(chg)) if (chg >= 0) "value positive" else "value negative" else "value"
    chg_txt <- if (!is.na(chg)) paste0(if (chg >= 0) "+" else "", chg, "%") else "—"
    tagList(div(class = "stats-row",
      div(class = "stat-card card-custom", h4("Last close"), div(class = "value", sprintf("$%.2f", r$close))),
      div(class = "stat-card card-custom", h4("Change (1d)"), div(class = cl_class, chg_txt)),
      div(class = "stat-card card-custom", h4("High"), div(class = "value", sprintf("$%.2f", r$high %||% NA))),
      div(class = "stat-card card-custom", h4("Low"), div(class = "value", sprintf("$%.2f", r$low %||% NA))),
      div(class = "stat-card card-custom", h4("Volume"), div(class = "value", format(as.numeric(r$volume %||% 0), big.mark = ",")))))
  })
  output$plot_stock <- renderPlot({
    df <- stock_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    days <- as.integer(input$stock_days %||% 90)
    df <- head(df, days)
    df <- df[!is.na(df$date) & is.finite(as.numeric(df$close)), ]
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = date, y = close)) + geom_line(color = "#0d9488", linewidth = 1) + geom_point(color = "#0d9488", size = 1.5) + labs(x = "Date", y = "Close", title = NULL) + theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
  })
  output$table_stock <- renderDT({
    df <- stock_df()
    if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    DT::datatable(as.data.frame(df), options = list(pageLength = 15))
  })
  output$download_stock_csv <- downloadHandler(filename = function() paste0("stock_", input$stock_symbol %||% "symbol", "_", Sys.Date(), ".csv"), content = function(file) { df <- stock_df(); if (!is.null(df) && nrow(df) > 0) write.csv(df, file, row.names = FALSE) })

  # ---- Top Gainers / Losers ----
  observeEvent(input$fetch_gainers, {
    clear_err()
    tryCatch({
      d <- av_top_gainers_losers()
      ng <- nrow(d$top_gainers %||% data.frame())
      if (ng == 0) {
        set_err(simpleError("No data returned. Check API key or rate limit (5/min, 25/day)."))
        gainers_data(NULL)
      } else {
        gainers_data(d)
      }
    }, error = set_err)
  })
  output$ui_gainers_toolbar <- renderUI({
    d <- gainers_data(); if (is.null(d)) return(NULL)
    div(class = "toolbar",
        div(class = "toolbar-item", span(class = "toolbar-label", "Show"), selectInput("gainers_n", NULL, choices = c(5, 10, 20), selected = 20)),
        div(class = "toolbar-item", span(class = "toolbar-label", "Filter"), textInput("gainers_filter", NULL, placeholder = "e.g. AAPL")),
        div(class = "toolbar-item", checkboxInput("gainers_chart", "Bar chart", value = FALSE)),
        div(class = "toolbar-item", downloadButton("download_gainers_csv", "Download CSV", class = "btn btn-default")))
  })
  gainers_filtered <- reactive({
    d <- gainers_data(); if (is.null(d)) return(d)
    flt <- trimws(input$gainers_filter %||% ""); n_show <- as.integer(input$gainers_n %||% 20)
    nn <- function(nms) tolower(gsub("[^a-z0-9]", "", nms))
    filter_df <- function(df) {
      if (!is.data.frame(df) || nrow(df) == 0) return(df)
      if (!nzchar(flt)) return(head(df, n_show))
      tk <- names(df)[match("ticker", nn(names(df)))]; if (is.na(tk)) tk <- names(df)[1]
      head(df[grepl(flt, as.character(df[[tk]]), ignore.case = TRUE), ], n_show)
    }
    list(top_gainers = filter_df(d$top_gainers), top_losers = filter_df(d$top_losers), top_activated = filter_df(d$top_activated))
  })
  output$ui_gainers_cards <- renderUI({
    d <- gainers_filtered(); if (is.null(d)) return(p("Click 'Fetch movers' to load data.", style = "color: var(--text-muted);"))
    nn <- function(nms) tolower(gsub("[^a-z0-9]", "", nms))
    ticker_col <- function(df, name) {
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
      nms <- names(df); nml <- nn(nms)
      tk <- nms[match("ticker", nml)]; if (is.na(tk)) tk <- nms[1]
      pr <- nms[match("price", nml)]; if (is.na(pr)) pr <- nms[min(2, length(nms))]
      ch <- nms[which(nml %in% c("changepercentage", "change_percentage", "changepercent"))[1]]
      vol <- nms[which(nml == "volume")[1]]
      items <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]; tkr <- as.character(row[[tk]])
        prc <- tryCatch(if (is.numeric(row[[pr]])) sprintf("%.2f", row[[pr]]) else as.character(row[[pr]]), error = function(e) "—")
        chg <- if (!is.null(ch) && ch %in% nms) as.character(row[[ch]]) else ""
        vol_txt <- if (!is.null(vol) && vol %in% nms) tryCatch(format(as.numeric(row[[vol]]), big.mark = ","), error = function(e) NULL) else NULL
        is_neg <- nzchar(chg) && grepl("-", chg); cl <- if (name == "Top Losers" || is_neg) "change-neg" else "change-pos"
        div(class = "ticker-card", div(style = "font-size: 0.75rem; font-weight: 700; color: var(--text-muted);", paste0("#", i)), div(class = "ticker", tkr), div(style = "font-size: 0.9rem; color: var(--text-muted);", prc), if (nzchar(chg)) div(class = cl, chg) else NULL, if (!is.null(vol_txt)) div(style = "font-size: 0.75rem; color: var(--text-muted);", paste0("Vol: ", vol_txt)) else NULL)
      })
      tagList(h4(name), div(style = "display: flex; flex-wrap: wrap; gap: 0.35rem;", items))
    }
    tagList(div(class = "card-custom", ticker_col(d$top_gainers, "Top Gainers")), div(class = "card-custom", ticker_col(d$top_losers, "Top Losers")), div(class = "card-custom", ticker_col(d$top_activated, "Most Actively Traded")))
  })
  output$ui_gainers_chart <- renderUI({
    if (!isTRUE(input$gainers_chart)) return(NULL)
    d <- gainers_filtered(); if (is.null(d) || !is.data.frame(d$top_gainers) || nrow(d$top_gainers) == 0) return(NULL)
    div(class = "plot-container", plotOutput("plot_gainers_bars"))
  })
  output$plot_gainers_bars <- renderPlot({
    if (!isTRUE(input$gainers_chart)) return(NULL)
    d <- gainers_filtered(); if (is.null(d) || !is.data.frame(d$top_gainers) || nrow(d$top_gainers) == 0) return(NULL)
    df <- d$top_gainers; nml <- tolower(gsub("[^a-z0-9]", "", names(df)))
    tk <- names(df)[match("ticker", nml)]; ch <- names(df)[which(nml %in% c("changepercentage", "change_percentage", "changepercent"))[1]]
    if (is.na(tk)) tk <- names(df)[1]; if (is.na(ch)) return(NULL)
    df$ticker_ <- df[[tk]]; df$change_ <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(df[[ch]])))); df <- df[is.finite(df$change_), ]
    if (nrow(df) == 0) return(NULL); df <- head(df, 15)
    ggplot(df, aes(x = reorder(ticker_, change_), y = change_, fill = change_ > 0)) + geom_col() + scale_fill_manual(values = c("FALSE" = "#dc2626", "TRUE" = "#059669"), guide = "none") + coord_flip() + labs(x = NULL, y = "Change %", title = "Top gainers") + theme_minimal(base_size = 12)
  })
  output$download_gainers_csv <- downloadHandler(filename = function() paste0("gainers_losers_", Sys.Date(), ".csv"), content = function(file) { d <- gainers_data(); if (!is.null(d)) { combined <- dplyr::bind_rows(dplyr::mutate(d$top_gainers, group = "gainers"), dplyr::mutate(d$top_losers, group = "losers"), dplyr::mutate(d$top_activated, group = "active")); if (nrow(combined) > 0) write.csv(combined, file, row.names = FALSE) } })
  output$table_gainers <- renderDT({ d <- gainers_data(); if (is.null(d) || !is.data.frame(d$top_gainers) || nrow(d$top_gainers) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 10))); DT::datatable(as.data.frame(d$top_gainers), options = list(pageLength = 10)) })
  output$table_losers <- renderDT({ d <- gainers_data(); if (is.null(d) || !is.data.frame(d$top_losers) || nrow(d$top_losers) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 10))); DT::datatable(as.data.frame(d$top_losers), options = list(pageLength = 10)) })
  output$table_active <- renderDT({ d <- gainers_data(); if (is.null(d) || !is.data.frame(d$top_activated) || nrow(d$top_activated) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 10))); DT::datatable(as.data.frame(d$top_activated), options = list(pageLength = 10)) })

  # ---- News ----
  observeEvent(input$fetch_news, {
    clear_err()
    tryCatch({
      df <- av_news_sentiment(tickers = trimws(input$news_tickers), limit = as.integer(input$news_limit))
      if (is.null(df) || nrow(df) == 0) {
        set_err(simpleError("No news returned. Check API key or rate limit (5/min, 25/day)."))
        news_df(NULL)
      } else {
        news_df(df)
      }
    }, error = set_err)
  })
  output$ui_news_toolbar <- renderUI({
    df <- news_df(); if (is.null(df) || nrow(df) == 0) return(NULL)
    div(class = "toolbar",
        div(class = "toolbar-item", span(class = "toolbar-label", "Show"), selectInput("news_n", NULL, choices = c(10, 25, 50), selected = 50)),
        div(class = "toolbar-item", span(class = "toolbar-label", "Keyword"), textInput("news_keyword", NULL, placeholder = "e.g. Fed")),
        div(class = "toolbar-item", span(class = "toolbar-label", "Sentiment"), selectInput("news_sentiment_filter", NULL, choices = c("All", "Bullish", "Bearish", "Neutral"), selected = "All")),
        div(class = "toolbar-item", downloadButton("download_news_csv", "Download CSV", class = "btn btn-default")))
  })
  news_filtered <- reactive({
    df <- news_df(); if (is.null(df) || nrow(df) == 0) return(df)
    kw <- trimws(input$news_keyword %||% ""); sent <- input$news_sentiment_filter %||% "All"; n_show <- as.integer(input$news_n %||% 50)
    if (nzchar(kw)) df <- df[grepl(kw, as.character(df$title), ignore.case = TRUE) | grepl(kw, as.character(df$summary %||% ""), ignore.case = TRUE), ]
    if (sent != "All") { lbl <- tolower(as.character(df$overall_sentiment_label %||% "")); idx <- switch(sent, Bullish = grepl("bullish|positive", lbl), Bearish = grepl("bearish|negative", lbl), Neutral = grepl("neutral", lbl), rep(TRUE, nrow(df))); df <- df[idx, ] }
    head(df, n_show)
  })
  output$ui_news_cards <- renderUI({
    df <- news_filtered(); if (is.null(df) || nrow(df) == 0) return(p("Click 'Fetch news' or loosen filters.", style = "color: var(--text-muted);"))
    sent_class <- function(lbl) { if (is.na(lbl) || !nzchar(lbl)) return("sentiment-neutral"); lbl <- tolower(as.character(lbl)); if (grepl("bullish|positive", lbl)) return("sentiment-bullish"); if (grepl("bearish|negative", lbl)) return("sentiment-bearish"); "sentiment-neutral" }
    cards <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]; title <- as.character(r$title %||% "—"); url <- as.character(r$url %||% ""); src <- as.character(r$source %||% ""); time <- as.character(r$time_published %||% ""); if (nzchar(time) && nchar(time) >= 8) time <- substr(time, 1, 10); sent <- as.character(r$overall_sentiment_label %||% "")
      div(class = "news-card", if (nzchar(url)) tags$a(href = url, target = "_blank", rel = "noopener", title) else p(style = "margin: 0 0 0.5rem 0; font-weight: 600;", title), div(style = "font-size: 0.85rem; color: var(--text-muted);", if (nzchar(src)) span(paste0(" ", src)), if (nzchar(time)) span(paste0(" · ", time)), if (nzchar(sent)) span(class = paste("sentiment-badge", sent_class(sent)), style = "margin-left: 0.5rem;", sent)))
    })
    tagList(cards)
  })
  output$download_news_csv <- downloadHandler(filename = function() paste0("news_", Sys.Date(), ".csv"), content = function(file) { df <- news_df(); if (!is.null(df) && nrow(df) > 0) write.csv(df, file, row.names = FALSE) })
  output$table_news <- renderDT({
    df <- news_df(); if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    cols <- intersect(c("title", "time_published", "overall_sentiment_label", "source", "url"), names(df)); if (length(cols) == 0) cols <- names(df)
    DT::datatable(as.data.frame(df)[, cols], options = list(pageLength = 15), escape = FALSE)
  })

  # ---- Forex ----
  observeEvent(input$fetch_fx, {
    clear_err()
    tryCatch({
      from <- trimws(input$fx_from %||% ""); to <- trimws(input$fx_to %||% "")
      if (!nzchar(from) || !nzchar(to)) { set_err(simpleError("Enter From and To currencies.")); return() }
      df <- av_fx_daily(from_symbol = from, to_symbol = to)
      if (is.null(df) || nrow(df) == 0) {
        set_err(simpleError("No FX data returned. Check API key or rate limit (5/min, 25/day)."))
        fx_df(NULL)
      } else {
        fx_df(df)
      }
    }, error = set_err)
  })
  output$ui_fx_toolbar <- renderUI({
    if (is.null(fx_df()) || nrow(fx_df()) == 0) return(NULL)
    div(class = "toolbar",
        div(class = "toolbar-item", span(class = "toolbar-label", "Chart points"), selectInput("fx_points", NULL, choices = c(30, 60, 90), selected = 90)),
        div(class = "toolbar-item", downloadButton("download_fx_csv", "Download CSV", class = "btn btn-default")))
  })
  output$ui_fx_summary <- renderUI({
    df <- fx_df(); if (is.null(df) || nrow(df) == 0) return(NULL); r <- df[1L, ]; rate <- as.numeric(r$close); if (!is.finite(rate)) return(NULL)
    div(class = "stats-row", div(class = "stat-card card-custom", h4("Latest rate"), div(class = "hero-rate", sprintf("%.4f", rate))), div(class = "stat-card card-custom", h4("Low (period)"), div(class = "value", sprintf("%.4f", min(df$close, na.rm = TRUE)))), div(class = "stat-card card-custom", h4("High (period)"), div(class = "value", sprintf("%.4f", max(df$close, na.rm = TRUE)))), div(class = "stat-card card-custom", h4("Days shown"), div(class = "value", as.character(nrow(df)))))
  })
  output$plot_fx <- renderPlot({
    df <- fx_df(); if (is.null(df) || nrow(df) == 0) return(NULL); pts <- as.integer(input$fx_points %||% 90); df <- head(df, pts)
    df <- df[!is.na(df$date) & is.finite(as.numeric(df$close)), ]; if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = date, y = close)) + geom_line(color = "#0d9488", linewidth = 1) + labs(x = "Date", y = "Close", title = NULL) + theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
  })
  output$table_fx <- renderDT({ df <- fx_df(); if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15))); DT::datatable(as.data.frame(df), options = list(pageLength = 15)) })
  output$download_fx_csv <- downloadHandler(filename = function() paste0("fx_", input$fx_from %||% "from", "_", input$fx_to %||% "to", "_", Sys.Date(), ".csv"), content = function(file) { df <- fx_df(); if (!is.null(df) && nrow(df) > 0) write.csv(df, file, row.names = FALSE) })

  # ---- Currency Exchange Rate (realtime: crypto or physical) ----
  observeEvent(input$fetch_currency_rate, {
    clear_err()
    tryCatch({
      from <- trimws(input$curr_from %||% "")
      to <- trimws(input$curr_to %||% "")
      if (length(from) == 0L || !nzchar(from) || length(to) == 0L || !nzchar(to)) { set_err(simpleError("Please enter From and To currency codes (e.g. USD, BTC).")); return() }
      currency_rate_df(av_currency_exchange_rate(from_currency = from, to_currency = to))
    }, error = set_err)
  })
  output$ui_currency_rate <- renderUI({
    df <- currency_rate_df(); if (is.null(df) || nrow(df) == 0) return(p("Click 'Get rate' to fetch the exchange rate.", style = "color: var(--text-muted);"))
    r <- df[1, ]; rate <- if (is.numeric(r$exchange_rate)) format(r$exchange_rate, digits = 8, scientific = FALSE) else as.character(r$exchange_rate)
    div(class = "card-custom", style = "text-align: center; padding: 2rem;", p(style = "margin: 0 0 0.5rem 0; font-size: 0.95rem; color: var(--text-muted);", paste(r$from_name %||% r$from_code, "→", r$to_name %||% r$to_code)), p(style = "margin: 0;", class = "hero-rate", paste0("1 ", r$from_code, " = ", rate, " ", r$to_code)), if (nzchar(r$last_refreshed %||% "")) p(style = "margin: 0.75rem 0 0 0; font-size: 0.85rem; color: var(--text-muted);", paste("Last updated:", r$last_refreshed)), if (nzchar(r$note %||% "")) p(style = "margin: 0.35rem 0 0 0; font-size: 0.8rem; color: var(--text-muted); font-style: italic;", r$note))
  })
  output$table_currency_rate <- renderDT({
    df <- currency_rate_df()
    if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 10)))
    DT::datatable(as.data.frame(df), options = list(pageLength = 10))
  })

  # ---- Commodity ----
  observeEvent(input$fetch_commodity, { clear_err(); tryCatch({ commodity_df(av_commodity(commodity = input$commodity, interval = input$commodity_interval)) }, error = set_err) })
  output$ui_commodity_toolbar <- renderUI({
    if (is.null(commodity_df()) || nrow(commodity_df()) == 0) return(NULL)
    div(class = "toolbar",
        div(class = "toolbar-item", span(class = "toolbar-label", "Chart points"), selectInput("commodity_points", NULL, choices = c(30, 60, 90), selected = 60)),
        div(class = "toolbar-item", downloadButton("download_commodity_csv", "Download CSV", class = "btn btn-default")))
  })
  output$ui_commodity_summary <- renderUI({
    df <- commodity_df(); if (is.null(df) || nrow(df) == 0) return(NULL); ycol <- if ("value" %in% names(df)) "value" else names(df)[2]; vv <- as.numeric(df[[ycol]]); vv <- vv[is.finite(vv)]; if (length(vv) == 0) return(NULL)
    div(class = "stats-row", div(class = "stat-card card-custom", h4(paste(input$commodity, "— Latest")), div(class = "value", format(vv[1], big.mark = ","))), div(class = "stat-card card-custom", h4("Period low"), div(class = "value", format(min(vv), big.mark = ","))), div(class = "stat-card card-custom", h4("Period high"), div(class = "value", format(max(vv), big.mark = ","))), div(class = "stat-card card-custom", h4("Data points"), div(class = "value", as.character(nrow(df)))))
  })
  output$plot_commodity <- renderPlot({
    df <- commodity_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    pts <- as.integer(input$commodity_points %||% 60); df <- head(df, pts)
    xcol <- if ("date" %in% names(df)) "date" else names(df)[1]
    ycol <- if ("value" %in% names(df)) "value" else names(df)[2]
    df[[ycol]] <- as.numeric(df[[ycol]])
    df <- df[!is.na(df[[xcol]]) & is.finite(df[[ycol]]), ]
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = .data[[xcol]], y = .data[[ycol]])) + geom_line(color = "#0d9488", linewidth = 1) + labs(x = xcol, y = ycol, title = NULL) + theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
  })
  output$table_commodity <- renderDT({ df <- commodity_df(); if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15))); DT::datatable(as.data.frame(df), options = list(pageLength = 15)) })
  output$download_commodity_csv <- downloadHandler(filename = function() paste0("commodity_", input$commodity %||% "", "_", Sys.Date(), ".csv"), content = function(file) { df <- commodity_df(); if (!is.null(df) && nrow(df) > 0) write.csv(df, file, row.names = FALSE) })

  # ---- Economic ----
  observeEvent(input$fetch_economic, {
    clear_err()
    tryCatch({
      ind <- input$economic_indicator
      int <- input$economic_interval
      if (ind == "TREASURY_YIELD") {
        economic_df(av_economic(indicator = ind, interval = int, maturity = "10year"))
      } else if (ind == "REAL_GDP") {
        economic_df(av_economic(indicator = ind, interval = if (int %in% c("quarterly", "annual")) int else "annual"))
      } else {
        economic_df(av_economic(indicator = ind, interval = int))
      }
    }, error = set_err)
  })
  output$ui_economic_toolbar <- renderUI({
    if (is.null(economic_df()) || nrow(economic_df()) == 0) return(NULL)
    div(class = "toolbar",
        div(class = "toolbar-item", span(class = "toolbar-label", "Chart points"), selectInput("economic_points", NULL, choices = c(30, 60, 90), selected = 60)),
        div(class = "toolbar-item", downloadButton("download_economic_csv", "Download CSV", class = "btn btn-default")))
  })
  output$ui_economic_summary <- renderUI({
    df <- economic_df(); if (is.null(df) || nrow(df) == 0) return(NULL); ycol <- if ("value" %in% names(df)) "value" else names(df)[2]; vv <- as.numeric(df[[ycol]]); vv <- vv[is.finite(vv)]; if (length(vv) == 0) return(NULL)
    div(class = "stats-row", div(class = "stat-card card-custom", h4(paste(input$economic_indicator, "— Latest")), div(class = "value", format(vv[1], big.mark = ","))), div(class = "stat-card card-custom", h4("Period min"), div(class = "value", format(min(vv), big.mark = ","))), div(class = "stat-card card-custom", h4("Period max"), div(class = "value", format(max(vv), big.mark = ","))), div(class = "stat-card card-custom", h4("Observations"), div(class = "value", as.character(nrow(df)))))
  })
  output$plot_economic <- renderPlot({
    df <- economic_df(); if (is.null(df) || nrow(df) == 0) return(NULL); pts <- as.integer(input$economic_points %||% 60); df <- head(df, pts)
    xcol <- if ("date" %in% names(df)) "date" else names(df)[1]; ycol <- if ("value" %in% names(df)) "value" else names(df)[2]; df[[ycol]] <- as.numeric(df[[ycol]]); df <- df[!is.na(df[[xcol]]) & is.finite(df[[ycol]]), ]; if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = .data[[xcol]], y = .data[[ycol]])) + geom_line(color = "#0d9488", linewidth = 1) + labs(x = xcol, y = ycol, title = NULL) + theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
  })
  output$table_economic <- renderDT({ df <- economic_df(); if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15))); DT::datatable(as.data.frame(df), options = list(pageLength = 15)) })
  output$download_economic_csv <- downloadHandler(filename = function() paste0("economic_", input$economic_indicator %||% "", "_", Sys.Date(), ".csv"), content = function(file) { df <- economic_df(); if (!is.null(df) && nrow(df) > 0) write.csv(df, file, row.names = FALSE) })

  # ---- AI Reporter ----
  report_choices <- reactive({
    switch(
      input$section,
      stock = c("Brief on current data" = "brief", "Stock snapshot" = "stock", "Stock trend (day/week/month/year)" = "stock_trend"),
      gainers = c("Top movers insight" = "movers"),
      news = c("News briefing" = "news"),
      forex = c("Brief on current forex data" = "forex_brief", "Forex snapshot" = "forex_snapshot", "Forex trend (day/week/month/year)" = "forex_trend"),
      commodity = c("Brief on current commodity data" = "commodity_brief", "Commodity snapshot" = "commodity_snapshot", "Commodity trend (day/week/month/year)" = "commodity_trend"),
      economic = c("Brief on indicator data" = "economic_brief", "Indicator snapshot" = "economic_snapshot", "Indicator trend (day/week/month/year)" = "economic_trend"),
      c("Brief on current data" = "brief")
    )
  })
  build_ai_context <- function(report_type) {
    parts <- character(0)
    if (report_type %in% c("movers", "brief")) { d <- gainers_data(); if (!is.null(d) && is.data.frame(d$top_gainers) && nrow(d$top_gainers) > 0) { g <- head(d$top_gainers, 5); nn <- tolower(gsub("[^a-z0-9]", "", names(g))); tk <- names(g)[match("ticker", nn)]; if (is.na(tk)) tk <- names(g)[1]; parts <- c(parts, paste0("Top gainers (sample): ", paste(g[[tk]], collapse = ", "))) }; if (!is.null(d) && is.data.frame(d$top_losers) && nrow(d$top_losers) > 0) { L <- head(d$top_losers, 5); nn <- tolower(gsub("[^a-z0-9]", "", names(L))); tk <- names(L)[match("ticker", nn)]; if (is.na(tk)) tk <- names(L)[1]; parts <- c(parts, paste0("Top losers (sample): ", paste(L[[tk]], collapse = ", "))) } }
    if (report_type %in% c("news", "brief")) { nw <- news_df(); if (!is.null(nw) && nrow(nw) > 0) parts <- c(parts, paste0("Recent headlines: ", paste(head(as.character(nw$title), 8), collapse = " | "))) }
    if (report_type %in% c("stock", "brief")) { st <- stock_df(); if (!is.null(st) && nrow(st) > 0) parts <- c(parts, sprintf("Latest stock: close %.2f, high %.2f, low %.2f", st$close[1], st$high[1] %||% NA, st$low[1] %||% NA)) }
    if (report_type %in% c("forex_brief", "forex_snapshot")) { fx <- fx_df(); if (!is.null(fx) && nrow(fx) > 0) parts <- c(parts, sprintf("Forex %s/%s: latest close %.4f, high %.4f, low %.4f (over %d days)", input$fx_from %||% "From", input$fx_to %||% "To", as.numeric(fx$close[1]), max(as.numeric(fx$close), na.rm = TRUE), min(as.numeric(fx$close), na.rm = TRUE), nrow(fx))) }
    if (report_type %in% c("commodity_brief", "commodity_snapshot")) { co <- commodity_df(); if (!is.null(co) && nrow(co) > 0) { ycol <- if ("value" %in% names(co)) "value" else names(co)[2]; v <- as.numeric(co[[ycol]]); v <- v[is.finite(v)]; if (length(v) > 0) parts <- c(parts, sprintf("Commodity %s: latest %.2f, period min %.2f, max %.2f (%d points)", input$commodity %||% "", v[1], min(v), max(v), nrow(co))) } }
    if (report_type %in% c("economic_brief", "economic_snapshot")) { ec <- economic_df(); if (!is.null(ec) && nrow(ec) > 0) { ycol <- if ("value" %in% names(ec)) "value" else names(ec)[2]; v <- as.numeric(ec[[ycol]]); v <- v[is.finite(v)]; if (length(v) > 0) parts <- c(parts, sprintf("Indicator %s: latest %.4f, period min %.4f, max %.4f (%d points)", input$economic_indicator %||% "", v[1], min(v), max(v), nrow(ec))) } }
    if (length(parts) == 0) return("No data for this section yet. Fetch data in the current section first."); paste(parts, collapse = "\n\n")
  }
  build_stock_trend_report <- function() {
    st <- stock_df(); if (is.null(st) || nrow(st) == 0) return("Fetch stock data first (Stock Daily section).")
    sym <- input$stock_symbol %||% "Stock"; current <- as.numeric(st$close[1]); if (!is.finite(current)) return("No valid price data.")
    lines <- c(paste0("Stock trend: ", sym), paste0("Current close: $", sprintf("%.2f", current)), "")
    fmt_chg <- function(pct) if (pct >= 0) paste0("+", sprintf("%.2f", pct), "%") else sprintf("%.2f", pct)
    if (nrow(st) >= 2L) { prev <- as.numeric(st$close[2]); if (is.finite(prev) && prev != 0) lines <- c(lines, paste0("Since yesterday:  ", fmt_chg(100 * (current - prev) / prev), " (was $", sprintf("%.2f", prev), ")")) }
    if (nrow(st) >= 6L) { prev <- as.numeric(st$close[6]); if (is.finite(prev) && prev != 0) lines <- c(lines, paste0("Since last week:   ", fmt_chg(100 * (current - prev) / prev), " (~5 trading days ago, $", sprintf("%.2f", prev), ")")) }
    if (nrow(st) >= 22L) { prev <- as.numeric(st$close[22]); if (is.finite(prev) && prev != 0) lines <- c(lines, paste0("Since last month:  ", fmt_chg(100 * (current - prev) / prev), " (~21 trading days ago, $", sprintf("%.2f", prev), ")")) }
    if (nrow(st) >= 253L) { prev <- as.numeric(st$close[253]); if (is.finite(prev) && prev != 0) lines <- c(lines, paste0("Since last year:   ", fmt_chg(100 * (current - prev) / prev), " (~252 trading days ago, $", sprintf("%.2f", prev), ")")) }
    if (length(lines) < 7L) lines <- c(lines, "", paste0("Note: Data has ", nrow(st), " trading days. Use Full history for 1-year trend.")); paste(lines, collapse = "\n")
  }
  build_forex_trend_report <- function() {
    fx <- fx_df(); if (is.null(fx) || nrow(fx) == 0) return("Fetch forex data first (Forex section).")
    pair <- paste0(input$fx_from %||% "From", "/", input$fx_to %||% "To"); current <- as.numeric(fx$close[1]); if (!is.finite(current)) return("No valid rate.")
    lines <- c(paste0("Forex trend: ", pair), paste0("Current close: ", sprintf("%.4f", current)), "")
    fmt_chg <- function(pct) if (pct >= 0) paste0("+", sprintf("%.2f", pct), "%") else sprintf("%.2f", pct)
    if (nrow(fx) >= 2L) { prev <- as.numeric(fx$close[2]); if (is.finite(prev) && prev != 0) lines <- c(lines, paste0("Since yesterday:  ", fmt_chg(100 * (current - prev) / prev), " (was ", sprintf("%.4f", prev), ")")) }
    if (nrow(fx) >= 6L) { prev <- as.numeric(fx$close[6]); if (is.finite(prev) && prev != 0) lines <- c(lines, paste0("Since last week:   ", fmt_chg(100 * (current - prev) / prev), " (~5 days ago, ", sprintf("%.4f", prev), ")")) }
    if (nrow(fx) >= 22L) { prev <- as.numeric(fx$close[22]); if (is.finite(prev) && prev != 0) lines <- c(lines, paste0("Since last month:  ", fmt_chg(100 * (current - prev) / prev), " (~21 days ago, ", sprintf("%.4f", prev), ")")) }
    if (nrow(fx) >= 253L) { prev <- as.numeric(fx$close[253]); if (is.finite(prev) && prev != 0) lines <- c(lines, paste0("Since last year:   ", fmt_chg(100 * (current - prev) / prev), " (~252 days ago, ", sprintf("%.4f", prev), ")")) }
    if (length(lines) < 7L) lines <- c(lines, "", paste0("Note: Data has ", nrow(fx), " days.")); paste(lines, collapse = "\n")
  }
  build_value_trend_report <- function(df, label, value_col) {
    if (is.null(df) || nrow(df) == 0) return(paste0("Fetch ", label, " data first."))
    if (is.null(value_col) || !value_col %in% names(df)) value_col <- names(df)[2]
    current <- as.numeric(df[[value_col]][1]); if (!is.finite(current)) return("No valid value.")
    lines <- c(paste0(label, " trend"), paste0("Current value: ", sprintf("%.4f", current)), "")
    fmt_chg <- function(pct) if (pct >= 0) paste0("+", sprintf("%.2f", pct), "%") else sprintf("%.2f", pct)
    if (nrow(df) >= 2L) { prev <- as.numeric(df[[value_col]][2]); if (is.finite(prev) && prev != 0) lines <- c(lines, paste0("Since previous: ", fmt_chg(100 * (current - prev) / prev), " (was ", sprintf("%.4f", prev), ")")) }
    if (nrow(df) >= 6L) { prev <- as.numeric(df[[value_col]][6]); if (is.finite(prev) && prev != 0) lines <- c(lines, paste0("Since ~1 week ago: ", fmt_chg(100 * (current - prev) / prev), " (", sprintf("%.4f", prev), ")")) }
    if (nrow(df) >= 22L) { prev <- as.numeric(df[[value_col]][22]); if (is.finite(prev) && prev != 0) lines <- c(lines, paste0("Since ~1 month ago: ", fmt_chg(100 * (current - prev) / prev), " (", sprintf("%.4f", prev), ")")) }
    if (nrow(df) >= 253L) { prev <- as.numeric(df[[value_col]][253]); if (is.finite(prev) && prev != 0) lines <- c(lines, paste0("Since ~1 year ago: ", fmt_chg(100 * (current - prev) / prev), " (", sprintf("%.4f", prev), ")")) }
    if (length(lines) < 5L) lines <- c(lines, "", paste0("Note: Data has ", nrow(df), " points.")); paste(lines, collapse = "\n")
  }
  observeEvent(input$section, {
    ch <- report_choices()
    updateSelectInput(session, "ai_report_type", choices = ch, selected = ch[1])
  })
  output$ui_ai_reporter <- renderUI({
    key <- ai_effective_key(); has_key <- is.character(key) && nzchar(trimws(key)); loading <- ai_loading(); report <- ai_report_text(); ch <- report_choices()
    needs_ai <- input$ai_report_type %||% "" %in% c("brief", "stock", "movers", "news", "forex_brief", "forex_snapshot", "commodity_brief", "commodity_snapshot", "economic_brief", "economic_snapshot")
    if (needs_ai && !has_key) return(div(class = "ai-reporter-card", div(class = "ai-title", "AI Market Reporter"), div(class = "ai-desc", "This report needs an AI key. Add OLLAMA_CLOUD_API_KEY or OPENAI_API_KEY to .env and restart."), div(class = "ai-reporter-connect", p(style = "margin: 0; font-size: 0.9rem;", "Ollama Cloud: ", tags$a(href = "https://ollama.com/settings", target = "_blank", "ollama.com/settings"), ". OpenAI: OPENAI_API_KEY=sk-... in .env."))))
    tagList(
      div(class = "ai-reporter-card",
        div(class = "ai-title", "AI Market Reporter"),
        div(class = "ai-desc", "Report type depends on the selected section. Trend reports use data only (no AI key)."),
        selectInput("ai_report_type", "Report type", choices = ch, selected = if (!is.null(input$ai_report_type) && input$ai_report_type %in% ch) input$ai_report_type else ch[1]),
        actionButton("ai_generate", "Generate report", class = "btn-primary")
      ),
      if (loading) div(class = "card-custom pulse", p(style = "margin: 0; color: var(--text-muted);", "Generating report...")) else NULL,
      if (!loading && !is.null(report) && nzchar(report)) div(class = "card-custom", style = "margin-top: 1rem;", h4("Report"), div(class = "ai-report-output", report)) else NULL
    )
  })
  observeEvent(input$ai_generate, {
    report_type <- input$ai_report_type %||% "brief"
    ai_loading(TRUE); ai_report_text(NULL)
    if (report_type == "stock_trend") { ai_loading(FALSE); ai_report_text(build_stock_trend_report()); return() }
    if (report_type == "forex_trend") { ai_loading(FALSE); ai_report_text(build_forex_trend_report()); return() }
    if (report_type == "commodity_trend") { ai_loading(FALSE); ai_report_text(build_value_trend_report(commodity_df(), paste0("Commodity (", input$commodity %||% "", ")"), "value")); return() }
    if (report_type == "economic_trend") { ai_loading(FALSE); ai_report_text(build_value_trend_report(economic_df(), paste0("Indicator (", input$economic_indicator %||% "", ")"), "value")); return() }
    key <- ai_effective_key(); if (!is.character(key) || !nzchar(trimws(key))) { ai_loading(FALSE); return() }
    context <- build_ai_context(report_type)
    prompt <- switch(
      report_type,
      brief = paste0("You are a professional equity research analyst. Based on the following market data, write a rich but concise overview in 2–3 short paragraphs (6–10 sentences). Focus on: current level and recent trend, broader context, key opportunities and risks, and what to watch next. Write in clear prose only.\n\n", context),
      stock = paste0("You are a market commentator. Based on this stock snapshot, write 2–4 sentences on performance, trend, and key risks or drivers. Be concise.\n\n", context),
      movers = paste0("You are a market analyst. Based on these top gainers and losers, write 2–3 sentences on what's moving the market. Highlight sectors, themes, and risks. Respond only in plain English paragraphs, no JSON or code.\n\n", context),
      news = paste0("You are a news summarizer. Based on these headlines, write 2–3 sentences on main themes and implications. Be neutral.\n\n", context),
      forex_brief = paste0("You are a forex analyst. Based on the following FX data, write a short overview (2–3 paragraphs): level and trend, drivers, and what to watch. Plain prose only.\n\n", context),
      forex_snapshot = paste0("You are a forex commentator. Based on this FX snapshot, write 2–4 sentences on the pair's performance and outlook. Be concise.\n\n", context),
      commodity_brief = paste0("You are a commodity analyst. Based on the following data, write a short overview (2–3 paragraphs): level and trend, drivers, and risks. Plain prose only.\n\n", context),
      commodity_snapshot = paste0("You are a commodity commentator. Based on this snapshot, write 2–4 sentences on performance and outlook. Be concise.\n\n", context),
      economic_brief = paste0("You are an economic analyst. Based on the following indicator data, write a short overview (2–3 paragraphs): level and trend, context, and implications. Plain prose only.\n\n", context),
      economic_snapshot = paste0("You are an economic commentator. Based on this indicator snapshot, write 2–4 sentences on the reading and what it suggests. Be concise.\n\n", context),
      paste0("Summarize this data in 2–4 sentences:\n\n", context)
    )
    prompt <- paste0(prompt, "\n\nRespond only with human-readable English in paragraph form. No JSON, code, or search queries.")
    result <- ollama_chat(prompt, key)
    ai_loading(FALSE); if (result$ok) ai_report_text(result$text) else set_err(simpleError(result$error))
  })
}

shinyApp(ui, server)
