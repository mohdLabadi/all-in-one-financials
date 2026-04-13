# Daily Market Analysis Tool – Alpha Vantage
# Based on 06_alphavantage_ai_report.R: readRenviron(.env), API_KEY / ALPHAVANTAGE_API_KEY, httr2.
# Run (from 5381tool1): source("run_market.R")  or  shiny::runApp("app_market.R")
# VS Code Code Runner: run run_market.R — not app_market.R alone (unless you use Run App in RStudio).
# Requires: httr2, jsonlite, shiny, ggplot2, DT, dplyr
#
# AI pipeline: agentic orchestration (3 agents) + RAG: local corpus data/rag_market_corpus.txt is
# retrieved (keyword overlap) and prepended to the app context for all three LLM calls. No LLM tool calling.

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
# When working directory is the repo root (e.g. dsai), the first .env found may be missing LLM keys;
# always merge in 5381tool1/.env if that file exists (same folder as this app).
env_5381 <- file.path(getwd(), "5381tool1", ".env")
if (file.exists(env_5381)) readRenviron(env_5381)

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
av_cache_env <- new.env(parent = emptyenv())

is_rate_limited_error <- function(msg) {
  m <- tolower(as.character(msg %||% ""))
  grepl("rate limit|5 requests|min|25/day|429|too many", m)
}

cache_put <- function(key, value) assign(key, value, envir = av_cache_env)
cache_get <- function(key) if (exists(key, envir = av_cache_env, inherits = FALSE)) get(key, envir = av_cache_env, inherits = FALSE) else NULL

safe_fetch_with_cache <- function(cache_key, fetch_fn) {
  tryCatch({
    out <- fetch_fn()
    cache_put(cache_key, out)
    list(ok = TRUE, data = out, fallback = FALSE, msg = NULL)
  }, error = function(e) {
    em <- conditionMessage(e)
    if (is_rate_limited_error(em)) {
      old <- cache_get(cache_key)
      if (!is.null(old)) {
        return(list(
          ok = TRUE,
          data = old,
          fallback = TRUE,
          msg = "Alpha Vantage limit reached. Showing last cached result for this query."
        ))
      }
    }
    list(ok = FALSE, error = em)
  })
}

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

# ---- Multi-agent: chat with explicit roles (system + user/assistant) ----
# messages: list of list(role = "system"|"user"|"assistant", content = "...")
# Retries on HTTP 429 with backoff (same pattern as rate_limit_429_hint text).
llm_chat_messages <- function(messages, api_key, provider = c("ollama", "openai"), model = NULL) {
  provider <- match.arg(provider)
  if (!nzchar(api_key)) return(list(ok = FALSE, error = "No API key provided."))
  if (is.null(model)) model <- if (provider == "ollama") "gpt-oss:120b" else "gpt-4o-mini"
  body <- if (provider == "ollama") {
    list(model = model, messages = messages, stream = FALSE)
  } else {
    list(model = model, messages = messages, max_tokens = 2048L)
  }
  url <- if (provider == "ollama") "https://ollama.com/api/chat" else "https://api.openai.com/v1/chat/completions"
  if (use_httr2) {
    chat_once <- function() {
      tryCatch({
        req <- request(url) %>%
          req_headers(Authorization = paste0("Bearer ", api_key), `Content-Type` = "application/json") %>%
          req_body_json(body) %>% req_method("POST")
        resp <- req_perform(req)
        st <- resp_status(resp)
        raw <- tryCatch(resp_body_string(resp), error = function(e) "")
        if (st == 200L && nzchar(raw)) {
          out <- jsonlite::fromJSON(raw, simplifyVector = TRUE)
          text <- if (provider == "ollama") out$message$content else out$choices[[1]]$message$content
          if (is.null(text)) return(list(ok = FALSE, error = "Empty response.", status = st))
          return(list(ok = TRUE, text = as.character(text), status = st))
        }
        err <- paste0("HTTP ", st)
        if (nzchar(raw)) {
          pj <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
          if (!is.null(pj$error)) {
            em <- pj$error$message %||% pj$error
            if (length(em)) err <- paste(as.character(em), collapse = " ")
          }
        }
        list(ok = FALSE, error = err, status = st)
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e), status = NA_integer_))
    }
    last <- NULL
    for (attempt in 1:5) {
      if (attempt > 1L) {
        if (!identical(last$status, 429L)) break
        Sys.sleep(c(6, 15, 30, 45)[attempt - 1L])
      }
      last <- chat_once()
      if (isTRUE(last$ok)) return(list(ok = TRUE, text = last$text))
    }
    err <- last$error %||% "Chat request failed."
    if (identical(last$status, 429L)) err <- paste0(err, rate_limit_429_hint())
    return(list(ok = FALSE, error = err))
  } else {
    tryCatch({
      r <- POST(url,
        add_headers(Authorization = paste0("Bearer ", api_key), `Content-Type` = "application/json"),
        body = body, encode = "json")
      sc <- status_code(r)
      if (sc != 200L) {
        err <- paste0("API status ", sc)
        if (sc == 429L) err <- paste0(err, rate_limit_429_hint())
        return(list(ok = FALSE, error = err))
      }
      out <- content(r, as = "parsed")
      text <- if (provider == "ollama") out$message$content else out$choices[[1]]$message$content
      if (is.null(text)) return(list(ok = FALSE, error = "Empty response."))
      list(ok = TRUE, text = as.character(text))
    }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  }
}

pick_llm_credentials <- function() {
  creds <- list()
  if (nzchar(trimws(OLLAMA_CLOUD_API_KEY))) creds <- c(creds, list(list(provider = "ollama", key = OLLAMA_CLOUD_API_KEY)))
  if (nzchar(trimws(OPENAI_API_KEY))) creds <- c(creds, list(list(provider = "openai", key = OPENAI_API_KEY)))
  if (length(creds) == 0L) return(list(ok = FALSE, creds = list()))
  list(ok = TRUE, creds = creds)
}

llm_chat_messages_with_fallback <- function(messages, creds) {
  if (length(creds) == 0L) return(list(ok = FALSE, error = "No configured AI provider.", provider = NA_character_))
  errs <- character(0)
  for (i in seq_along(creds)) {
    c0 <- creds[[i]]
    out <- llm_chat_messages(messages, api_key = c0$key, provider = c0$provider)
    if (isTRUE(out$ok)) return(list(ok = TRUE, text = out$text, provider = c0$provider))
    errs <- c(errs, paste0(c0$provider, ": ", out$error %||% "Unknown error"))
  }
  list(ok = FALSE, error = paste(errs, collapse = " | "), provider = NA_character_)
}

truncate_ai_context <- function(text, max_chars = 12000L) {
  if (is.null(text) || !nzchar(text)) return(text)
  if (nchar(text) <= max_chars) return(text)
  paste0(substring(text, 1L, max_chars), "\n\n[Context truncated for length.]")
}

# ---- RAG: retrieve chunks from local corpus (data/rag_market_corpus.txt), segments separated by --- ----
# Resolve app folder when getwd() is wrong (e.g. Shiny, RStudio project root): use path of this script if available.
daily_market_app_dir <- function() {
  args_all <- commandArgs(trailingOnly = FALSE)
  file_line <- grep("^--file=", args_all, value = TRUE)
  if (length(file_line)) {
    sp <- sub("^--file=", "", file_line[1])
    sp <- normalizePath(sp, winslash = "/", mustWork = FALSE)
    if (!is.na(sp) && nzchar(sp) && grepl("app_market\\.R$", sp, ignore.case = TRUE) && file.exists(sp)) {
      return(dirname(sp))
    }
  }
  wd <- getwd()
  if (file.exists(file.path(wd, "app_market.R"))) return(normalizePath(wd, winslash = "/", mustWork = FALSE))
  if (file.exists(file.path(wd, "5381tool1", "app_market.R"))) {
    return(normalizePath(file.path(wd, "5381tool1"), winslash = "/", mustWork = FALSE))
  }
  wd
}

rag_corpus_path <- function() {
  appd <- daily_market_app_dir()
  candidates <- c(
    file.path(appd, "data", "rag_market_corpus.txt"),
    file.path(getwd(), "data", "rag_market_corpus.txt"),
    file.path(getwd(), "5381tool1", "data", "rag_market_corpus.txt"),
    file.path("data", "rag_market_corpus.txt"),
    file.path("5381tool1", "data", "rag_market_corpus.txt")
  )
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  NA_character_
}

# Used only if rag_market_corpus.txt is missing on disk (still gives RAG traces).
RAG_CORPUS_EMBEDDED <- paste0(
  "Stock and index levels move with earnings, rates, and sentiment. When comparing day-over-day moves, use the same price field (e.g. adjusted close vs close) and note whether the series is trading days only.\n\n---\n\n",
  "Forex pairs quote the value of the base currency in terms of the quote. A rising EUR/USD means euros buy more dollars. Macro drivers include interest-rate differentials, risk appetite, and surprise data prints.\n\n---\n\n",
  "Commodity futures and spot references differ by contract month and delivery location. Reported latest values may be month-end or last settlement; state the source cadence when interpreting moves.\n\n---\n\n",
  "Economic indicators are often revised. CPI and jobs releases can move markets on the surprise versus consensus, not only the level. Treasury yields embed growth and inflation expectations.\n\n---\n\n",
  "News sentiment scores are model-based and noisy. Use them as one signal alongside price action and fundamentals. Headlines may lag fast markets.\n\n---\n\n",
  "Top gainers and losers lists are snapshots; liquidity and halts can distort one-day percent changes. Cross-check unusual movers against corporate actions.\n\n---\n\n",
  "Risk: past performance does not guarantee future results. This workstation combines retrieved notes with live API context; always verify figures against primary sources."
)

load_rag_chunks <- function() {
  path <- rag_corpus_path()
  raw <- if (!is.na(path) && nzchar(path) && file.exists(path)) {
    tryCatch(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), error = function(e) "")
  } else {
    ""
  }
  if (!nzchar(raw)) raw <- RAG_CORPUS_EMBEDDED
  parts <- strsplit(raw, "\n---\n", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    parts <- strsplit(RAG_CORPUS_EMBEDDED, "\n---\n", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
  }
  parts
}

retrieve_rag_for_report <- function(report_type, section, context, k = 3L) {
  chunks <- load_rag_chunks()
  if (length(chunks) == 0L) {
    return(list(text = "", trace = "RAG: corpus empty (unexpected)."))
  }
  query <- paste(report_type, section, context)
  qtok <- tolower(unlist(strsplit(gsub("[^a-zA-Z0-9]+", " ", query), "\\s+")))
  qtok <- unique(qtok[nchar(qtok) > 2L])
  if (length(qtok) == 0L) qtok <- c("market", "data")
  score_chunk <- function(ch) {
    ct <- tolower(ch)
    sum(vapply(qtok, function(w) as.integer(grepl(w, ct, fixed = TRUE)), integer(1)))
  }
  sc <- vapply(chunks, score_chunk, integer(1))
  ord <- order(sc, decreasing = TRUE)
  take <- head(ord[sc[ord] > 0L], n = k)
  if (length(take) == 0L) take <- head(ord, n = min(k, length(chunks)))
  sel <- chunks[take]
  rp <- rag_corpus_path()
  src_line <- if (!is.na(rp) && nzchar(rp) && file.exists(rp)) {
    paste0("Corpus file: ", rp)
  } else {
    "Corpus: embedded default (place data/rag_market_corpus.txt beside app_market.R to override)"
  }
  trace <- paste0("[", seq_along(sel), "] ", substr(gsub("\n", " ", sel), 1L, min(100L, nchar(sel))), "...")
  list(
    text = paste(sel, collapse = "\n\n"),
    trace = paste(c(src_line, trace), collapse = "\n")
  )
}

section_display_name <- function(section) {
  switch(section %||% "",
    stock = "Stock Daily",
    gainers = "Top Gainers/Losers",
    news = "News & Sentiment",
    forex = "Forex",
    commodity = "Commodities",
    economic = "Economic Indicators",
    ai = "AI Reporter",
    as.character(section %||% "Market")
  )
}

report_type_display_name <- function(report_type) {
  switch(report_type %||% "",
    brief = "Cross-section overview",
    stock = "Stock snapshot",
    stock_trend = "Stock trend (computed)",
    movers = "Top movers insight",
    news = "News briefing",
    forex_brief = "Forex overview",
    forex_snapshot = "Forex snapshot",
    forex_trend = "Forex trend (computed)",
    commodity_brief = "Commodity overview",
    commodity_snapshot = "Commodity snapshot",
    commodity_trend = "Commodity trend (computed)",
    economic_brief = "Economic indicator overview",
    economic_snapshot = "Economic indicator snapshot",
    economic_trend = "Economic trend (computed)",
    as.character(report_type %||% "Analysis")
  )
}

# Agent 1 — Orchestration: plan themes, gaps, and delegation (no polished narrative).
AGENT_SYSTEM_ORCHESTRATOR <- paste(
  "You are the Orchestration Lead for a market analysis workstation.",
  "The user message may include RETRIEVED KNOWLEDGE (RAG) — short reference notes from a local corpus — plus APPLICATION / API CONTEXT from the workstation.",
  "Plan the workflow only: prioritize themes, identify gaps in the provided data, and state what downstream analysis should emphasize.",
  "Do not write a polished narrative for the end user.",
  "Output clearly labeled sections: PRIORITY THEMES (bullets), DATA COVERAGE & GAPS, DELEGATION (what the Market Analyst should stress-test).",
  "If the context indicates no data was fetched, say so. Never invent prices, rates, or statistics not present in the context.",
  sep = "\n"
)

# Agent 2 — Analyst: evidence-based memo from context + plan (RAG notes are guidance, not live prices).
AGENT_SYSTEM_ANALYST <- paste(
  "You are the Market Intelligence Analyst.",
  "You receive (1) optional RETRIEVED KNOWLEDGE (RAG) — general reference text, (2) APPLICATION / API CONTEXT with numbers from the app, and (3) the Orchestration Lead's plan.",
  "Treat RAG as background reading; cite numbers only from the APPLICATION / API CONTEXT unless the RAG chunk itself contains a numeric fact you clearly label as from the note corpus.",
  "Produce rigorous, evidence-based analysis: cite specific numbers from the application context when available; separate facts from interpretation.",
  "Use headings: FACTS FROM DATA, INTERPRETATION, RISKS & UNCERTAINTIES, SCENARIOS (or state that scenarios are not warranted).",
  "If data is missing for a point, write 'Insufficient data in context' for that item. Do not fabricate figures.",
  sep = "\n"
)

# Agent 3 — Editor: unify plan + analyst memo into the user-facing brief.
AGENT_SYSTEM_EDITOR <- paste(
  "You are the Lead Editor.",
  "Combine the Orchestration plan and the Market Analyst memo into one coherent brief for the reader.",
  "Match the requested style: overview briefs use 2–3 short paragraphs; snapshots use 2–4 tight sentences; stay neutral for news.",
  "Remove redundancy, align tone, and end with one sentence on what to watch next.",
  "Output plain paragraphs only—no JSON, code, or section labels like 'SECTION 1'.",
  sep = "\n"
)

run_agentic_pipeline <- function(report_type, section, context, cred, progress_fn = NULL) {
  ctx_base <- truncate_ai_context(context)
  rag <- retrieve_rag_for_report(report_type, section, ctx_base, k = 3L)
  rag_block <- if (nzchar(rag$text)) {
    paste0("RETRIEVED KNOWLEDGE (RAG — local corpus, keyword retrieval):\n\n", rag$text)
  } else {
    ""
  }
  ctx <- paste0(
    if (nzchar(rag_block)) paste0(rag_block, "\n\n---\n\n") else "",
    "APPLICATION / API CONTEXT (from the workstation):\n\n",
    ctx_base
  )
  sec_name <- section_display_name(section)
  rtp_name <- report_type_display_name(report_type)
  creds <- cred$creds
  providers_used <- character(0)
  p <- function(value, detail) {
    if (is.function(progress_fn)) progress_fn(value, detail)
  }
  u1 <- paste0(
    "Report type code: ", report_type, " (", rtp_name, ")\n",
    "App section: ", sec_name, "\n\n",
    ctx
  )
  p(0.2, "Agent 1 — Orchestrator: planning themes and gaps…")
  r1 <- llm_chat_messages_with_fallback(
    list(
      list(role = "system", content = AGENT_SYSTEM_ORCHESTRATOR),
      list(role = "user", content = u1)
    ),
    creds = creds
  )
  if (!r1$ok) return(list(ok = FALSE, error = r1$error, orchestrator = NULL, analyst = NULL, final = NULL, rag_trace = rag$trace, providers_used = providers_used))
  providers_used <- c(providers_used, paste0("orchestrator=", r1$provider))
  orch <- r1$text
  Sys.sleep(2.2)
  u2 <- paste0(
    "ORCHESTRATION PLAN:\n", orch, "\n\n",
    "FULL CONTEXT (same as Orchestrator — RAG + application data):\n", ctx
  )
  p(0.55, "Agent 2 — Market Analyst: evidence-based memo…")
  r2 <- llm_chat_messages_with_fallback(
    list(
      list(role = "system", content = AGENT_SYSTEM_ANALYST),
      list(role = "user", content = u2)
    ),
    creds = creds
  )
  if (!r2$ok) return(list(ok = FALSE, error = r2$error, orchestrator = orch, analyst = NULL, final = NULL, rag_trace = rag$trace, providers_used = providers_used))
  providers_used <- c(providers_used, paste0("analyst=", r2$provider))
  an <- r2$text
  Sys.sleep(2.2)
  orch_short <- if (nchar(orch) > 1800L) paste0(substring(orch, 1L, 1800L), "\n[…]") else orch
  u3 <- paste0(
    "Deliverable: ", rtp_name, " for section ", sec_name, ".\n\n",
    "ORCHESTRATION (for alignment; may be shortened):\n", orch_short, "\n\n",
    "MARKET ANALYST OUTPUT:\n", an
  )
  p(0.85, "Agent 3 — Lead Editor: final brief…")
  r3 <- llm_chat_messages_with_fallback(
    list(
      list(role = "system", content = AGENT_SYSTEM_EDITOR),
      list(role = "user", content = u3)
    ),
    creds = creds
  )
  if (!r3$ok) return(list(ok = FALSE, error = r3$error, orchestrator = orch, analyst = an, final = NULL, rag_trace = rag$trace, providers_used = providers_used))
  providers_used <- c(providers_used, paste0("editor=", r3$provider))
  p(1, "Done")
  list(ok = TRUE, error = NULL, orchestrator = orch, analyst = an, final = r3$text, rag_trace = rag$trace, providers_used = providers_used)
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

# Shown when HTTP 429 (LLM or burst traffic).
rate_limit_429_hint <- function() {
  paste0(
    " [429 = rate limit: wait 1–3 min, then retry. The 3-agent pipeline sends several LLM requests per run. ",
    "Upgrade API tier or space out runs. Alpha Vantage free tier: 5 requests/minute.]"
  )
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
  .agent-pipeline { margin-top: 1rem; }
  .agent-step { background: #f8fafc; border: 1px solid var(--border); border-radius: 10px; padding: 0.75rem 1rem; margin-bottom: 0.65rem; }
  .agent-step summary { font-weight: 700; font-size: 0.82rem; color: var(--accent); cursor: pointer; }
  .agent-step .agent-body { margin-top: 0.5rem; white-space: pre-wrap; font-size: 0.82rem; line-height: 1.5; color: var(--text); max-height: 280px; overflow-y: auto; }
  .empty-state-card { background: #f8fafc; border: 1px dashed var(--border); border-radius: 10px; padding: 0.9rem 1rem; margin-bottom: 0.85rem; }
  .empty-state-card .title { font-weight: 700; font-size: 0.86rem; color: var(--accent); margin-bottom: 0.25rem; }
  .empty-state-card .desc { margin: 0; color: var(--text-muted); font-size: 0.82rem; line-height: 1.45; }
  .status-pill { display: inline-block; padding: 0.18rem 0.55rem; border-radius: 999px; font-size: 0.72rem; font-weight: 700; border: 1px solid var(--border); background: #fff; color: var(--text-muted); margin-right: 0.35rem; }
  .status-pill.ok { color: #065f46; border-color: #a7f3d0; background: #ecfdf5; }
  .pulse { animation: pulse 1.5s ease-in-out infinite; }
  @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.6; } }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  titlePanel(windowTitle = "Daily Market", title = div(class = "app-header", div(class = "title", "Daily Market Analysis"), div(class = "subtitle", "Stocks · Forex · News · Multi-agent AI analysis"))),
  sidebarLayout(
    sidebarPanel(class = "sidebar", width = 3,
      selectInput("section", "Section",
        choices = c("Stock Daily" = "stock", "Top Gainers/Losers" = "gainers", "News & Sentiment" = "news", "Forex" = "forex",
          "Commodities" = "commodity", "Economic Indicators" = "economic", "AI Reporter" = "ai"),
        selected = "ai"),
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
          uiOutput("ui_section_help"),
          conditionalPanel(
            "input.section == 'ai'",
            div(class = "card-custom", p(style = "margin:0; color: var(--text-muted);", "AI Reporter is available in the right panel. Fetch any data section first for best results, then click Generate report."))
          ),
          p("Select a section and click Fetch to load data. View, filter, and Download use already-loaded data (no extra API calls).", class = "api-limit-note"),
          uiOutput("api_fallback_ui"),
          uiOutput("api_error_ui"),
          conditionalPanel(
            "input.section == 'stock'",
            uiOutput("ui_stock_toolbar"),
            uiOutput("ui_stock_summary"),
            div(class = "plot-container", plotOutput("plot_stock")),
            div(class = "plot-container", plotOutput("plot_stock_returns", height = "240px")),
            uiOutput("ui_stock_calculator"),
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
            div(class = "plot-container", plotOutput("plot_news_sentiment", height = "250px")),
            uiOutput("ui_news_cards"),
            conditionalPanel("input.show_raw_data", DT::dataTableOutput("table_news"))
          ),
          conditionalPanel(
            "input.section == 'forex'",
            uiOutput("ui_fx_toolbar"),
            uiOutput("ui_fx_summary"),
            div(class = "plot-container", plotOutput("plot_fx")),
            div(class = "plot-container", plotOutput("plot_fx_returns", height = "240px")),
            uiOutput("ui_fx_calculator"),
            conditionalPanel("input.show_raw_data", DT::dataTableOutput("table_fx"))
          ),
          conditionalPanel(
            "input.section == 'commodity'",
            uiOutput("ui_commodity_toolbar"),
            uiOutput("ui_commodity_summary"),
            div(class = "plot-container", plotOutput("plot_commodity")),
            div(class = "plot-container", plotOutput("plot_commodity_changes", height = "240px")),
            uiOutput("ui_commodity_calculator"),
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
  fallback_msg <- reactiveVal(NULL)
  stock_df <- reactiveVal(NULL)
  gainers_data <- reactiveVal(NULL)
  news_df <- reactiveVal(NULL)
  fx_df <- reactiveVal(NULL)
  currency_rate_df <- reactiveVal(NULL)
  commodity_df <- reactiveVal(NULL)
  economic_df <- reactiveVal(NULL)
  ai_report_text <- reactiveVal(NULL)
  ai_orchestrator_text <- reactiveVal(NULL)
  ai_analyst_text <- reactiveVal(NULL)
  ai_rag_trace_text <- reactiveVal(NULL)
  ai_stage_text <- reactiveVal(NULL)
  ai_provider_trace <- reactiveVal(NULL)
  ai_loading <- reactiveVal(FALSE)

  clear_err <- function() err_msg(NULL)
  clear_fallback <- function() fallback_msg(NULL)
  set_err <- function(e) err_msg(paste0("Error: ", conditionMessage(e)))
  qmark <- function(label, tip) {
    tagList(
      tags$span(label),
      tags$span(" ?", title = tip, style = "cursor: help; color: #0f766e; font-weight: 700; margin-left: 2px;")
    )
  }

  output$section_title <- renderUI({
    titles <- c(
      stock = "Stock Daily",
      gainers = "Top Gainers/Losers",
      news = "News & Sentiment",
      forex = "Forex",
      commodity = "Commodities",
      economic = "Economic Indicators",
      ai = "AI Reporter"
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
  output$api_fallback_ui <- renderUI({
    msg <- fallback_msg()
    if (is.null(msg) || !nzchar(msg)) return(NULL)
    div(class = "card-custom", style = "border-left: 4px solid #f59e0b; background: #fffbeb;", p(style = "margin: 0; color: #92400e; font-weight: 600;", msg))
  })
  output$ui_section_help <- renderUI({
    sec <- input$section %||% "stock"
    msg <- switch(sec,
      stock = "Enter a symbol (example: AAPL), click Fetch, then use the toolbar to adjust the window.",
      gainers = "Click Fetch movers to load gainers, losers, and active symbols. Use filter/chart toggles to narrow focus.",
      news = "Optionally provide tickers (example: AAPL,MSFT), then filter stories by keyword or sentiment.",
      forex = "Set From/To currencies (example: EUR/USD), fetch data, then inspect the trend and summary cards.",
      commodity = "Choose a commodity and interval, fetch, then review latest level and range in the chart card.",
      economic = "Choose an indicator and interval, fetch, then compare current value against period min/max.",
      ai = "Use the AI Reporter panel to generate a 3-agent analysis from your currently fetched data.",
      "Fetch data in this section first, then run AI analysis if needed."
    )
    div(class = "empty-state-card",
      div(class = "title", "Quick start for this section"),
      p(class = "desc", msg)
    )
  })

  # ---- Stock Daily ----
  observeEvent(input$fetch_stock, {
    clear_err(); clear_fallback()
    tryCatch({
      sym <- trimws(input$stock_symbol %||% "")
      if (length(sym) == 0L || !nzchar(sym)) { set_err(simpleError("Please enter a symbol.")); return() }
      outsize <- if (isTRUE(input$stock_full_history)) "full" else "compact"
      rk <- paste("stock", toupper(sym), outsize, sep = "|")
      res <- safe_fetch_with_cache(rk, function() av_stock_daily(sym, outputsize = outsize))
      if (!isTRUE(res$ok)) stop(res$error %||% "Fetch failed.")
      df <- res$data
      if (is.null(df) || nrow(df) == 0) {
        set_err(simpleError("No data returned. Check API key, symbol, or rate limit (5/min, 25/day)."))
        stock_df(NULL)
      } else {
        stock_df(df)
        if (isTRUE(res$fallback)) fallback_msg(res$msg %||% "Using cached data.")
      }
    }, error = set_err)
  })
  output$ui_stock_toolbar <- renderUI({
    if (is.null(stock_df()) || nrow(stock_df()) == 0) return(NULL)
    div(class = "toolbar",
        div(class = "toolbar-item toolbar-item-wide", span(class = "toolbar-label", "View"), selectInput("stock_days", NULL, choices = c("Last 5 days" = 5, "Last 21 days" = 21, "Last 60 days" = 60, "Last 90 days" = 90), selected = 90)),
        div(class = "toolbar-item", checkboxInput("stock_show_ma", qmark("MA(20/50)", "Show 20-day and 50-day moving averages."), value = TRUE)),
        div(class = "toolbar-item", checkboxInput("stock_show_bands", qmark("Bollinger", "Show Bollinger upper/lower volatility bands."), value = FALSE)),
        div(class = "toolbar-item", numericInput("stock_calc_invest", qmark("Invest $", "How much capital to simulate in the position calculator."), value = 10000, min = 0, step = 100)),
        div(class = "toolbar-item", numericInput("stock_calc_target", qmark("Target %", "Desired percent move used for projected P/L."), value = 5, min = -100, max = 500, step = 0.5)),
        div(class = "toolbar-item", numericInput("stock_scn_days", qmark("Scenario days", "Number of future days to project on the chart."), value = 20, min = 1, max = 252, step = 1)),
        div(class = "toolbar-item", numericInput("stock_scn_drift", qmark("Scenario daily %", "Assumed daily percent change for scenario path."), value = 0.2, min = -10, max = 10, step = 0.1)),
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
    chg5 <- if (nrow(df) >= 6L && is.finite(df$close[6]) && df$close[6] != 0) round(100 * (r$close - df$close[6]) / df$close[6], 2) else NA
    chg21 <- if (nrow(df) >= 22L && is.finite(df$close[22]) && df$close[22] != 0) round(100 * (r$close - df$close[22]) / df$close[22], 2) else NA
    lret <- diff(log(as.numeric(df$close)))
    lret <- lret[is.finite(lret)]
    vol_ann <- if (length(lret) >= 5L) round(sd(lret) * sqrt(252) * 100, 1) else NA
    cl_class <- if (!is.na(chg)) if (chg >= 0) "value positive" else "value negative" else "value"
    chg_txt <- if (!is.na(chg)) paste0(if (chg >= 0) "+" else "", chg, "%") else "—"
    chg5_txt <- if (!is.na(chg5)) paste0(if (chg5 >= 0) "+" else "", chg5, "%") else "—"
    chg21_txt <- if (!is.na(chg21)) paste0(if (chg21 >= 0) "+" else "", chg21, "%") else "—"
    cl5 <- if (!is.na(chg5)) if (chg5 >= 0) "value positive" else "value negative" else "value"
    cl21 <- if (!is.na(chg21)) if (chg21 >= 0) "value positive" else "value negative" else "value"
    regime <- if (nrow(df) >= 50L) {
      ma20 <- mean(head(df$close, 20), na.rm = TRUE)
      ma50 <- mean(head(df$close, 50), na.rm = TRUE)
      if (is.finite(ma20) && is.finite(ma50)) if (ma20 >= ma50) "Bullish trend regime" else "Bearish trend regime" else "Regime unavailable"
    } else {
      "Regime unavailable (need 50+ days)"
    }
    tagList(div(class = "stats-row",
      div(class = "stat-card card-custom", h4("Last close"), div(class = "value", sprintf("$%.2f", r$close))),
      div(class = "stat-card card-custom", h4("Change (1d)"), div(class = cl_class, chg_txt)),
      div(class = "stat-card card-custom", h4("Change (1w)"), div(class = cl5, chg5_txt)),
      div(class = "stat-card card-custom", h4("Change (1m)"), div(class = cl21, chg21_txt)),
      div(class = "stat-card card-custom", h4("High"), div(class = "value", sprintf("$%.2f", r$high %||% NA))),
      div(class = "stat-card card-custom", h4("Low"), div(class = "value", sprintf("$%.2f", r$low %||% NA))),
      div(class = "stat-card card-custom", h4("Ann. volatility"), div(class = "value", if (!is.na(vol_ann)) paste0(vol_ann, "%") else "—")),
      div(class = "stat-card card-custom", h4("Volume"), div(class = "value", format(as.numeric(r$volume %||% 0), big.mark = ",")))),
      div(class = "card-custom", p(style = "margin: 0; color: var(--text-muted); font-size: 0.82rem;",
        paste0("Signal: ", regime, ". This combines moving-average structure with recent returns to give quick context, not investment advice.")))
    )
  })
  output$plot_stock <- renderPlot({
    df <- stock_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    days <- as.integer(input$stock_days %||% 90)
    df <- head(df, days)
    df <- df[!is.na(df$date) & is.finite(as.numeric(df$close)), ]
    if (nrow(df) == 0) return(NULL)
    df <- df[order(df$date), ]
    cls <- as.numeric(df$close)
    ma20 <- as.numeric(stats::filter(cls, rep(1 / 20, 20), sides = 1))
    ma50 <- as.numeric(stats::filter(cls, rep(1 / 50, 50), sides = 1))
    sd20 <- vapply(seq_along(cls), function(i) {
      if (i < 20) return(NA_real_)
      stats::sd(cls[(i - 19):i], na.rm = TRUE)
    }, numeric(1))
    df$ma20 <- ma20
    df$ma50 <- ma50
    df$bb_up <- ma20 + 2 * sd20
    df$bb_dn <- ma20 - 2 * sd20
    p <- ggplot(df, aes(x = date)) +
      geom_line(aes(y = close, color = "Close"), linewidth = 1) +
      geom_point(aes(y = close, color = "Close"), size = 1.2, show.legend = FALSE) +
      labs(x = "Date", y = "Close", title = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
    if (isTRUE(input$stock_show_ma)) {
      p <- p +
        geom_line(aes(y = ma20, color = "MA20"), linewidth = 0.9, na.rm = TRUE) +
        geom_line(aes(y = ma50, color = "MA50"), linewidth = 0.9, na.rm = TRUE)
    }
    if (isTRUE(input$stock_show_bands)) {
      p <- p +
        geom_line(aes(y = bb_up, color = "Bollinger Upper"), linetype = "dashed", linewidth = 0.8, na.rm = TRUE) +
        geom_line(aes(y = bb_dn, color = "Bollinger Lower"), linetype = "dashed", linewidth = 0.8, na.rm = TRUE)
    }
    scn_days <- as.integer(input$stock_scn_days %||% 20)
    scn_drift <- as.numeric(input$stock_scn_drift %||% 0)
    if (is.finite(scn_days) && scn_days > 0 && is.finite(scn_drift)) {
      last_dt <- max(df$date, na.rm = TRUE)
      last_px <- as.numeric(tail(df$close, 1))
      if (is.finite(last_px) && last_px > 0) {
        fut_dates <- seq.Date(last_dt + 1, by = "day", length.out = scn_days)
        fut_px <- last_px * cumprod(rep(1 + scn_drift / 100, scn_days))
        scn <- data.frame(date = fut_dates, close = fut_px)
        p <- p +
          geom_line(data = scn, aes(x = date, y = close, color = "Scenario"), linewidth = 1, linetype = "dotdash") +
          geom_point(data = scn[nrow(scn), , drop = FALSE], aes(x = date, y = close), color = "#f59e0b", size = 2) +
          labs(subtitle = "Orange line: user-defined scenario path")
      }
    }
    p <- p + scale_color_manual(
      name = "Series",
      values = c(
        "Close" = "#0d9488",
        "MA20" = "#2563eb",
        "MA50" = "#7c3aed",
        "Bollinger Upper" = "#94a3b8",
        "Bollinger Lower" = "#64748b",
        "Scenario" = "#f59e0b"
      )
    )
    p
  })
  output$plot_stock_returns <- renderPlot({
    df <- stock_df()
    if (is.null(df) || nrow(df) < 3L) return(NULL)
    days <- as.integer(input$stock_days %||% 90)
    df <- head(df, days)
    df <- df[order(df$date), ]
    ret <- 100 * (as.numeric(df$close) / dplyr::lag(as.numeric(df$close)) - 1)
    r2 <- data.frame(date = df$date, ret = ret)
    r2 <- r2[is.finite(r2$ret), ]
    if (nrow(r2) < 2) return(NULL)
    r2$direction <- ifelse(r2$ret >= 0, "Up day", "Down day")
    ggplot(r2, aes(x = date, y = ret, fill = direction)) +
      geom_col(width = 0.9) +
      scale_fill_manual(name = "Return sign", values = c("Up day" = "#059669", "Down day" = "#dc2626")) +
      geom_hline(yintercept = 0, color = "#94a3b8", linewidth = 0.6) +
      labs(x = "Date", y = "Daily return (%)", title = "Return bars (up/down days)") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  output$ui_stock_calculator <- renderUI({
    df <- stock_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    px <- as.numeric(df$close[1])
    invest <- as.numeric(input$stock_calc_invest %||% 0)
    tgt <- as.numeric(input$stock_calc_target %||% 0)
    if (!is.finite(px) || px <= 0) return(NULL)
    shares <- if (is.finite(invest) && invest > 0) invest / px else 0
    tgt_px <- px * (1 + tgt / 100)
    pnl <- shares * (tgt_px - px)
    div(class = "card-custom",
      h4("Position calculator"),
      div(class = "stats-row",
        div(class = "stat-card card-custom", h4("Implied shares"), div(class = "value", sprintf("%.2f", shares))),
        div(class = "stat-card card-custom", h4("Current price"), div(class = "value", sprintf("$%.2f", px))),
        div(class = "stat-card card-custom", h4("Target price"), div(class = "value", sprintf("$%.2f", tgt_px))),
        div(class = "stat-card card-custom", h4("Projected P/L"), div(class = if (pnl >= 0) "value positive" else "value negative", sprintf("$%.2f", pnl)))
      ),
      p(style = "margin: 0; color: var(--text-muted); font-size: 0.8rem;", "Educational what-if only; ignores fees, slippage, and taxes.")
    )
  })
  output$table_stock <- renderDT({
    df <- stock_df()
    if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    DT::datatable(as.data.frame(df), options = list(pageLength = 15))
  })
  output$download_stock_csv <- downloadHandler(filename = function() paste0("stock_", input$stock_symbol %||% "symbol", "_", Sys.Date(), ".csv"), content = function(file) { df <- stock_df(); if (!is.null(df) && nrow(df) > 0) write.csv(df, file, row.names = FALSE) })

  # ---- Top Gainers / Losers ----
  observeEvent(input$fetch_gainers, {
    clear_err(); clear_fallback()
    tryCatch({
      res <- safe_fetch_with_cache("gainers|default", function() av_top_gainers_losers())
      if (!isTRUE(res$ok)) stop(res$error %||% "Fetch failed.")
      d <- res$data
      ng <- nrow(d$top_gainers %||% data.frame())
      if (ng == 0) {
        set_err(simpleError("No data returned. Check API key or rate limit (5/min, 25/day)."))
        gainers_data(NULL)
      } else {
        gainers_data(d)
        if (isTRUE(res$fallback)) fallback_msg(res$msg %||% "Using cached data.")
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
    clear_err(); clear_fallback()
    tryCatch({
      tk <- trimws(input$news_tickers %||% "")
      lim <- as.integer(input$news_limit)
      rk <- paste("news", tolower(tk), lim, sep = "|")
      res <- safe_fetch_with_cache(rk, function() av_news_sentiment(tickers = tk, limit = lim))
      if (!isTRUE(res$ok)) stop(res$error %||% "Fetch failed.")
      df <- res$data
      if (is.null(df) || nrow(df) == 0) {
        set_err(simpleError("No news returned. Check API key or rate limit (5/min, 25/day)."))
        news_df(NULL)
      } else {
        news_df(df)
        if (isTRUE(res$fallback)) fallback_msg(res$msg %||% "Using cached data.")
      }
    }, error = set_err)
  })
  output$ui_news_toolbar <- renderUI({
    df <- news_df(); if (is.null(df) || nrow(df) == 0) return(NULL)
    div(class = "toolbar",
        div(class = "toolbar-item", span(class = "toolbar-label", qmark("Show", "Maximum number of headlines to display.")), selectInput("news_n", NULL, choices = c(10, 25, 50), selected = 50)),
        div(class = "toolbar-item", span(class = "toolbar-label", qmark("Keyword", "Filter headlines containing this text.")), textInput("news_keyword", NULL, placeholder = "e.g. Fed")),
        div(class = "toolbar-item", span(class = "toolbar-label", qmark("Sentiment", "Filter by bullish, bearish, or neutral labels.")), selectInput("news_sentiment_filter", NULL, choices = c("All", "Bullish", "Bearish", "Neutral"), selected = "All")),
        div(class = "toolbar-item", checkboxInput("news_show_dist", qmark("Show sentiment mix", "Toggle sentiment distribution chart."), value = TRUE)),
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
  output$plot_news_sentiment <- renderPlot({
    if (!isTRUE(input$news_show_dist)) return(NULL)
    df <- news_filtered()
    if (is.null(df) || nrow(df) == 0 || !"overall_sentiment_label" %in% names(df)) return(NULL)
    lbl <- tolower(as.character(df$overall_sentiment_label %||% ""))
    cls <- ifelse(grepl("bullish|positive", lbl), "Bullish",
      ifelse(grepl("bearish|negative", lbl), "Bearish", "Neutral"))
    dist <- as.data.frame(table(factor(cls, levels = c("Bullish", "Neutral", "Bearish"))), stringsAsFactors = FALSE)
    names(dist) <- c("sentiment", "n")
    dist$n <- as.numeric(dist$n)
    if (!any(dist$n > 0)) return(NULL)
    ggplot(dist, aes(x = sentiment, y = n, fill = sentiment)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = n), vjust = -0.35, size = 4) +
      scale_fill_manual(name = "Sentiment", values = c(Bullish = "#059669", Neutral = "#64748b", Bearish = "#dc2626")) +
      labs(x = NULL, y = "Headline count", title = "Sentiment distribution (filtered set)") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  output$download_news_csv <- downloadHandler(filename = function() paste0("news_", Sys.Date(), ".csv"), content = function(file) { df <- news_df(); if (!is.null(df) && nrow(df) > 0) write.csv(df, file, row.names = FALSE) })
  output$table_news <- renderDT({
    df <- news_df(); if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    cols <- intersect(c("title", "time_published", "overall_sentiment_label", "source", "url"), names(df)); if (length(cols) == 0) cols <- names(df)
    DT::datatable(as.data.frame(df)[, cols], options = list(pageLength = 15), escape = FALSE)
  })

  # ---- Forex ----
  observeEvent(input$fetch_fx, {
    clear_err(); clear_fallback()
    tryCatch({
      from <- trimws(input$fx_from %||% ""); to <- trimws(input$fx_to %||% "")
      if (!nzchar(from) || !nzchar(to)) { set_err(simpleError("Enter From and To currencies.")); return() }
      rk <- paste("fx", toupper(from), toupper(to), sep = "|")
      res <- safe_fetch_with_cache(rk, function() av_fx_daily(from_symbol = from, to_symbol = to))
      if (!isTRUE(res$ok)) stop(res$error %||% "Fetch failed.")
      df <- res$data
      if (is.null(df) || nrow(df) == 0) {
        set_err(simpleError("No FX data returned. Check API key or rate limit (5/min, 25/day)."))
        fx_df(NULL)
      } else {
        fx_df(df)
        if (isTRUE(res$fallback)) fallback_msg(res$msg %||% "Using cached data.")
      }
    }, error = set_err)
  })
  output$ui_fx_toolbar <- renderUI({
    if (is.null(fx_df()) || nrow(fx_df()) == 0) return(NULL)
    div(class = "toolbar",
        div(class = "toolbar-item", span(class = "toolbar-label", qmark("Chart points", "How many historical points to display in charts.")), selectInput("fx_points", NULL, choices = c(30, 60, 90), selected = 90)),
        div(class = "toolbar-item", numericInput("fx_calc_amount", qmark("Amount", "Base currency amount for conversion calculator."), value = 1000, min = 0, step = 100)),
        div(class = "toolbar-item", numericInput("fx_calc_shift", qmark("Rate shock %", "One-time percentage shift used in stress calculation."), value = 1, min = -50, max = 50, step = 0.1)),
        div(class = "toolbar-item", numericInput("fx_scn_days", qmark("Scenario days", "Number of future periods in scenario projection."), value = 15, min = 1, max = 180, step = 1)),
        div(class = "toolbar-item", numericInput("fx_scn_drift", qmark("Scenario daily %", "Assumed daily % move for projected path."), value = 0.05, min = -5, max = 5, step = 0.01)),
        div(class = "toolbar-item", downloadButton("download_fx_csv", "Download CSV", class = "btn btn-default")))
  })
  output$ui_fx_summary <- renderUI({
    df <- fx_df(); if (is.null(df) || nrow(df) == 0) return(NULL); r <- df[1L, ]; rate <- as.numeric(r$close); if (!is.finite(rate)) return(NULL)
    div(class = "stats-row", div(class = "stat-card card-custom", h4("Latest rate"), div(class = "hero-rate", sprintf("%.4f", rate))), div(class = "stat-card card-custom", h4("Low (period)"), div(class = "value", sprintf("%.4f", min(df$close, na.rm = TRUE)))), div(class = "stat-card card-custom", h4("High (period)"), div(class = "value", sprintf("%.4f", max(df$close, na.rm = TRUE)))), div(class = "stat-card card-custom", h4("Days shown"), div(class = "value", as.character(nrow(df)))))
  })
  output$plot_fx <- renderPlot({
    df <- fx_df(); if (is.null(df) || nrow(df) == 0) return(NULL); pts <- as.integer(input$fx_points %||% 90); df <- head(df, pts)
    df <- df[!is.na(df$date) & is.finite(as.numeric(df$close)), ]; if (nrow(df) == 0) return(NULL)
    p <- ggplot(df, aes(x = date)) +
      geom_line(aes(y = close, color = "Observed")) +
      labs(x = "Date", y = "Close", title = NULL) + theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
    scn_days <- as.integer(input$fx_scn_days %||% 15)
    scn_drift <- as.numeric(input$fx_scn_drift %||% 0)
    if (is.finite(scn_days) && scn_days > 0 && is.finite(scn_drift)) {
      dfo <- df[order(df$date), ]
      last_dt <- tail(dfo$date, 1)
      last_px <- as.numeric(tail(dfo$close, 1))
      if (is.finite(last_px) && last_px > 0) {
        fut_dates <- seq.Date(last_dt + 1, by = "day", length.out = scn_days)
        fut_px <- last_px * cumprod(rep(1 + scn_drift / 100, scn_days))
        scn <- data.frame(date = fut_dates, close = fut_px)
        p <- p +
          geom_line(data = scn, aes(x = date, y = close, color = "Scenario"), linewidth = 1, linetype = "dotdash") +
          geom_point(data = scn[nrow(scn), , drop = FALSE], aes(x = date, y = close), color = "#f97316", size = 2) +
          labs(subtitle = "Orange path: user-defined FX scenario")
      }
    }
    p <- p + scale_color_manual(name = "Line", values = c("Observed" = "#0d9488", "Scenario" = "#f97316"))
    p
  })
  output$plot_fx_returns <- renderPlot({
    df <- fx_df(); if (is.null(df) || nrow(df) < 3L) return(NULL); pts <- as.integer(input$fx_points %||% 90); df <- head(df, pts)
    df <- df[order(df$date), ]
    ret <- 100 * (as.numeric(df$close) / dplyr::lag(as.numeric(df$close)) - 1)
    r2 <- data.frame(date = df$date, ret = ret)
    r2 <- r2[is.finite(r2$ret), ]
    if (nrow(r2) < 2) return(NULL)
    r2$direction <- ifelse(r2$ret >= 0, "Positive", "Negative")
    ggplot(r2, aes(x = date, y = ret)) +
      geom_col(aes(fill = direction), width = 0.9) +
      geom_smooth(aes(color = "Trend"), method = "loess", se = FALSE, linewidth = 0.8) +
      scale_fill_manual(name = "Return sign", values = c("Positive" = "#14b8a6", "Negative" = "#f97316")) +
      scale_color_manual(name = "", values = c("Trend" = "#334155")) +
      labs(x = "Date", y = "Return (%)", title = "FX return momentum with smooth trend") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  output$ui_fx_calculator <- renderUI({
    df <- fx_df(); if (is.null(df) || nrow(df) == 0) return(NULL)
    rate <- as.numeric(df$close[1]); if (!is.finite(rate) || rate <= 0) return(NULL)
    amt <- as.numeric(input$fx_calc_amount %||% 0)
    shock <- as.numeric(input$fx_calc_shift %||% 0)
    converted <- amt * rate
    shocked_rate <- rate * (1 + shock / 100)
    shocked_value <- amt * shocked_rate
    delta <- shocked_value - converted
    pair <- paste0(toupper(input$fx_from %||% "FROM"), "/", toupper(input$fx_to %||% "TO"))
    div(class = "card-custom",
      h4("FX conversion and stress calculator"),
      div(class = "stats-row",
        div(class = "stat-card card-custom", h4("Pair"), div(class = "value", pair)),
        div(class = "stat-card card-custom", h4("Current conversion"), div(class = "value", sprintf("%.2f", converted))),
        div(class = "stat-card card-custom", h4("Shocked value"), div(class = "value", sprintf("%.2f", shocked_value))),
        div(class = "stat-card card-custom", h4("Value change"), div(class = if (delta >= 0) "value positive" else "value negative", sprintf("%.2f", delta)))
      ),
      p(style = "margin: 0; color: var(--text-muted); font-size: 0.8rem;", "Stress result applies a simple percentage shock to latest close rate.")
    )
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
  observeEvent(input$fetch_commodity, {
    clear_err(); clear_fallback()
    tryCatch({
      com <- input$commodity %||% "WHEAT"
      int <- input$commodity_interval %||% "monthly"
      rk <- paste("commodity", com, int, sep = "|")
      res <- safe_fetch_with_cache(rk, function() av_commodity(commodity = com, interval = int))
      if (!isTRUE(res$ok)) stop(res$error %||% "Fetch failed.")
      commodity_df(res$data)
      if (isTRUE(res$fallback)) fallback_msg(res$msg %||% "Using cached data.")
    }, error = set_err)
  })
  output$ui_commodity_toolbar <- renderUI({
    if (is.null(commodity_df()) || nrow(commodity_df()) == 0) return(NULL)
    div(class = "toolbar",
        div(class = "toolbar-item", span(class = "toolbar-label", qmark("Chart points", "How many commodity observations to display.")), selectInput("commodity_points", NULL, choices = c(30, 60, 90), selected = 60)),
        div(class = "toolbar-item", numericInput("commodity_calc_units", qmark("Units", "Quantity used in notional and P/L scenario calculator."), value = 100, min = 0, step = 1)),
        div(class = "toolbar-item", numericInput("commodity_calc_move", qmark("Price move %", "One-time price change used in scenario notional."), value = 3, min = -80, max = 200, step = 0.5)),
        div(class = "toolbar-item", numericInput("commodity_scn_steps", qmark("Scenario steps", "Future periods projected on scenario line."), value = 12, min = 1, max = 120, step = 1)),
        div(class = "toolbar-item", numericInput("commodity_scn_drift", qmark("Scenario step %", "Assumed percent move per projected step."), value = 0.5, min = -30, max = 30, step = 0.1)),
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
    p <- ggplot(df, aes(x = .data[[xcol]])) + geom_line(aes(y = .data[[ycol]], color = "Observed"), linewidth = 1) + labs(x = xcol, y = ycol, title = NULL) + theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
    scn_steps <- as.integer(input$commodity_scn_steps %||% 12)
    scn_drift <- as.numeric(input$commodity_scn_drift %||% 0)
    if (is.finite(scn_steps) && scn_steps > 0 && is.finite(scn_drift)) {
      dfo <- df[order(df[[xcol]]), ]
      last_dt <- tail(dfo[[xcol]], 1)
      last_v <- as.numeric(tail(dfo[[ycol]], 1))
      if (inherits(last_dt, "Date") && is.finite(last_v) && last_v > 0) {
        fut_dates <- seq.Date(last_dt + 1, by = "day", length.out = scn_steps)
        fut_vals <- last_v * cumprod(rep(1 + scn_drift / 100, scn_steps))
        scn <- data.frame(date = fut_dates, value = fut_vals)
        p <- p +
          geom_line(data = scn, aes(x = date, y = value, color = "Scenario"), linewidth = 1, linetype = "dotdash") +
          geom_point(data = scn[nrow(scn), , drop = FALSE], aes(x = date, y = value), color = "#f59e0b", size = 2) +
          labs(subtitle = "Orange path: user-defined commodity scenario")
      }
    }
    p <- p + scale_color_manual(name = "Line", values = c("Observed" = "#0d9488", "Scenario" = "#f59e0b"))
    p
  })
  output$plot_commodity_changes <- renderPlot({
    df <- commodity_df()
    if (is.null(df) || nrow(df) < 3) return(NULL)
    pts <- as.integer(input$commodity_points %||% 60); df <- head(df, pts)
    xcol <- if ("date" %in% names(df)) "date" else names(df)[1]
    ycol <- if ("value" %in% names(df)) "value" else names(df)[2]
    df[[ycol]] <- as.numeric(df[[ycol]])
    df <- df[!is.na(df[[xcol]]) & is.finite(df[[ycol]]), ]
    if (nrow(df) < 3) return(NULL)
    df <- df[order(df[[xcol]]), ]
    df$chg <- 100 * (df[[ycol]] / dplyr::lag(df[[ycol]]) - 1)
    df <- df[is.finite(df$chg), ]
    if (nrow(df) == 0) return(NULL)
    df$direction <- ifelse(df$chg >= 0, "Increase", "Decrease")
    ggplot(df, aes(x = .data[[xcol]], y = chg, fill = direction)) +
      geom_col(width = 0.9) +
      geom_hline(yintercept = 0, color = "#94a3b8", linewidth = 0.6) +
      scale_fill_manual(name = "Change sign", values = c("Increase" = "#059669", "Decrease" = "#dc2626")) +
      labs(x = xcol, y = "Period change (%)", title = "Commodity change bars") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  output$ui_commodity_calculator <- renderUI({
    df <- commodity_df(); if (is.null(df) || nrow(df) == 0) return(NULL)
    ycol <- if ("value" %in% names(df)) "value" else names(df)[2]
    px <- as.numeric(df[[ycol]][1]); if (!is.finite(px) || px <= 0) return(NULL)
    units <- as.numeric(input$commodity_calc_units %||% 0)
    move <- as.numeric(input$commodity_calc_move %||% 0)
    base <- units * px
    shock_px <- px * (1 + move / 100)
    shock_val <- units * shock_px
    pnl <- shock_val - base
    div(class = "card-custom",
      h4("Commodity scenario calculator"),
      div(class = "stats-row",
        div(class = "stat-card card-custom", h4("Units"), div(class = "value", format(units, big.mark = ","))),
        div(class = "stat-card card-custom", h4("Current notional"), div(class = "value", sprintf("%.2f", base))),
        div(class = "stat-card card-custom", h4("Scenario notional"), div(class = "value", sprintf("%.2f", shock_val))),
        div(class = "stat-card card-custom", h4("Scenario P/L"), div(class = if (pnl >= 0) "value positive" else "value negative", sprintf("%.2f", pnl)))
      ),
      p(style = "margin: 0; color: var(--text-muted); font-size: 0.8rem;", "Simple sensitivity estimate using latest displayed value and user-defined percentage move.")
    )
  })
  output$table_commodity <- renderDT({ df <- commodity_df(); if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15))); DT::datatable(as.data.frame(df), options = list(pageLength = 15)) })
  output$download_commodity_csv <- downloadHandler(filename = function() paste0("commodity_", input$commodity %||% "", "_", Sys.Date(), ".csv"), content = function(file) { df <- commodity_df(); if (!is.null(df) && nrow(df) > 0) write.csv(df, file, row.names = FALSE) })

  # ---- Economic ----
  observeEvent(input$fetch_economic, {
    clear_err(); clear_fallback()
    tryCatch({
      ind <- input$economic_indicator
      int <- input$economic_interval
      rk <- paste("economic", ind, int, sep = "|")
      res <- safe_fetch_with_cache(rk, function() {
        if (ind == "TREASURY_YIELD") {
          av_economic(indicator = ind, interval = int, maturity = "10year")
        } else if (ind == "REAL_GDP") {
          av_economic(indicator = ind, interval = if (int %in% c("quarterly", "annual")) int else "annual")
        } else {
          av_economic(indicator = ind, interval = int)
        }
      })
      if (!isTRUE(res$ok)) stop(res$error %||% "Fetch failed.")
      economic_df(res$data)
      if (isTRUE(res$fallback)) fallback_msg(res$msg %||% "Using cached data.")
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
    cred <- pick_llm_credentials(); has_key <- isTRUE(cred$ok); loading <- ai_loading()
    report <- ai_report_text(); orch <- ai_orchestrator_text(); an <- ai_analyst_text(); ch <- report_choices()
    ragtr <- ai_rag_trace_text()
    needs_ai <- input$ai_report_type %||% "" %in% c("brief", "stock", "movers", "news", "forex_brief", "forex_snapshot", "commodity_brief", "commodity_snapshot", "economic_brief", "economic_snapshot")
    providers <- if (has_key) unique(vapply(cred$creds, function(z) z$provider, character(1))) else character(0)
    prov_lab <- if (length(providers) > 0) paste0(" (available: ", paste(providers, collapse = ", "), ")") else ""
    if (needs_ai && !has_key) return(div(class = "ai-reporter-card", div(class = "ai-title", "Multi-agent AI analysis"), div(class = "ai-desc", "LLM-backed reports need a key in .env (Ollama Cloud preferred, or OpenAI)."), div(class = "ai-reporter-connect", p(style = "margin: 0; font-size: 0.9rem;", "Ollama Cloud: ", tags$a(href = "https://ollama.com/settings", target = "_blank", "ollama.com/settings"), ". OpenAI: OPENAI_API_KEY=sk-... in .env."))))
    tagList(
      div(class = "ai-reporter-card",
        div(class = "ai-title", "Multi-agent AI analysis"),
        div(class = "ai-desc", paste0("Orchestrator → Market Analyst → Lead Editor run in sequence for each AI report", prov_lab, ". Context includes RAG notes from ", tags$code("data/rag_market_corpus.txt"), " (keyword retrieval) plus your fetched data. Trend reports use fetched data only (no LLM).")),
        selectInput("ai_report_type", "Report type", choices = ch, selected = if (!is.null(input$ai_report_type) && input$ai_report_type %in% ch) input$ai_report_type else ch[1]),
        if (needs_ai && has_key) {
          div(class = "ai-reporter-connect", style = "margin-top: 0.5rem;",
            p(style = "margin: 0; font-size: 0.78rem; color: var(--text-muted);", "RAG: edit ", tags$code("data/rag_market_corpus.txt"), " (chunks separated by ", tags$code("---"), ") to change reference notes. No LLM tool calling.")
          )
        } else NULL,
        actionButton("ai_generate", "Generate report", class = "btn-primary")
      ),
      if (loading) div(class = "card-custom pulse", p(style = "margin: 0; color: var(--text-muted);", "Running 3-agent pipeline…")) else NULL,
      if (!loading && !is.null(report) && nzchar(report)) div(class = "card-custom", style = "margin-top: 1rem;", h4("Final brief"), div(class = "ai-report-output", report)) else NULL,
      if (!loading && ((!is.null(orch) && nzchar(orch)) || (!is.null(an) && nzchar(an)) || (!is.null(ragtr) && nzchar(ragtr)))) div(class = "agent-pipeline",
        p(style = "margin: 0 0 0.5rem 0; font-size: 0.8rem; color: var(--text-muted);", "Agent traces (intermediate outputs)"),
        if (!is.null(orch) && nzchar(orch)) tags$details(class = "agent-step", open = FALSE, tags$summary("1. Orchestrator — plan & gaps"), div(class = "agent-body", orch)) else NULL,
        if (!is.null(an) && nzchar(an)) tags$details(class = "agent-step", open = FALSE, tags$summary("2. Market Analyst — evidence memo"), div(class = "agent-body", an)) else NULL,
        if (!is.null(ragtr) && nzchar(ragtr)) tags$details(class = "agent-step", open = FALSE, tags$summary("RAG retrieval (local corpus)"), div(class = "agent-body", ragtr)) else NULL
      ) else NULL
    )
  })
  observeEvent(input$ai_generate, {
    report_type <- input$ai_report_type %||% "brief"
    ai_loading(TRUE)
    ai_stage_text("Preparing analysis context...")
    ai_provider_trace(NULL)
    ai_report_text(NULL); ai_orchestrator_text(NULL); ai_analyst_text(NULL); ai_rag_trace_text(NULL)
    if (report_type == "stock_trend") { ai_loading(FALSE); ai_stage_text(NULL); ai_report_text(build_stock_trend_report()); return() }
    if (report_type == "forex_trend") { ai_loading(FALSE); ai_stage_text(NULL); ai_report_text(build_forex_trend_report()); return() }
    if (report_type == "commodity_trend") { ai_loading(FALSE); ai_stage_text(NULL); ai_report_text(build_value_trend_report(commodity_df(), paste0("Commodity (", input$commodity %||% "", ")"), "value")); return() }
    if (report_type == "economic_trend") { ai_loading(FALSE); ai_stage_text(NULL); ai_report_text(build_value_trend_report(economic_df(), paste0("Indicator (", input$economic_indicator %||% "", ")"), "value")); return() }
    cred <- pick_llm_credentials()
    if (!isTRUE(cred$ok)) { ai_loading(FALSE); ai_stage_text(NULL); return() }
    context <- build_ai_context(report_type)
    res <- tryCatch(
      withProgress(message = "Multi-agent analysis", value = 0, {
        ai_stage_text("Running orchestrator, analyst, and editor agents...")
        run_agentic_pipeline(
          report_type = report_type,
          section = input$section %||% "",
          context = context,
          cred = cred,
          progress_fn = function(value, detail) setProgress(value, detail = detail)
        )
      }),
      error = function(e) {
        list(ok = FALSE, error = conditionMessage(e), orchestrator = NULL, analyst = NULL, final = NULL, rag_trace = NULL)
      }
    )
    ai_loading(FALSE)
    ai_stage_text(NULL)
    if (!isTRUE(res$ok)) {
      if (!is.null(res$orchestrator)) ai_orchestrator_text(res$orchestrator)
      if (!is.null(res$analyst)) ai_analyst_text(res$analyst)
      rt_err <- res$rag_trace
      if (!is.null(rt_err) && nzchar(paste(rt_err, collapse = ""))) ai_rag_trace_text(paste(rt_err, collapse = "\n\n")) else ai_rag_trace_text(NULL)
      set_err(simpleError(res$error %||% "Analysis failed."))
      return()
    }
    if (!is.null(res$providers_used) && length(res$providers_used) > 0) ai_provider_trace(paste(res$providers_used, collapse = " | "))
    ai_orchestrator_text(res$orchestrator)
    ai_analyst_text(res$analyst)
    rt <- res$rag_trace
    if (!is.null(rt) && nzchar(paste(rt, collapse = ""))) ai_rag_trace_text(paste(rt, collapse = "\n\n")) else ai_rag_trace_text(NULL)
    ai_report_text(res$final)
  })
}

# RStudio "Run App" and shiny::runApp("app_market.R") must receive the app object as the
# last value. VS Code Code Runner uses Rscript on this file: --file=...app_market.R — then
# we start the server here (do not use interactive(); Rscript -e runApp() is non-interactive too).
app_market <- shinyApp(ui, server)
args_all <- commandArgs(trailingOnly = FALSE)
file_line <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_line)) sub("^--file=", "", file_line[1]) else ""
direct_run <- nzchar(script_path) && grepl("app_market\\.R$", script_path, ignore.case = TRUE)
if (direct_run && !interactive()) {
  shiny::runApp(app_market, launch.browser = TRUE)
} else {
  app_market
}
