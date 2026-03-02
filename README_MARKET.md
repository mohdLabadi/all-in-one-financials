# Daily Market Analysis Tool (Alpha Vantage)

A Shiny app for daily market analysis, separate from the Crime Analysis project. Data is fetched from the Alpha Vantage API.

## Files

- **`.env`** – Store API keys here (do not commit; in `.gitignore`): `ALPHAVANTAGE_API_KEY` (required for market data), `OLLAMA_CLOUD_API_KEY` or `OPENAI_API_KEY` (optional, for AI Reporter only).
- **`scripts/load_env.R`** – Loads environment variables from `.env`.
- **`scripts/fetch_alphavantage.R`** – Optional; API logic is inlined in `app_market.R` (stock daily, gainers/losers, news, forex, commodities, economic indicators).
- **`app_market.R`** – Main Shiny app file.
- **`run_market.R`** – Launcher script: sets working directory and runs the app (recommended).

## Dependencies

Install these R packages once:

```r
install.packages(c("shiny", "httr2", "jsonlite", "ggplot2", "DT", "dplyr"))
```

If `httr2` is not installed, the app automatically falls back to `httr`.

## Step‑by‑step setup (reproduce the app)

1. **Clone or copy the project**
   - Put all files (including `.env`, `app_market.R`, `run_market.R`) in a folder, for example `5381tool1`.

2. **Create `.env` and add keys**
   - Get a **free Alpha Vantage key** from `https://www.alphavantage.co/support/#api-key`.
   - Get an **Ollama Cloud key** from `https://ollama.com/settings/keys`.
   - In the project folder, create a file named `.env` with:

     ```text
     ALPHAVANTAGE_API_KEY=your_alpha_key_here
     OLLAMA_CLOUD_API_KEY=your_ollama_key_here
     ```

   - Keep `.env` private; it should not be committed to git.

3. **Open R or RStudio and set the working directory**

   ```r
   setwd("/path/to/5381tool1")  # adjust the path for your machine
   source("run_market.R")
   ```

   This launches the Shiny app in your browser.

4. **Use the app**
   - Choose a section in the sidebar (e.g. `Stock Daily`, `Top Gainers/Losers`, `News & Sentiment`, `Forex`, etc.).
   - Enter any inputs (symbol, currencies, etc.) and click **Fetch** to load data.
   - Use the toolbar to change the view, filter results, or **Download CSV**. These actions reuse already-fetched data (no extra API calls).

5. **Try the AI Reporter (Ollama Cloud)**
   - Go to the **AI Reporter** section.
   - Optionally first fetch some data (e.g. gainers or news) to give the AI more context.
   - Click **Generate report** to get a short AI-written summary. This uses only `OLLAMA_CLOUD_API_KEY` (it does not consume your Alpha Vantage quota).

## Sections

| Section                  | API / Description                                      |
|--------------------------|--------------------------------------------------------|
| Stock Daily              | `TIME_SERIES_DAILY` – enter a symbol (e.g. IBM, AAPL)  |
| Top Gainers/Losers       | `TOP_GAINERS_LOSERS` – gainers, losers, most active    |
| News & Sentiment         | `NEWS_SENTIMENT` – optional ticker filter              |
| Forex                    | `FX_DAILY` – choose From/To currency pair              |
| Currency Exchange Rate   | Realtime rate (premium) or latest from FX_DAILY (free) |
| Commodities              | WHEAT, CORN, WTI, BRENT, etc. – choose interval        |
| Economic Indicators      | CPI, UNEMPLOYMENT, TREASURY_YIELD, etc.                 |
| AI Reporter              | Optional. Set `OLLAMA_CLOUD_API_KEY` or `OPENAI_API_KEY` in `.env` to generate AI market summaries from your data. |

Select a section and click **Fetch** to load data. View, filter, and Download use already-loaded data (no extra API calls). Use **AI Reporter** for narrative insights.

## API limits (free key)

- **5 requests per minute**
- **~25 requests per day**

If you see no data or a rate-limit error, wait a minute or try again the next day. The app displays a short reminder and clearer error messages when limits are hit.

## AI Reporter (optional)

The AI Reporter uses a separate key from Alpha Vantage and runs on **Ollama Cloud**.

- Get a key from [ollama.com/settings](https://ollama.com/settings). 
- Add to `.env`: `OLLAMA_CLOUD_API_KEY=your-key`.
