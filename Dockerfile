FROM rocker/r-ver:4.3.3

# System dependencies for common R packages used by the app
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
  && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('shiny','httr2','jsonlite','ggplot2','DT','dplyr'), repos = 'https://cloud.r-project.org')"

# Copy app code
WORKDIR /app
COPY . /app

# Expose default Shiny port (actual port is controlled by $PORT for platforms like Render)
EXPOSE 3838

# Run the Shiny app, binding to 0.0.0.0 and honoring $PORT from the platform
CMD [\"R\", \"-e\", \"shiny::runApp('app_market.R', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 3838)))\"]

