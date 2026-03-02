# Shiny app - use R base image (similar pattern to rstudio/plumber for APIs)
FROM rocker/r-ver:4.3.2

# System deps for R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny','httr','httr2','jsonlite','ggplot2','DT','dplyr'), repos='https://cloud.r-project.org')"

# Copy app files
WORKDIR /app
COPY app_market.R run.R /app/

# Expose port (platforms like Render set PORT at runtime)
EXPOSE 3838

# Run the Shiny app
CMD ["Rscript", "/app/run.R"]
