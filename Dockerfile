# Use the rocker/shiny base image
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    pandoc \
    pandoc-citeproc \
    && rm -rf /var/lib/apt/lists/*

# Install renv & restore packages
COPY renv.lock /srv/shiny-server/renv.lock
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
RUN R -e "setwd('/srv/shiny-server'); renv::restore()"

# Make the ShinyApp available at port 3838
EXPOSE 3838

# Serve the R Markdown document as a Shiny app
CMD ["R", "-e", "rmarkdown::run('/srv/shiny-server/app.R', shiny_args = list(port = 3838, host = '0.0.0.0'))"]