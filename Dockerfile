# Use a specific version of the rocker/shiny base image
FROM rocker/shiny:4.1.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    pandoc \
    libcairo2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    librsvg2-dev \
    libpoppler-cpp-dev \
    libmagick++-dev \
    && rm -rf /var/lib/apt/lists/*

# Set up a work directory
WORKDIR /home/app

# Install additional R packages
RUN R -e "install.packages(c('rio', 'here', 'ggplot2', 'dplyr', 'tidyr', 'purrr', 'stringr', 'janitor', 'scales', 'plotly', 'shiny', 'shinythemes', 'shinydashboard', 'googlesheets4', 'googledrive', 'jsonlite', 'DT', 'waterfalls'), repos='http://cran.rstudio.com/', dependencies = TRUE)"
RUN R -e "install.packages(c('ragg', 'officer', 'rvg'), repos='http://cran.rstudio.com/', dependencies = TRUE)"
RUN R -e "install.packages('flextable', repos='http://cran.rstudio.com/', dependencies = TRUE)"

# Copy the service account JSON file into the Docker image
COPY GOOGLE_APPLICATION_CREDENTIALS.json /home/app/GOOGLE_APPLICATION_CREDENTIALS.json

# Set the environment variable to point to the file
ENV GOOGLE_APPLICATION_CREDENTIALS=/home/app/GOOGLE_APPLICATION_CREDENTIALS.json

# Create an 'app' user and group to run the app
RUN groupadd app && useradd -r -g app app

# Copy the Shiny app files to the image
COPY . /home/app
RUN ls -la /home/app

# Change ownership of the copied files
RUN chown -R app:app /home/app

# Switch to the 'app' user
USER app

# Make the ShinyApp available at port 3838
EXPOSE 3838

# Serve the R Markdown document as a Shiny app
CMD ["R", "-e", "shiny::runApp('/home/app', port = 3838, host = '0.0.0.0')"]
