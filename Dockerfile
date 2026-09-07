FROM --platform=linux/amd64 rocker/shiny:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libicu-dev \
    libuv1-dev \
    gdal-bin \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    pkg-config \
    git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy renv files and restore packages
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/

RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages('renv'); renv::restore(rebuild = TRUE)"

# Copy app sources
COPY . .

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
