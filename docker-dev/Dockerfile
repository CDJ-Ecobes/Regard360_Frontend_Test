# Environnement de developpement local pour le front-end Shiny Regard360.
# R 4.2.2 : version exigee par renv.lock.
# rocker/rstudio fournit RStudio Server et la chaine de compilation.
FROM rocker/rstudio:4.2.2

# Dependances systeme des paquets geospatiaux (sf, terra, s2, units)
# et du rendu graphique (ragg, svglite, textshaping).
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    libfreetype6-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libicu-dev \
    zlib1g-dev \
    make \
 && rm -rf /var/lib/apt/lists/*

# Depot Posit servant des binaires Linux precompiles pour Ubuntu 22.04.
# Sans cette surcharge, renv compile les 167 paquets depuis les sources.
ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/jammy/latest

# La bibliotheque et le cache renv restent hors du dossier monte,
# pour ne pas melanger binaires Linux et bibliotheque Windows.
ENV RENV_PATHS_LIBRARY=/opt/renv/library
ENV RENV_PATHS_CACHE=/opt/renv/cache
RUN mkdir -p /opt/renv/library /opt/renv/cache && chown -R rstudio:rstudio /opt/renv

RUN R -e "install.packages('renv', repos = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest')"

EXPOSE 8787
