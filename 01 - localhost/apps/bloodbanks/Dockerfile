FROM rocker/tidyverse:4.1.0

RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("rstudio/shiny@v1.6.0")'
RUN R -e 'remotes::install_github("rstudio/leaflet@v2.0.3")'
RUN R -e 'remotes::install_github("rstudio/DT@v0.19")'

RUN mkdir /root/app
COPY ui.R /root/app/ui.R
COPY server.R /root/app/server.R
COPY blood-banks.csv /root/app/blood-banks.csv
COPY readme.md /root/app/blood-banks_raw.csv
COPY readme.md /root/app/readme.md

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app')"]
