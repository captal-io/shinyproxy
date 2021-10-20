FROM rocker/tidyverse:4.1.0

RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("rstudio/shiny@v1.6.0")'

RUN mkdir /root/app
COPY global.R /root/app/global.R
COPY ui.R /root/app/ui.R
COPY server.R /root/app/server.R

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app')"]
