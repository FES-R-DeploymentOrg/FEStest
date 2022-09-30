FROM rocker/shiny

RUN R -e "install.packages(c('shinywidgets', 'readxl', 'ggplot2', 'shinydashboard', 'shinythemes', 'DT', 'dplyr', 'ggridges', 'shinyjs'),dependencies=TRUE, repos='https://cran.r-project.org/')"

COPY *.R /srv/shiny-server/
COPY www /srv/shiny-server/www
EXPOSE 3838

RUN sudo chown -R shiny:shiny /srv/shiny-server

COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN ["chmod", "+x", "usr/bin/shiny-server.sh"]

CMD ["/usr/bin/shiny-server.sh"]