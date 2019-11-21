FROM openanalytics/r-base

MAINTAINER Rafael Pereira "r.s.p.models@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0\
    libpoppler-cpp-dev \
    libxml2-dev
# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/',dependencies=TRUE)"

# install dependencies of the Analysis app
RUN R -e "install.packages(c('pdftools','plotly','fields','reshape2','cluster','tm'), repos='https://cloud.r-project.org/',dependencies=TRUE)"

RUN R -e "install.packages(c('tm', 'SnowballC','wordcloud','RColorBrewer'), repos='https://cloud.r-project.org/',dependencies=TRUE)"
RUN R -e "install.packages(c('tidytext','dplyr'), repos='https://cloud.r-project.org/',dependencies=TRUE)"
RUN R -e "install.packages(c('igraph','ggraph'), repos='https://cloud.r-project.org/',dependencies=TRUE)" 

# copy the app to the image
#RUN mkdir /root/Exploration
#COPY APPLastVersion.R /root/Exploration
#COPY Encontrar_candidatos_dataset_v1.R /root/Exploration
#COPY Rprofile.site /usr/lib/R/etc/
COPY app.R  app.R
COPY Analise_texto.R Analise_texto.R
COPY WorldCloud.R WorldCloud.R
COPY Ngrams.R Ngrams.R
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('app.R',port=3838,host='0.0.0.0',launch.browser=FALSE)"]
#CMD ["R", "-e", "shiny::runApp('/home/rafael/Downloads/APPs/DataExploration/APPLastVersion.R')"]
