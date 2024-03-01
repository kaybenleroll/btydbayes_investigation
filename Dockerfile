FROM rocker/tidyverse:4.3.2

#COPY build/Rprofile.site /usr/local/lib/R/etc/
#COPY build/Renviron.site /usr/local/lib/R/etc/

ENV TZ=Europe/Dublin

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
  && echo $TZ > /etc/timezone \
  && apt-get update \
  && apt-get upgrade -y \
  && apt-get install -y --no-install-recommends \
    byobu \
    clang \
    ditaa \
    graphviz \
    htop \
    less \
    libclang-dev \
    libglpk-dev \
    libgsl-dev \
    libnlopt-dev \
    libomp-dev \
    p7zip-full \
    pbzip2 \
    rsyslog \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && mkdir -p $HOME/.R \
  && echo "" > $HOME/.R/Makevars \
  && echo "CC=clang"                                               >> $HOME/.R/Makevars \
  && echo "CXX=clang++"                                            >> $HOME/.R/Makevars \
  && echo "CXXFLAGS=-Os"                                           >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+= -Wno-unused-variable -Wno-unused-function"   >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined"   >> $HOME/.R/Makevars \
  && echo ""                                                       >> $HOME/.R/Makevars \
  && echo "CC11=clang"                                             >> $HOME/.R/Makevars \
  && echo "CXX11=clang++"                                          >> $HOME/.R/Makevars \
  && echo "CXX11FLAGS=-Os"                                         >> $HOME/.R/Makevars \
  && echo "CXX11FLAGS+= -Wno-unused-variable -Wno-unused-function" >> $HOME/.R/Makevars \
  && echo "CXX11FLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo ""                                                       >> $HOME/.R/Makevars \
  && echo "CC14=clang"                                             >> $HOME/.R/Makevars \
  && echo "CXX14=clang++"                                          >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS=-Os"                                         >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS+= -Wno-unused-variable -Wno-unused-function" >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo ""                                                       >> $HOME/.R/Makevars \
  && echo "CC17=clang++"                                           >> $HOME/.R/Makevars \
  && echo "CXX17=clang++"                                          >> $HOME/.R/Makevars \
  && echo "CXX17FLAGS=-Os"                                         >> $HOME/.R/Makevars \
  && echo "CXX17FLAGS+= -Wno-unused-variable -Wno-unused-function" >> $HOME/.R/Makevars \
  && echo "CXX17FLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo ""                                                       >> $HOME/.R/Makevars \
  && install2.r --error \
    anytime \
    bayesplot \
    brms \
    BTYD \
    BTYDplus \
    CLVTools \
    conflicted \
    cowplot \
    DataExplorer \
    directlabels \
    evir \
    fitdistrplus \
    fs \
    furrr \
    loo \
    markdown \
    modeltime \
    posterior \
    projpred \
    prophet \
    pryr \
    quarto \
    rfm \
    rstan \
    rstanarm \
    rsyslog \
    sessioninfo \
    shinybrms \
    shinystan \
    snakecase \
    tictoc \
    tidybayes \
    tidyquant \
    timetk


WORKDIR /tmp

COPY build/docker_install_sys_rpkgs.R /tmp
RUN Rscript /tmp/docker_install_sys_rpkgs.R

RUN git clone https://github.com/lindenb/makefile2graph.git \
  && cd makefile2graph \
  && make \
  && make install
  
RUN cp -r $HOME/.R /home/rstudio \
  && chown -R rstudio:rstudio /home/rstudio/.R


WORKDIR /home/rstudio
USER rstudio

COPY build/conffiles.7z /tmp
RUN 7z x /tmp/conffiles.7z       \
  && cp conffiles/.bash*     .   \
  && cp conffiles/.gitconfig .   \
  && cp conffiles/.Renviron  .   \
  && cp conffiles/.Rprofile  .   \
  && mkdir -p .config/rstudio    \
  && cp conffiles/rstudio-prefs.json .config/rstudio/  \
  && rm -rfv conffiles/ \
  && touch /home/rstudio/.bash_eternal_history

COPY build/docker_install_user_rpkgs.R /tmp
RUN Rscript /tmp/docker_install_user_rpkgs.R


USER root

RUN chown -R rstudio:rstudio /home/rstudio \
  && chmod ugo+rx /home/rstudio \
  && chown -R rstudio:rstudio /home/rstudio/.cmdstan


ARG BUILD_DATE

LABEL org.opencontainers.image.source="https://github.com/kaybenleroll/btydbayes_investigation" \
      org.opencontainers.image.authors="Mick Cooney <mickcooney@gmail.com>" \
      org.label-schema.build-date=$BUILD_DATE


