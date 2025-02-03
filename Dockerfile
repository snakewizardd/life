FROM rocker/rstudio:latest

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    default-jdk \ 
    r-cran-rjava \
    libgdal-dev \
    libproj-dev \
    software-properties-common \
    curl 


RUN DEBIAN_FRONTEND=noninteractive sudo apt-get install -y xorg
RUN DEBIAN_FRONTEND=noninteractive sudo apt-get install -y libx11-dev 
RUN DEBIAN_FRONTEND=noninteractive sudo apt-get install -y libglu1-mesa-dev 
RUN DEBIAN_FRONTEND=noninteractive sudo apt-get install -y libfreetype6-dev 

RUN R -e "install.packages('rgl', repos ='http://cran.rstudio.com/')"

RUN R -e "install.packages('dplyr', repos ='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos ='http://cran.rstudio.com/')"

# Install Rust and Cargo  
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y  
ENV PATH="/root/.cargo/bin:${PATH}"  

RUN R -e "install.packages('gganimate', repos ='http://cran.rstudio.com/')"

RUN R -e "install.packages('gifski', repos ='http://cran.rstudio.com/')"

RUN  yes | sudo apt install libudunits2-dev

RUN R -e "install.packages('transformr', repos ='http://cran.rstudio.com/')"

RUN  yes | sudo apt install libglpk-dev


EXPOSE 8787