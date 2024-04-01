FROM shogi/dev_env_r_austemp:4.3.2

RUN mkdir /home/austemp

RUN mkdir /home/austemp/pipeline_output

RUN mkdir /home/austemp/shared_folder

COPY renv.lock /home/austemp/renv.lock

COPY weather_graphs.Rmd /home/austemp/weather_graphs.Rmd

COPY _targets.R /home/austemp/_targets.R

RUN R -e "setwd('/home/austemp');renv::init();renv::restore()"

RUN cd /home/austemp && R -e "targets::tar_make()"

CMD mv /home/austemp/pipeline_output/* /home/austemp/shared_folder/
