#esto es un .R que tiene como objetivo realizar source con otros. 

#librerias
library(readr)
library(tidyverse)
library(dplyr)
library(knitr)
library(ggplot2)
library(readxl)
library(lubridate)
library(stringr)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("treemapify")
library(treemapify)
#install.packages("ggtext")
library(ggtext)


#cargo las fuentes instaladas en mi sistema para poder utilizar la del ministerio---------
#install.packages("extrafont")
library(extrafont)


#loadfonts(device = "win", quiet = TRUE)
#fonts()
#font_import(prompt = FALSE)  # Escanea todas las fuentes del sistema



#correr las 2 lineas juntas, source y message. Seleccionar ambas y ctrl+enter o "Run".
#si el codigo corrio correctamente, van a ver el message en la consola-

source("1_general.R")
message("1_general.R ejecutado sin problemas. Pasar al siguiente source")


#source("2_respiratorias.R")
message("2_respiratorias.R ejecutado sin problemas. Pasar al siguiente source")


source("3_diarreas.R")
message("3_diarreas.R ejecutado sin problemas. Pasar al siguiente source")


#source("4_lesiones_hogar.R")
source("4B_lesiones_hogar.R")
message("4_lesiones_hogar.R ejecutado sin problemas. Pasar al siguiente source")


source("5_mordeduras.R")
message("5_mordeduras.R ejecutado sin problemas. Pasar al siguiente source")


source("6_siniestros viales.R")
message("6_siniestros viales.R ejecutado sin problemas. Pasar al siguiente source")


#source("7_sifilis.R")
message("7_sifilis.R ejecutado sin problemas. Pasar al siguiente source")


source("8_inmunoprevenibles.R")
message("8_inmunoprevenibles.R ejecutado sin problemas. Pasar al siguiente source")



