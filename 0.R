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

#SOURCES

#AGRUPADA. PREPARACION DEL DATASET AGRUPADA (POR EJ CON FILTROS Y SELECTOR DE SE DEL BEM)
source("1_agrupada.R") #LISTO


#VR nominal. Acá se aplican las funciones y algoritmo para pasar de una base multirregistro a una de variables dicotómicas.
source("2_VRNOMINAL.R") #LISTO

#indicador de eventos respi. Conjuga IRAG (nominal), BQL, ETI y NAC (agrupadas). También hay texto automatizado acá
source("2A_Respi_indicador.R") #LISTO


#IRAG nominal. Gráficos de notificación por SE y grupos de edad. 
source("2B_IRAG.R") #LISTO

#ETI/NAC/BQL//AGRUPADA. Esto es para un gráfico de los 3 eventos juntos según grupo de edad y para texto automatizado
source("1A_ETI_NAC_BQL.R") #LISTO

#ETI//AGRUPADA
source("1B_ETI.R") #LISTO

#NAC//AGRUPADA
source("1C_NAC.R") #LISTO

#BQL//AGRUPADA
source("1D_BQL.R") #LISTO

#DIARREA AGUDA//AGRUPADA
source("1E_DA.R") #LISTO

#LESIONES EN EL HOGAR//AGRUPADA
source("1F_lesiones_hogar.R") #LISTO

#MORDEDURAS//AGRUPADA
source("1G_mordeduras_perros.R") #LISTO

#SINIESTROS VIALES//AGRUPADA
source("1H_siniestros viales.R") #LISTO

#INMUNOPREVENIBLES//AGRUPADA
source("1I_inmunoprevenibles.R")#LISTO



