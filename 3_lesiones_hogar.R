#activacion de librerias####
library(readr)
library(tidyverse)
library(dplyr)
library(knitr)
library(ggplot2)
library(readxl)
library(highcharter)
library(lubridate)
library(gridExtra) 


#preparacion de base agrupada----
{
  #lectura de base y chequeo de formato dataframe
  agrupada <- read.csv("NEUQUEN_CLI.CSV", sep = ";",na.strings = "")
  sum(complete.cases(agrupada))
  
  nrow(agrupada)
  
  #selecciono hasta la SE de mi BEM
  agrupada <- agrupada %>%
    filter(ANIO <= 2024 & SEMANA <= 53)
  



#agrego columna region----
{
  #lectura de la base 
  regiones <- read_excel("REGIONES.xlsx")
  regiones <- as.data.frame(regiones) #cambio formato a dataframe
  
  
  #cruzo base de datos (es como un buscar V en excel)
  ?left_join
  
  regiones_duplicadas <- regiones %>% #esto lo hago porque habia duplicados que agregaban observaciones a "agrupadas"
    group_by(LOCALIDAD) %>%
    filter(n() > 1)
  
  regiones <- regiones%>%
    distinct(LOCALIDAD, .keep_all = TRUE)
  
  #agrego la columna regiones e ID regiones a agrupadas
  agrupada <- agrupada %>%
    left_join(regiones, by = "LOCALIDAD")
  
}
#columna se-año, SE MAX, MIN----
{#armo la variable SE-año
  agrupada <- agrupada %>%
    mutate(SE_ANIO= paste(SEMANA, ANIO, sep = "-"))
  
  ##SE/año del BEM-----
  #las SE/año del BEM de este mes son 48,49,50,51 y 52 (esto es un ayuda memoria) del año 2024#SE maxima
  SE_BEM <- c(48, 49, 50, 51, 52) #hay que cambiarlo mensualmente
  SE_BEM <- as.vector(SE_BEM)
  
  #ANIO maximo (lo voy a usar para tablas, gráficos)
  ANIO_max <- agrupada %>% 
    summarise(ANIO_max = max(ANIO, na.rm = TRUE))
  
  ANIO_max <- as.numeric(ANIO_max[[1]])  # Extrae la primera columna y lo convierte a numérico
  
  
  view(ANIO_max)
  
  #SE_min 
  SE_min <- SE_BEM %>%
    min()
  
  #SE_max 
  SE_max <- SE_BEM %>%
    max()
}
  
  
#Lesiones en el hogar---------
  lesiones_hogar <- agrupada %>% 
    filter(IDEVENTOAGRUPADO==116) %>% 
    filter((ANIO > 2023) | (ANIO == 2023 & SEMANA >= 21))
  
  #Cantidad segun SE BEM
  lesiones_hogar_SE_BEM <- lesiones_hogar %>%
    filter(ANIO == 2024, SEMANA   %in% c(48, 49, 50, 51, 52)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
    summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") 
 #----   
    
#hago una tabla para ver los eventos de este grupo
lesiones_eventos <- lesiones_hogar %>%
      filter(SEMANA %in% SE_BEM,                                     # Filtrar por el valor de SE_BEM
        ANIO == ANIO_max) %>%                     # Filtrar por el año máximo
     group_by(NOMBREEVENTOAGRP) %>%                     # Agrupar por los eventos
      summarize(
        Total = sum(CANTIDAD, na.rm = TRUE),                # Calcular el total
        .groups = "drop")
    
#contruccion del indicador ----   
    # Datos simulados
    indicadores <- data.frame(
      Categoria = c("Caídas y golpes", "Cortes y quemaduras", "Sin especificar", "Lesiones por electrocución", "Ahogamiento por inmersión", "Otras"),
      Valor = c(lesiones_total))
    
    # Función para crear recuadros con centro claro
    crear_recuadro <- function(categoria, valor, variacion, color_fondo, color_numero) {
      # Dividir el texto del título en dos líneas si es demasiado largo
      titulo_wrapped <- stringr::str_wrap(categoria, width = 20)
      
      ggplot() +
        # Fondo del recuadro
        annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = color_fondo, alpha = 0.9) +
        # Centro más claro
        annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.3, ymax = 0.7, fill = "white", alpha = 0.8) +
        # Título en dos líneas, más pequeño
        annotate("text", x = 0.5, y = 0.9, label = titulo_wrapped, size = 4, fontface = "bold", color = "white", lineheight = 0.8) +
        # Número en el centro
        annotate("text", x = 0.5, y = 0.5, label = format(valor, big.mark = ","), size = 8, fontface = "bold", color = color_numero) +
        # Nota de variación
        annotate("text", x = 0.5, y = 0.15, label = paste("Variación:", variacion, "%"), size = 3.5, color = "white") +
        theme_void()
    
    # Recuadro superior con centro más claro
    recuadro_superior <- ggplot() +
      # Fondo del recuadro
      annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#4286f5",  alpha = 0.9) +
      # Centro más claro
      annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.3, ymax = 0.7, fill = "white", alpha = 0.8) +
      # Texto del título
      annotate("text", x = 0.5, y = 0.8, label = "Internaciones por lesiones en el hogar", size = 5, fontface = "bold", color = "white") +
      # Número en el centro
      annotate("text", x = 0.5, y = 0.5, label = "30,000", size = 10, fontface = "bold", color = "#003366") +
      theme_void()
    
    # Crear gráficos para los indicadores secundarios
    graficos <- list(
      crear_recuadro("Caídas y golpes", 24217, 29.2, "#4286f5", "#003366"),
      crear_recuadro("Cortes y quemaduras", 1747, 31.5, "#4286f5", "#003366"),
      crear_recuadro("Sin especificar", 377, 41.7, "#4286f5", "#003366"),
      crear_recuadro("Lesiones por electrocución", 1300, 15.3, "#4286f5", "#003366"),
      crear_recuadro("Ahogamiento por inmersión", 1300, 15.3, "#4286f5", "#003366"),
      crear_recuadro("Otras", 1300, 15.3, "#4286f5", "#003366"),
      )
  
    
    # Organizar el recuadro superior y los indicadores secundarios
    botonera_1 <- grid.arrange(
      recuadro_superior, 
      arrangeGrob(grobs = graficos, ncol = 6), #columnas en las que se organizan los graf abajo
      heights = c(1, 2) 
      # Ajusta la proporción de alturas
    )
    print(botonera_1)  




#tabla_lesiones_ evolutivo
lesiones_evolutivo <- lesiones_hogar %>%
  group_by(SEMANA,ANIO) %>%  
  summarize(Total = sum(CANTIDAD, na.rm = TRUE), .groups = "drop")%>%   # Suma la columna cantidad, ignorando NA
  mutate(ANIO_SE = paste(SEMANA, ANIO, sep = "-")) %>% 
  arrange(ANIO, SEMANA) %>% 
  mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE))) %>% 
  as.data.frame()



#objeto n lesiones
lesiones_total <- sum(lesiones_evolutivo$Total, na.rm = TRUE)



#grafico evolutivo lesiones

lesiones_grafico_evolutivo <- lesiones_evolutivo %>% 
  ggplot(aes(x = ANIO_SE,  y = Total)) +
  geom_bar(stat = "identity", fill = "#21618c", width = 0.5) +
  scale_x_discrete(
    breaks = levels(lesiones_evolutivo$ANIO_SE)[seq(1, length(levels(lesiones_evolutivo$ANIO_SE)), by =3)])+
  scale_y_continuous(
    breaks = seq(0, max(lesiones_evolutivo$Total, na.rm = TRUE), by = 10),
    expand = c(0, 0)) +
  labs(
    x = "SE-año",
    y = "Lesiones en el hogar") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank())

# Mostrar el gráfico
lesiones_grafico_evolutivo 
    }
    