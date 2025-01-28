#Lesiones por mordedura de perro (ambulatorias)----
library(readr)
library(tidyverse)
library(dplyr)
library(knitr)
library(ggplot2)
library(readxl)
library(highcharter)
library(lubridate)
library(gridExtra) #para las tablitas de indicadores

#preparacion de base agrupada----

#lectura de base y chequeo de formato dataframe
  agrupada <- read.csv("NEUQUEN_CLI.CSV", sep = ";",na.strings = "")
  sum(complete.cases(agrupada))
  
  nrow(agrupada)
  
  #selecciono hasta la SE de mi BEM
  agrupada <- agrupada %>%
    filter(ANIO <= 2024 & SEMANA <= 53) #CAMBIAR EL AÑO CUANDO CORRESPONDA
  
  #agrego columna region----
  {
    #lectura de la base 
    regiones <- read_excel("REGIONES.xlsx")
    regiones <- as.data.frame(regiones) #cambio formato a dataframe
    
    
    #cruzo base de datos (es como un buscar V en excel)
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
    
    ##SE/año del BEM
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


{#mordeduras evolutivo   
  mordedura_perro <- agrupada %>% 
    filter(ID_SNVS_EVENTO_AGRP %in% c(341, 507, 508, 509, 512))
  
  
  #tabla mordedura evolutivo
  
  mordeduras_evolutivo <- mordedura_perro %>%
    group_by(SEMANA, ANIO) %>%  
    summarise(total_cantidad = sum(CANTIDAD), .groups = "drop") %>%  # Suma la columna cantidad, ignorando NA
    arrange(ANIO, SEMANA) %>% 
    mutate(SE_ANIO = paste(ANIO, SEMANA, sep = "-")) %>%  # Crear la variable como texto: "ANIO-SEMANA"
    mutate(SE_ANIO = factor(SE_ANIO, levels = unique(SE_ANIO)))  # Convertir a factor, asegurando el orden cronológico
  
  
  #objeto n mordeduras acumulado
  mordeduras_total <- sum(mordeduras_evolutivo$total_cantidad, na.rm = TRUE)
  
  
  #objeto n mordeduras SE BEM
 mordeduras_cantidad_SE_BEM <- mordedura_perro %>%
    filter(ANIO == 2024, SEMANA   %in% c(48, 49, 50, 51, 52)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
    summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE))
  
  
  
  #objeto n mordeduras SE BEM año anterior (es para variacion %)
 mordeduras_cantidad_SE_BEManterior <- mordedura_perro %>%
 filter(ANIO == 2023, SEMANA   %in% c(SE_BEM)) %>% #Es importante ir cambiando el año y las SE según corresponda al BEM
   summarise(total_Cantidad = sum(CANTIDAD, na.rm = TRUE))
 
 #variacion porcentual 
 mordedura_variacion_porcentual <- 
   round((mordeduras_cantidad_SE_BEM - mordeduras_cantidad_SE_BEManterior)/mordeduras_cantidad_SE_BEManterior*100,1)
 
 
 
 
#Indicadores 
 {
   
#objetos para el set de indicadores
   #hago una tabla para ver los eventos de este grupo
   mordeduras_eventos <- mordedura_perro %>%
     filter(SEMANA %in% SE_BEM,                                     # Filtrar por el valor de SE_BEM
            ANIO == ANIO_max) %>%                     # Filtrar por el año máximo
     group_by(NOMBREEVENTOAGRP, ID_SNVS_EVENTO_AGRP) %>%                     # Agrupar por los eventos
     summarize(
       Total = sum(CANTIDAD, na.rm = TRUE),                # Calcular el total
       .groups = "drop")
   
   
   
#perro conocido via publica
   perro_conocido <- mordeduras_eventos%>% 
     filter(ID_SNVS_EVENTO_AGRP == 509) %>% 
     summarise(Total= sum(Total)) %>% 
     pull(Total)
   
   
#perro desconocido via publica
   perro_desconocido <- mordeduras_eventos%>% 
     filter(ID_SNVS_EVENTO_AGRP == 508) %>% 
     summarise(Total= sum(Total)) %>% 
     pull(Total)
   
#perro vivienda 
   perro_vivienda <- mordeduras_eventos%>% 
     filter(ID_SNVS_EVENTO_AGRP == 507) %>% 
     summarise(Total= sum(Total)) %>% 
     pull(Total)

#perro sin especificar
   perro_sinespecificar <- mordeduras_eventos%>% 
     filter(ID_SNVS_EVENTO_AGRP == 512) %>% 
     summarise(Total= sum(Total)) %>% 
     pull(Total)  
   

#construccion de indicadores
   
   # Crear el dataframe indicadores
indicadores <- data.frame(
     Categoria = c("Perro conocido en la vía pública", "Perro desconocido en la vía pública", "En la vivienda", 
                   "Sin especificar"),
     Valor = c(perro_conocido,perro_desconocido, perro_vivienda, perro_sinespecificar),
     Variacion = c(mordedura_variacion_porcentual))
   
   # Función para crear recuadros
   crear_recuadro <- function(categoria, valor, color_fondo, color_numero) 
     ggplot() +
       annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = color_fondo, alpha = 0.9) +
       annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.3, ymax = 0.7, fill = "white", alpha = 0.8) +
       annotate("text", x = 0.5, y = 0.8, label = categoria, size = 4, fontface = "bold", color = "white") +
       annotate("text", x = 0.5, y = 0.5, label = valor, size = 8, fontface = "bold", color = color_numero) +
       theme_void()
   
   # Recuadro superior
   recuadro_superior <- ggplot() +
     annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#c0de00", alpha = 0.9) +
     annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.3, ymax = 0.7, fill = "white", alpha = 0.8) +
     annotate("text", x = 0.5, y = 0.8, label = "Lesiones por mordedura de perro", 
              size = 5, fontface = "bold", color = "white") +
     annotate("text", x = 0.5, y = 0.5, label = sum(indicadores$Valor), 
              size = 10, fontface = "bold", color = "#00a44a") +
     # Variación porcentual en la banda inferior
     annotate("text", x = 0.1, y = 0.165, label = "Variación", size = 4, fontface = "bold", color = "#00a44a", hjust = 0) +
     annotate("text", x = 0.9, y = 0.165, label = mordedura_variacion_porcentual, size = 4, fontface = "bold", color = "#00a44a", hjust = 1) +
     theme_void()
 
   
   # Crear gráficos individuales
   graficos <- lapply(1:nrow(indicadores), function(i) 
     crear_recuadro(
       categoria = indicadores$Categoria[i],
       valor = indicadores$Valor[i],
       color_fondo = "#c0de00",
       color_numero = "#00a44a"))
   
   # Organizar los gráficos en la botonera
   indicador_mordedura <- grid.arrange(
     recuadro_superior,
     arrangeGrob(grobs = graficos, ncol = 4), # Ajusta ncol según el número de categorías
     heights = c(1, 2))
   
   # Mostrar el gráfico final
   print(indicador_mordedura)
   
     }  
  
 
 #grafico evolutivo
{
  mordeduras_grafico_evolutivo <- mordeduras_evolutivo %>% 
    ggplot(aes(x = SE_ANIO, y = total_cantidad)) +
    geom_bar(stat = "identity", fill = "#7dd473", width = 0.5) +
    scale_x_discrete(
      breaks = levels(mordeduras_evolutivo$SE_ANIO)[seq(1, length(levels(mordeduras_evolutivo$SE_ANIO)), by = 5)],
      expand = c(0, 0)) +  
    labs(
      x = "SE-año",
      y = "Casos de mordeduras de perros") +
    theme_classic() +
    theme(
      axis.title = element_text(size = 12),
      axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 12),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank())
  mordeduras_grafico_evolutivo
}

 
#mordeduras incidencia
{
  #calculo de incidencia
  #cargo la base de poblaciones (es la misma que están usando en el BEM)
  poblacion <- read_excel("Poblacion.xlsx")
  
  #armo la tabla
  
  mordedura_incidencia <- mordedura_perro %>%
    filter(SEMANA %in% SE_BEM) %>% #estoy usando el objeto SE_BEM asique mucha atención a poner las SE correctas del BEM
    group_by(REGIONES, ANIO) %>%
    summarise(
      total_cantidad = sum(CANTIDAD),
      .groups = "drop")
  
  #agrego la columna poblacion con leftjoin
  mordedura_incidencia<- mordedura_incidencia %>%
    left_join(poblacion, by = c("ANIO", "REGIONES"))
  
  
  
  #agrego el calculo de incidencia 
  mordedura_incidencia <- mordedura_incidencia %>%
    mutate(
      total_cantidad = as.numeric(total_cantidad),  # cambio formato
      poblacion = as.numeric(poblacion),  # cambio formato
      incidencia = ifelse(!is.na(total_cantidad) & !is.na(poblacion), 
                          round((total_cantidad / poblacion) * 10000, 1), 
                          NA)) %>% 
    complete(ANIO,REGIONES) #BUSCAR chatgpt
  
  
  mordedura_incidencia <- as.data.frame(mordedura_incidencia)
  
  
  #grafico incidencia
  mordedura_incidencia_grafico <- mordedura_incidencia %>% 
    filter(!is.na(REGIONES)) %>%  # Filtrar para excluir NA en incidencia
    ggplot(aes(y = incidencia, x = factor(ANIO))) +  # Años en el eje Y, ordenados cronológicamente
    geom_col(stat = "identity", fill = "#7dd473", width = 0.5) +  # Gráfico de barras horizontales
    geom_text(aes(label = round(incidencia, 1)),  # Etiquetas con valores redondeados
              hjust = -0.2,  # Desplaza las etiquetas a la derecha
              size = 3) +  # Tamaño del texto de las etiquetas
    facet_wrap(~ REGIONES, ncol = 3, scales = "free_x") +  # Facetear por región, 2 columnas
    labs(
      x = "Año",
      y = "Incidencia acumulada cada 10000 habitantes") +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      strip.text = element_text(size = 10))  # Tamaño del texto en los facetes
  
  mordedura_incidencia_grafico
  }
  
  
  