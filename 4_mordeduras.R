#Lesiones por mordedura de perro (ambulatorias)----
{#mordeduras evolutivo
  mordedura_perro <- agrupada %>% 
    filter(ID_SNVS_EVENTO_AGRP %in% c(512, 341))
  
  
  #tabla mordedura evolutivo
  
  mordeduras_evolutivo <- mordedura_perro %>%
    group_by(SEMANA, ANIO) %>%  
    summarise(total_cantidad = sum(CANTIDAD), .groups = "drop") %>%  # Suma la columna cantidad, ignorando NA
    arrange(ANIO, SEMANA) %>% 
    mutate(SE_ANIO = paste(ANIO, SEMANA, sep = "-")) %>%  # Crear la variable como texto: "ANIO-SEMANA"
    mutate(SE_ANIO = factor(SE_ANIO, levels = unique(SE_ANIO)))  # Convertir a factor, asegurando el orden cronológico
  
  
  #objeto n mordeduras
  mordeduras_total <- sum(mordeduras_evolutivo$total_cantidad, na.rm = TRUE)
  
  
  #grafico evolutivo
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