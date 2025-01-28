#source
source("1_general.R")
  
  
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
     group_by(NOMBREEVENTOAGRP, ID_SNVS_EVENTO_AGRP) %>%                     # Agrupar por los eventos
      summarize(
        Total = sum(CANTIDAD, na.rm = TRUE),                # Calcular el total
        .groups = "drop")
  
#MUY IMPORTANTE esta tabla porque se define los eventos que se muestran en el set de indicadores
  #"si hay alguno nuevo" hay que hacer los objetos y demás tal como está en las lineas siguientes
  
  
  
  # Calcular los totales para cada categoría
  caidas_golpes <- lesiones_eventos %>% 
    filter(ID_SNVS_EVENTO_AGRP == 501) %>% 
    summarise(Total = sum(Total)) %>% 
    pull(Total)
  
  cortes_quemaduras <- lesiones_eventos %>% 
    filter(ID_SNVS_EVENTO_AGRP == 505) %>% 
    summarise(Total = sum(Total)) %>% 
    pull(Total)
  
  sin_especificar <- lesiones_eventos %>% 
    filter(ID_SNVS_EVENTO_AGRP == 510) %>% 
    summarise(Total = sum(Total)) %>% 
    pull(Total)
  
  electrocusion <- lesiones_eventos %>% 
    filter(ID_SNVS_EVENTO_AGRP == 504) %>% 
    summarise(Total = sum(Total)) %>% 
    pull(Total)
  
  # ahogamiento <- lesiones_eventos %>% 
  #   filter(ID_SNVS_EVENTO_AGRP == 506) %>% 
  #   summarise(Total = sum(Total)) %>% 
  #   pull(Total)
  
  otras <- lesiones_eventos %>% 
    filter(ID_SNVS_EVENTO_AGRP == 511) %>% 
    summarise(Total = sum(Total)) %>% 
    pull(Total)
  
  # Crear el dataframe indicadores
  indicadores <- data.frame(
    Categoria = c("Caídas y golpes", "Cortes y quemaduras", "Sin especificar", 
                  "Lesiones por electrocución", "Otras"),
    Valor = c(caidas_golpes, cortes_quemaduras, sin_especificar, electrocusion, otras)
  )
  
  # Función para crear recuadros
  crear_recuadro <- function(categoria, valor, color_fondo, color_numero) {
    ggplot() +
      annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = color_fondo, alpha = 0.9) +
      annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.3, ymax = 0.7, fill = "white", alpha = 0.8) +
      annotate("text", x = 0.5, y = 0.8, label = categoria, size = 4, fontface = "bold", color = "white") +
      annotate("text", x = 0.5, y = 0.5, label = valor, size = 8, fontface = "bold", color = color_numero) +
      theme_void()
  }
  
  # Recuadro superior
  recuadro_superior <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#4286f5", alpha = 0.9) +
    annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.3, ymax = 0.7, fill = "white", alpha = 0.8) +
    annotate("text", x = 0.5, y = 0.8, label = "Internaciones por lesiones en el hogar", 
             size = 5, fontface = "bold", color = "white") +
    annotate("text", x = 0.5, y = 0.5, label = sum(indicadores$Valor), 
             size = 10, fontface = "bold", color = "#003366") +
    theme_void()
  
  # Crear gráficos individuales
  graficos <- lapply(1:nrow(indicadores), function(i) {
    crear_recuadro(
      categoria = indicadores$Categoria[i],
      valor = indicadores$Valor[i],
      color_fondo = "#4286f5",
      color_numero = "#003366"
    )
  })
  
  # Organizar los gráficos en la botonera
  indicador_lesiones <- grid.arrange(
    recuadro_superior,
    arrangeGrob(grobs = graficos, ncol = 5), # Ajusta ncol según el número de categorías
    heights = c(1, 2)
  )
  
  # Mostrar el gráfico final
  print(botonera)
  
  



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
    