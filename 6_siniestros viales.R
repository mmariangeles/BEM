#sinistrios viales evolutivo
siniestros_viales <- agrupada %>% 
  filter(IDEVENTOAGRUPADO == 115) %>% 
  filter((ANIO > 2023) | (ANIO == 2023 & SEMANA >= 21))


#armo la tabla evolutivo
siniestros_viales_evolutivo <- siniestros_viales %>%
  group_by(SEMANA, ANIO) %>%  
  summarise(total_cantidad = sum(CANTIDAD), .groups = "drop") %>%  # Suma la columna cantidad, ignorando NA
  arrange(ANIO, SEMANA) %>% 
  tidyr::complete(SEMANA = 1:max(SEMANA, na.rm = TRUE), fill = list(Total = 0)) %>%
  mutate(SE_ANIO = paste(ANIO, SEMANA, sep = "-")) %>%  # Crear la variable como texto: "ANIO-SEMANA"
  mutate(SE_ANIO = factor(SE_ANIO, levels = unique(SE_ANIO)))  # Convertir a factor, asegurando el orden cronológico 

#armo objeto total de siniestros viales
siniestros_viales_total <- siniestros_viales_evolutivo %>% 
  summarise(total_Cantidad = sum(total_cantidad, na.rm = TRUE))

#armo  total SE BEM de siniestros viales SE BEM
siniestros_viales_SEBEM <- siniestros_viales_evolutivo %>% 
  filter((ANIO == ANIO_max & SEMANA %in% SE_BEM)) %>%
  summarise(total_Cantidad = sum(total_cantidad, na.rm = TRUE))

#armo tabla para ver los eventos del grupo y el ID
siniestrov_eventos <- siniestros_viales %>%
  filter(SEMANA %in% SE_BEM,                                     # Filtrar por el valor de SE_BEM
         ANIO == ANIO_max) %>%                     # Filtrar por el año máximo
  group_by(NOMBREEVENTOAGRP, ID_SNVS_EVENTO_AGRP) %>%                     # Agrupar por los eventos
  summarize(
    Total = sum(CANTIDAD, na.rm = TRUE),                # Calcular el total
    .groups = "drop")


#armo los objetos de cada uno de los eventos que luego usare en el set de indicadores
ciclista <- siniestrov_eventos %>% 
  filter(ID_SNVS_EVENTO_AGRP == 497) %>% 
  summarise(Total = sum(Total)) %>% 
  pull(Total)


automovil <- siniestrov_eventos %>% 
  filter(ID_SNVS_EVENTO_AGRP == 498) %>% 
  summarise(Total = sum(Total)) %>% 
  pull(Total)

moto <- siniestrov_eventos %>% 
  filter(ID_SNVS_EVENTO_AGRP == 500) %>% 
  summarise(Total = sum(Total)) %>% 
  pull(Total)

transporte_publico <- siniestrov_eventos %>% 
  filter(ID_SNVS_EVENTO_AGRP == 499) %>% 
  summarise(Total = sum(Total)) %>% 
  pull(Total)


peaton <- siniestrov_eventos %>% 
  filter(ID_SNVS_EVENTO_AGRP == 496) %>% 
  summarise(Total = sum(Total)) %>% 
  pull(Total)

{

#INDICADORES
  
  # Cargar librerías necesarias
  library(ggplot2)
  library(gridExtra)
  library(stringr)
  
  # Crear dataframe con categorías y valores
  indicadores <- data.frame(
    Categoria = c("Conductor o pasajero de transporte público", "Conductor o pasajero de automóvil", 
                  "Conductor o pasajero de motocicleta", "Ciclista", "Peatón"),
    Valor = c(transporte_publico, automovil, moto, ciclista, peaton) 
  )
  
  # Función para crear recuadros
  crear_recuadro <- function(categoria, valor, color_fondo, color_numero) {
    # Ajustar el texto para dividirlo en varias líneas
    categoria_wrapped <- stringr::str_wrap(categoria, width = 20)
    
    ggplot() +
      annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = color_fondo) +
      annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.3, ymax = 0.7, fill = "#faf7ff", alpha = 0.8) +
      annotate("text", x = 0.5, y = 0.8, label = categoria_wrapped, size = 4, fontface = "bold", 
               color = "white", lineheight = 0.8) +
      annotate("text", x = 0.5, y = 0.5, label = valor, size = 8, fontface = "bold", color = color_numero) +
      theme_void()
  }
  
  # Recuadro superior
  recuadro_superior <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#e24a38", alpha = 0.9) +
    annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.3, ymax = 0.7, fill = "#faf7ff", alpha = 0.8) +
    annotate("text", x = 0.5, y = 0.8, label = "Internaciones por siniestros viales", 
             size = 5, fontface = "bold", color = "#faf7ff") +
    annotate("text", x = 0.5, y = 0.5, label = siniestros_viales_SEBEM, 
             size = 10, fontface = "bold", color = "#e24a38") +
    theme_void()
  
  # Crear gráficos individuales
  graficos <- lapply(1:nrow(indicadores), function(i) {
    crear_recuadro(
      categoria = indicadores$Categoria[i],
      valor = indicadores$Valor[i],
      color_fondo = "#e24a38",
      color_numero = "#e24a38"
    )
  })
  
  # Organizar los gráficos en la botonera
  indicador_accidentes <- grid.arrange(
    recuadro_superior,
    do.call(arrangeGrob, list(grobs = graficos, ncol = 5)), # Ajusta ncol según el número de categorías
    heights = c(1, 2)
  )
  
  # Mostrar el gráfico final
  indicador_accidentes
  
  }  

{
#grafico evolutivo
siniestros_viales_grafico_evolutivo <- siniestros_viales_evolutivo %>% 
  ggplot(aes(x = SE_ANIO, y = total_cantidad)) +
  geom_bar(stat = "identity", fill = "#e24a38", width = 0.5) +
  scale_x_discrete(
    breaks = levels(siniestros_viales_evolutivo$SE_ANIO)[seq(1, length(levels(siniestros_viales_evolutivo$SE_ANIO)), by = 3)],
    expand = c(0, 0)) +  
  labs(
    x = "SE-año",
    y = "Lesiones por causas externas") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 7),
    axis.text.x = element_text(size = 7, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 7),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank())
siniestros_viales_grafico_evolutivo
}
