#utilizo source

source("1_general.R")

#sinistrios viales evolutivo
siniestros_viales <- agrupada %>% 
  filter(IDEVENTOAGRUPADO == 115) %>% 
  filter((ANIO > 2023) | (ANIO == 2023 & SEMANA >= 21))


#armo la tabla evolutivo
siniestros_viales_evolutivo <- siniestros_viales %>%
  group_by(SEMANA, ANIO) %>%  
  summarise(total_cantidad = sum(CANTIDAD), .groups = "drop") %>%  # Suma la columna cantidad, ignorando NA
  arrange(ANIO, SEMANA) %>% 
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


#INDICADORES
indicadores <- data.frame(
  Categoria = c("Conductor o pasajero de transporte público", "Conductor o pasajero de automovil", "Conductor o pasajero de motocicleta", "Ciclista","Peatón"),
  Valor = c(transporte_publico, automovil, moto, ciclista, peaton))

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
}

# Recuadro superior con centro más claro
recuadro_superior <- ggplot() +
  # Fondo del recuadro
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#4286f5",  alpha = 0.9) +
  # Centro más claro
  annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.3, ymax = 0.7, fill = "white", alpha = 0.8) +
  # Texto del título
  annotate("text", x = 0.5, y = 0.8, label = "Internaciones por siniestros viales", size = 5, fontface = "bold", color = "white") +
  # Número en el centro
  annotate("text", x = 0.5, y = 0.5, label = siniestros_viales_SEBEM, size = 10, fontface = "bold", color = "#003366") +
  theme_void()

# Crear gráficos para los indicadores secundarios
graficos <- list(
  crear_recuadro("Conductor o pasajero de transporte público", 29.2, "#4286f5", "#003366"),
  crear_recuadro("Conductor o pasajero de automovil", 31.5, "#4286f5", "#003366"),
  crear_recuadro("Conductor o pasajero de motocicleta",  41.7, "#4286f5", "#003366"),
  crear_recuadro("Ciclista",  15.3, "#4286f5", "#003366"),
  crear_recuadro("Peatón", 41.7, "#4286f5", "#003366")
)

# Organizar el recuadro superior y los indicadores secundarios
botonera_1 <- grid.arrange(
  recuadro_superior, 
  arrangeGrob(grobs = graficos, ncol = 3), #columnas en las que se organizan los graf abajo
  heights = c(1, 2) 
  # Ajusta la proporción de alturas
)
print(botonera_1)










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