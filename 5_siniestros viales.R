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