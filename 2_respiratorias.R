respi <- read.csv2("VR_NOMINAL_Neuquen.csv")
sum(complete.cases(respi))


respi <- as.data.frame(respi)
str(respi)


colnames(respi)
#primero filtro que ID_prov residencia sea = 58
respi <- respi %>% 
  filter(ID_PROV_INDEC_RESIDENCIA==58)


#esta mal respi <- respi %>% 
#  arrange(IDEVENTOCASO) %>% 
#  mutate(duplicado = ifelse(duplicated(IDEVENTOCASO) | duplicated(IDEVENTOCASO, fromLast = TRUE), 
#                            "DUPLICADO", "NO"))


respi <- respi %>%
  arrange(IDEVENTOCASO) %>%
  mutate(duplicado = ifelse(duplicated(IDEVENTOCASO), "DUPLICADO", "NO"))






# Guardar un dataset como archivo CSV
#write.csv(respi, "respi_chequeocompleto.csv", row.names = FALSE)

#ARMO LA BASE SOLO CON LOS NO DUPLICADOS
respi <- respi %>% 
  filter(duplicado=="NO")


#agrego columnas fecha min, anio min y SE min
#primero chequeo formato de las fechas
str(respi) #son character, entonces las paso a date

respi <- respi %>%
  mutate(
    FIS = as.Date(FIS, format = "%Y-%m-%d"),
    FECHA_CONSULTA = as.Date(FECHA_CONSULTA, format = "%Y-%m-%d"),
    FECHA_APERTURA = as.Date(FECHA_APERTURA, format = "%Y-%m-%d"),
    FTM = as.Date(FTM, format = "%Y-%m-%d"),
    FECHA_ESTUDIO = as.Date(FECHA_ESTUDIO, format = "%Y-%m-%d"))

#corro de nuevo línea 17 (str) para corroborar todo ok con formato


#columna fecha min
respi <- respi%>% 
  mutate(FECHA_MIN = pmin(FIS, FECHA_CONSULTA, FECHA_ESTUDIO,FTM,FECHA_APERTURA, na.rm = TRUE))



#columna año min y semana con la fecha min

respi <- respi %>% 
  mutate(SE_MIN=epiweek(FECHA_MIN))

respi <- respi %>% 
  mutate(ANIO_MIN=year(FECHA_MIN))


#columna contador 1 (1 fila = 1 notificacion)
respi <- respi %>% 
  mutate(CANTIDAD=1) %>%
  ungroup()


#selecciono hasta la SE de mi BEM
respi <- respi %>%
  filter(ANIO_MIN < 2025 | (ANIO_MIN == 2025 & SE_MIN <= 5))






# grafico evolutivo----------
#armo una variable con ID del rdo para despues usarlo como fill en el gráfico

respi <- respi %>% 
  mutate(
    RESULTADO_2 = case_when(
      RESULTADO == "Positivo" ~ "Positivo/detectable",
      RESULTADO == "Detectable" ~ "Positivo/detectable",
      RESULTADO == "No detectable" ~ "Negativo/no detectable",
      RESULTADO == "Negativo" ~ "Negativo/no detectable",
      TRUE ~ "ver"))



#ARMO UN ID POR SI ACASO
respi <- respi %>% 
  mutate(
    ID_RDO = case_when(
      RESULTADO == "Positivo" ~ "1",
      RESULTADO == "Detectable" ~ "2",
      RESULTADO == "No detectable" ~ "3",
      RESULTADO == "Negativo" ~ "4",
      TRUE ~ "ver"))




#Armo la base solo con los positivos y detectables
respi_posit <- respi %>% 
  filter(duplicado=="NO", RESULTADO %in% c("Positivo", "Detectable"))
    
#hay un rdo que es "muestra no apta..." no la voy a considerar

respi_evolutivo <- respi %>% 
  group_by(SE_MIN, ANIO_MIN,, ID_RDO, RESULTADO_2) %>% 
  filter(ID_RDO %in% c(1, 2, 3,4), ANIO_MIN >= 2022, duplicado=="NO" ) %>% 
  summarize(casos = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>% 
  mutate(ANIO_SE = paste(SE_MIN, ANIO_MIN, sep = "-")) %>% 
  complete(SE_MIN = 1:max(SE_MIN, na.rm = TRUE), fill = list(casos = 0)) %>% 
  ungroup() %>% 
  arrange(ANIO_MIN, SE_MIN) %>% 
  as.data.frame()

respi_evolutivo <- respi_evolutivo %>% 
  mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE)))




#grafico
respi_grafico_evolutivo <- respi_evolutivo %>% 
  ggplot(aes(x = ANIO_SE, y = casos, fill=RESULTADO_2)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(
    breaks = levels(respi_evolutivo$ANIO_SE)[seq(1, length(levels(respi_evolutivo$ANIO_SE)), by = 6)],
    expand = c(0, 0))
  theme_minimal() +
  labs(x = "semana",
       y = "Casos de IRAG")
respi_grafico_evolutivo



#grafico con positivos y detectables

respi_evolutivo2 <- respi_posit %>% 
  group_by(SE_MIN, ANIO_MIN, ID_RDO, RESULTADO_2) %>% 
  filter(ID_RDO %in% c(1, 2), ANIO_MIN >= 2022, duplicado=="NO" ) %>% 
  summarize(casos = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>% 
  mutate(ANIO_SE = paste(SE_MIN, ANIO_MIN, sep = "-")) %>% 
  complete(SE_MIN = 1:max(SE_MIN, na.rm = TRUE), fill = list(casos = 0)) %>% 
  ungroup() %>% 
  arrange(ANIO_MIN, SE_MIN) %>% 
  as.data.frame()


respi_evolutivo2 <- respi_evolutivo2 %>% 
  mutate(ANIO_SE = factor(ANIO_SE, levels = unique(ANIO_SE)))


#grafico

respi_grafico_evolutivo2 <- respi_evolutivo2 %>% 
  ggplot(aes(x = ANIO_SE, y = casos)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(
    breaks = levels(respi_evolutivo$ANIO_SE)[seq(1, length(levels(respi_evolutivo$ANIO_SE)), by = 6)],
    expand = c(0, 0))
theme_minimal() +
  labs(x = "semana",
       y = "Casos de IRAG")
respi_grafico_evolutivo2














# bajo la base para mriarla en excel 
#write.csv(respi, "respi_chequeo.csv", row.names = FALSE)







 
#grupos de edad SE del BEM
respi_edades <- respi %>% 
  filter(duplicado=="NO", SE_MIN %in% c(SE_BEM), ANIO_MIN == ANIO_max) %>% 
  group_by(GRUPO_ETARIO) %>%
  mutate(GRUPO_ETARIO = as.factor(GRUPO_ETARIO)) %>% 
  summarise(casos = n(), .groups = "drop")



#cambiar los nombres


#grafico
respi_grafico_evolutivo <- respi_edades %>% 
  ggplot(aes(x = GRUPO_ETARIO, y = casos)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Grupo Etario",
       y = "Casos de IRAG")
respi_grafico_evolutivo





























