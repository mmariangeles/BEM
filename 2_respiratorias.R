

#lectura base
VR_NOMINAL <- read_excel("VR_NOMINAL_Neuquen.xlsx")

# Aplicar la conversión a fecha solo si es necesario
VR_NOMINAL <- VR_NOMINAL %>%
  mutate(
    FECHA_CONSULTA = convertir_a_fecha(FECHA_CONSULTA),
    FIS = convertir_a_fecha(FIS),
    FECHA_APERTURA = convertir_a_fecha(FECHA_APERTURA),
    FECHA_ALTA_MEDICA = convertir_a_fecha(FECHA_ALTA_MEDICA),
    FECHA_INTERNACION = convertir_a_fecha(FECHA_INTERNACION),
    FECHA_CUI_INTENSIVOS = convertir_a_fecha(FECHA_CUI_INTENSIVOS),
    FECHA_FALLECIMIENTO = convertir_a_fecha(FECHA_FALLECIMIENTO),
    FECHA_NACIMIENTO = convertir_a_fecha(FECHA_NACIMIENTO),
    FECHA_ESTUDIO = convertir_a_fecha(FECHA_ESTUDIO))


###creo una nueva variable de fecha 
VR_NOMINAL <- VR_NOMINAL %>% 
  mutate(FECHA_CREADA = coalesce(FIS, FECHA_CONSULTA, FECHA_ESTUDIO, FECHA_APERTURA))


resultado_algoritmo_1 <- algoritmo_1(data=VR_NOMINAL,
                                     col_signos = "SIGNO_SINTOMA",
                                     col_comorbilidades = "COMORBILIDAD",
                                     col_determinacion= "DETERMINACION", 
                                     col_resultado= "RESULTADO",
                                     col_tipo_lugar =  "TIPO_LUGAR_OCURRENCIA",
                                     col_antecedente = "ANTECEDENTE_EPIDEMIOLOGICO",
                                     col_cobertura_social = "COBERTURA_SOCIAL"
)

VR_NOMINAL_IDEVENTOCASO <- resultado_algoritmo_1$data


## Creo la clasificacion de virus (opcional)

clasificar_virus <- function(x) {
  case_when(
    str_detect(x, "VSR") ~ "VSR",
    str_detect(x, "SARS") ~ "SARS-CoV-2",
    str_detect(x, "Influenza A") ~ "Influenza A",
    is.na(x) | x == "" ~ "Sin determinación",
    TRUE ~ "Otro"
  )
}

## Identifico cuales son las columnas de deteminaciones, en este caso las que comienzan con "_DETERMINACION"
columnas_determinacion <- names(VR_NOMINAL_IDEVENTOCASO) %>%
  str_subset("^DETERMINACION_") %>%
  setdiff(c("DETERMINACION_DICO",
            "DETERMINACION_DICO_CENTINELA",
            "DETERMINACION_SIN_DATO"))

## Aplico la funcion creada
resultado <- analizar_determinaciones(
  data = VR_NOMINAL_IDEVENTOCASO, # Dataset tranformado
  columnas_determinacion = columnas_determinacion,# columnas determinacion creado arriba
  variable_agrupar = "SEPI_APERTURA",# variables de agrupacion principal
  variable_cruce = "ANIO_EPI_APERTURA",# variables de agrupacion secundaria (opciona)
  clasificar = clasificar_virus # Clasificacion de virus creada arriba, opcional. 
)


names(resultado)

## hago un grafico con la tabla resultado 1


# Primero transformamos a formato largo para poder graficar detectables y no detectables juntos
resultado_influenza <- resultado %>%
  filter(DETERMINACION == "Influenza A") %>%
  pivot_longer(cols = c("Detectable", "No_detectable"), 
               names_to = "Tipo_Resultado", 
               values_to = "n") %>%
  mutate(Tipo_Resultado = factor(Tipo_Resultado, levels = c("No_detectable", "Detectable")))


# Gráfico
ggplot(resultado_influenza, aes(x = as.numeric(SEPI_APERTURA), y = n, fill = Tipo_Resultado)) +
  geom_col(position = "stack") +
  facet_wrap(~ ANIO_EPI_APERTURA) +
  scale_fill_manual(
    values = c(
      "No_detectable" = "#A9A9A9",  # gris
      "Detectable" = "#D7263D"      # rojo
    ),
    name = "Resultado"
  ) +
  labs(
    title = "Detección de Influenza A por semana y año",
    x = "Semana epidemiológica",
    y = "Número de testeos",
    fill = "Resultado"
  ) +
  theme_minimal()+
  theme(
    legend.position = "bottom"
  )

