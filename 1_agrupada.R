#objetivos de este .R Es util para la base agrupada
#a)cargar dataset
#b)crear objetos SE BEM, AÑO MAX, AÑO MIN
#c)agregar columnas regiones 


#aclaración: la base agrupada viene solo con los años 2024 y 2025.Se pidio
#a nación una base completa. Lo primero que se hace acá es unir las 2 bases


#cargo la base agrupada completa, sin años 2024 y 2025

agrupada_vieja <- read.csv("NEUQUEN_CLI_ANTERIOR.csv", sep = ";",na.strings = "")
sum(complete.cases(agrupada_vieja))

agrupada_vieja <- agrupada_vieja %>%
  filter(ANIO < 2024 )


#preparacion de base agrupada actual----
{
  
  #lectura de base y chequeo de formato dataframe
  agrupada <- read.csv("NEUQUEN_CLI.csv", sep = ";",na.strings = "")
  sum(complete.cases(agrupada))
  
  nrow(agrupada)
  
#junto las bases
  
agrupada<- bind_rows(agrupada_vieja, agrupada) 
  
  
  #selecciono hasta la SE de mi BEM
  agrupada <- agrupada %>%
    filter(ANIO < 2025 | (ANIO == 2025 & SEMANA <= 13))
  
  
  #agrego columna region----
  #lectura de la base 
  regiones <- read_excel("REGIONES.xlsx")
  regiones <- as.data.frame(regiones) #cambio formato a dataframe
  
  #regiones_duplicadas <- regiones %>% #esto lo hago porque habia duplicados que agregaban observaciones a "agrupadas"
  # group_by(LOCALIDAD) %>%
  # filter(n() > 1)
  
  #regiones <- regiones%>%
  #  distinct(LOCALIDAD, .keep_all = TRUE)
  
  #agrego la columna regiones e ID regiones a agrupadas
  agrupada <- agrupada %>%
    left_join(regiones, by = "ID_ORIGEN")
  
}


#columna se-año, SE MAX, MIN----
{#armo la variable SE-año
  agrupada <- agrupada %>%
    mutate(SE_ANIO= paste(SEMANA, ANIO, sep = "-"))
  
  ##SE/año del BEM
  #las SE/año del BEM de este mes son   (esto es un ayuda memoria) del año  #SE maxima
  SE_BEM <- c(10, 11, 12, 13) #hay que cambiarlo mensualmente
  SE_BEM <- as.vector(SE_BEM)
  
  #ANIO maximo (lo voy a usar para tablas, gráficos)
  ANIO_max <- agrupada %>% 
    summarise(ANIO_max = max(ANIO, na.rm = TRUE))
  
  ANIO_max <- as.numeric(ANIO_max[[1]])  # Extrae la primera columna y lo convierte a numérico
  
  #SE_min 
  SE_min <- SE_BEM %>%
    min()
  
  #SE_max 
  SE_max <- SE_BEM %>%
    max()
  
  #AÑO ANTERIOR AL ACTUAL PARA VARIACION %
  anio_anterior <- ANIO_max-1
    
    }








