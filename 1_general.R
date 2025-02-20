#objetivos de este .R Es util para la base agrupada
#a) cargar librerias
#b)cargar dataset
#c)crear objetos SE BEM, AÑO MAX, AÑO MIN
#d)agregar columnas regiones 



#preparacion de base agrupada----
{
  #lectura de base y chequeo de formato dataframe
  agrupada <- read.csv("NEUQUEN_CLI.CSV", sep = ";",na.strings = "")
  sum(complete.cases(agrupada))
  
  nrow(agrupada)
  
  #selecciono hasta la SE de mi BEM
  agrupada <- agrupada %>%
    filter(ANIO < 2025 | (ANIO == 2025 & SEMANA <= 5))
  
  
  #agrego columna region----
  #lectura de la base 
  regiones <- read_excel("REGIONES.xlsx")
  regiones <- as.data.frame(regiones) #cambio formato a dataframe
  
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
  #las SE/año del BEM de este mes son   (esto es un ayuda memoria) del año  #SE maxima
  SE_BEM <- c(1, 2, 3, 4, 5) #hay que cambiarlo mensualmente
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








