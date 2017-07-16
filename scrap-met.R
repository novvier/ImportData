##################
# Función para descargar datos meteorológicos de una estación
# meteorológica desde la web de Senamhi
##################

senamhi <- function(inicio, fin, estacion){
  
  # Cargar librerias
  library(rvest)
  library(plyr)
  
  # Generar fechas por mes
  fecha <- seq(as.Date(inicio), as.Date(fin), by="month") # Genera las fechas
  fecha <- gsub(' |-','',substr(as.character(fecha),1,7)) #Cambia el formato
  
  # Función para extraer data de un mes (sólo una pagina)
  extraer <- function(fecha, estacion){
    url <- paste0("http://www.senamhi.gob.pe/include_mapas/_dat_esta_tipo02.php?estaciones=",estacion,"&tipo=CON&CBOFiltro=",fecha,"&t_e=M")
    webpage <- read_html(url)
    tbls <- html_nodes(webpage, "table")
    tbls_ls <- webpage %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
    tbls_ls[[1]][-1:-2,]
  }
  
  # Extraer todos los meses
  datos <- list()
  for (i in 1:length(fecha)){
    datos[[i]] <- extraer(fecha[i],estacion)
  }
  datos <- ldply(datos) #Combinar datos
  colnames(datos) <- c("date","Tmax","Tmin","Tdry07","Tdry13","Tdry19",
                       "Twed07","Twed13","Twed19","PP07","PP09","wd","ws")
  return(datos)
}

# Ejemplo. Datos de la estación Cerro de Pasco
datos <- senamhi("2011-11-1","2017-1-1","000593")

################
# Tratamiento de datos
################

# Convertir a valos numéricos
datos[,c(2:11,13)] <- sapply(datos[,c(2:11,13)], as.numeric)

# Para viento en calma, velocidad = 0
datos$ws <- ifelse(datos$wd=="C",0,datos$ws)

# Cambiar formato de dirección de viento. De valores nominales a ángulos
datos$wd <- as.character(datos$wd)
for (i in 1:length(datos$wd)){
  if (datos$wd[i] == "N") datos$wd[i] <- "0"
  if (datos$wd[i] == "NNE") datos$wd[i] <- "22.5"
  if (datos$wd[i] == "NE") datos$wd[i] <- "45"
  if (datos$wd[i] == "ENE") datos$wd[i] <- "67.5"
  if (datos$wd[i] == "E") datos$wd[i] <- "90"
  if (datos$wd[i] == "ESE") datos$wd[i] <- "112.5"
  if (datos$wd[i] == "SE") datos$wd[i] <- "135"
  if (datos$wd[i] == "SSE") datos$wd[i] <- "157.5"
  if (datos$wd[i] == "S") datos$wd[i] <- "180"
  if (datos$wd[i] == "SSW") datos$wd[i] <- "202.5"
  if (datos$wd[i] == "SW") datos$wd[i] <- "225"
  if (datos$wd[i] == "WSW") datos$wd[i] <- "247.5"
  if (datos$wd[i] == "W") datos$wd[i] <- "270"
  if (datos$wd[i] == "WNW") datos$wd[i] <- "292.5"
  if (datos$wd[i] == "NW") datos$wd[i] <- "315"
  if (datos$wd[i] == "NNW") datos$wd[i] <- "337.5"
  if (datos$wd[i] == "C") datos$wd[i] <- NA
}
datos$wd <- as.numeric(datos$wd)

# Cambiar formato de fecha para usarlos en openair
datos$date <- gsub('-','.-',datos$date)
datos$date <- as.POSIXct(strptime(datos$date, "%d.-%b-%Y"))

# Exportar e importar datos
write.table(datos, "data/metCdePasco.txt", sep = "\t", row.names = FALSE)
datos0 <- read.table("data/metCdePasco_test.txt", header=TRUE, sep = "\t")
