# ----------------------------------------------------------------- #   
# ------------------- IMPORT STATION DAVIS DATA ------------------- #
# ----------------------------------------------------------------- #

MetDavis <- function(file, tz = "America/Lima") {
  # Import data from station Davis Vintage Pro
  #
  # Args:
  #   file: name file (character)
  #   tz: local time zone
  #
  # Return:
  #   data.frame whit data
  #
  # Importar de txt
  x <- read.csv2(file, sep = "\t", skip = 2,
    header = FALSE, col.names = seq(1,38,1),
    colClasses = "character")
  #
  # Variables Davis
  names(x) <- c("date","time","TempOut","TempHi","TempLow",
    "HumOut","PtDew","WindSpeed","WindDir","WindRun",
    "HiSpeed","HeatIndex","HiDir","WindChill","ThwIndex",
    "ThsIndex","Bar","Rain","RainRate","SolarRad",
    "SolarEnergy","HiSolarRad","UvIndex","UvDose","HiUv",
    "DdHeat","DdCool","TempIn","HumIn","DewIn","HeatIn",
    "EmcIn","DensityInAir","Et","IssRecept","WindSamp",
    "WindTx","ArcInt")
  #
  # Convertir fecha en POSIX
  x$date <- paste(x$date, x$time)
  x$date <- paste(x$date, "m", sep = "")
  x$date <- as.POSIXct(strptime(x$date,
    format = "%d/%m/%y %I:%M %p", tz=tz))
  x$time <- NULL
  #
  # WindDir Nominal
  x$WindDir.n <- x$WindDir
  # Convertir valores nominales deWwindDir a grado
  for (i in 1:length(x$WindDir)){
    if (x$WindDir[i] == "N") x$WindDir[i] <- "360"
    if (x$WindDir[i] == "NNE") x$WindDir[i] <- "22.5"
    if (x$WindDir[i] == "NE") x$WindDir[i] <- "45"
    if (x$WindDir[i] == "ENE") x$WindDir[i] <- "67.5"
    if (x$WindDir[i] == "E") x$WindDir[i] <- "90"
    if (x$WindDir[i] == "ESE") x$WindDir[i] <- "112.5"
    if (x$WindDir[i] == "SE") x$WindDir[i] <- "135"
    if (x$WindDir[i] == "SSE") x$WindDir[i] <- "157.5"
    if (x$WindDir[i] == "S") x$WindDir[i] <- "180"
    if (x$WindDir[i] == "SSW") x$WindDir[i] <- "202.5"
    if (x$WindDir[i] == "SW") x$WindDir[i] <- "225"
    if (x$WindDir[i] == "WSW") x$WindDir[i] <- "247.5"
    if (x$WindDir[i] == "W") x$WindDir[i] <- "270"
    if (x$WindDir[i] == "WNW") x$WindDir[i] <- "292.5"
    if (x$WindDir[i] == "NW") x$WindDir[i] <- "315"
    if (x$WindDir[i] == "NNW") x$WindDir[i] <- "337.5"
  }
  #
  lx <- length(x)-1
  for (i in 2:lx)
    x[,i] <- as.numeric(x[,i])
  #
  return(x)
}