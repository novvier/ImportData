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
  x <- read.csv(file, sep = "\t", skip = 2, header = FALSE, 
                col.names = seq(1,38,1), na.strings = "---")
  #
  # Variables Davis
  names(x) <- c("date","time","TempOut","TempHi","TempLow",
    "HumOut","PtDew","WindSpeed","WindDir","WindRun",
    "HiSpeed","HiDir","HeatIndex","WindChill","ThwIndex",
    "ThsIndex","Bar","Rain","RainRate","SolarRad",
    "SolarEnergy","HiSolarRad","UvIndex","UvDose","HiUv",
    "DdHeat","DdCool","TempIn","HumIn","DewIn","HeatIn",
    "EmcIn","DensityInAir","Et","IssRecept","WindSamp",
    "WindTx","ArcInt")
  #
  x[, c(1, 2, 9, 12)] <- lapply(x[, c(1, 2, 9, 12)], as.character)
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
  x$HiDir.n <- x$HiDir
  # Convertir valores nominales deWwindDir a grado
  WinDirNum <- function(x) {
    lx = length(x)
    y = rep("",lx)
    name = c("C","NNE","NE","ENE","E","ESE","SE",
      "SSE","S","SSW","SW","WSW","W","WNW","NW","NNW","N")
    value = seq(0,360,22.5)
    #
    for (i in 1:lx){
      z = value[which(name == x[i])]
      if(length(z) == 0) y[i] = ""
      else y[i] = z 
    }
    #
    return(y)
  }
  x$WindDir <- as.numeric(WinDirNum(x$WindDir))
  x$HiDir <- as.numeric(WinDirNum(x$HiDir))
  #
  #lx <- length(x)-1
  #for (i in 2:lx)
  #  x[,i] <- as.numeric(x[,i])
  #
  return(x)
}