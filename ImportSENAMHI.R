# ----------------------------------------------------------------- #   
# -------------------- IMPORT WEB SENAMHI DATA -------------------- #
# ----------------------------------------------------------------- #

senamhi <- function(start, end, station){
  #
  # Import web-senamhi data (www.senamhi.gob.pe)
  #
  # Args:
  #   start: date start (character with format: "%Y-%m"
  #   end: date en (character with format: "%Y-%m")
  #   station: code statation. search in www.senamhi.gob.pe
  #
  # Return:
  #   data.frame with SENAMHI data of "station"
  #
  # Load libraries
  pkgTest <- function(x) {
    if (!require(x,character.only = TRUE)) {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE))
          stop("Package not found")
    }
  }
  pkgTest("rvest")
  pkgTest("plyr")
  #
  # Dates generate by month mes
  start <- paste0(start, "-1")
  end <- paste0(end, "-1")
  date <- seq(as.Date(start), as.Date(end), by = "month")
  date <- gsub(' |-','',substr(as.character(date),1,7))
  
  # Extract web-senamhi data for each month
  extraer <- function(date, station){
    url <- paste0("http://www.senamhi.gob.pe/include_mapas/",
                  "_dat_esta_tipo02.php?estaciones=",
                  station,"&tipo=CON&CBOFiltro=",date,"&t_e=M")
    webpage <- read_html(url)
    tbls <- html_nodes(webpage, "table")
    tbls_ls <- webpage %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
    tbls_ls[[1]][-1:-2,]
  }
  
  # Bucle for estract every month 
  x <- list()
  for (i in 1:length(date)){
    x[[i]] <- extraer(date[i],station)
  }
  x <- ldply(x) # merge data
  colnames(x) <- c("date","Tmax","Tmin","Tdry07","Tdry13","Tdry19",
          "Twed07","Twed13","Twed19","PP07","PP09","wd","ws")
  #
  # Change nominal to numeric the wind speed values
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
  x$wd.n <- x$wd
  x$wd.n[x$wd.n == ""] <- NA
  x$wd.n <- as.factor(x$wd.n)
  x$wd <- WinDirNum(x$wd)
  #
  # format date
  x$date <- gsub('-', '.-', x$date)
  x$date <- gsub('Sep', 'Set', x$date)
  x$date <- as.POSIXct(strptime(x$date, "%d.-%b-%Y"))
  #
  # all to numeric
  x[, 2:13] <- lapply(x[, 2:13], as.numeric)
  #
  return(x)
}