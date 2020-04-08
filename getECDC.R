getECDC <- function(isDeath = FALSE, isCumulative = TRUE){
  library(readxl)
  library(httr)
  
  #create the URL where the dataset is stored with automatic updates every day
  last_day <- as.Date(format(Sys.time(), tz="UTC"))
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",last_day, ".xlsx", sep = "")
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

  data <- try(read_excel(tf), silent = TRUE)
  
  if (class(data) == "try-error") {
    last_day <- as.Date(format(Sys.time(), tz="UTC"))-1
    url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",last_day, ".xlsx", sep = "")
    GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
    data <- read_excel(tf)
    
  }
  
  data.mat <- matrix(NA, nrow = length(unique(data$dateRep)), ncol = length(unique(data$countriesAndTerritories)))
  days <- rev(seq(as.Date("2019-12-31"), last_day, by="days"))
  rownames(data.mat) <- format(days, "%Y_%m_%d")
  colnames(data.mat) <- unique(data$countriesAndTerritories)
  
  if(isDeath){
    for(i in 1:nrow(data)){
      data.mat[format(data$dateRep[i], "%Y_%m_%d"),data$countriesAndTerritories[i]] <- data$deaths[i]
    }
  }
  else{
    for(i in 1:nrow(data)){
      data.mat[format(data$dateRep[i], "%Y_%m_%d"),data$countriesAndTerritories[i]] <- data$cases[i]
    }
  }
  if(isCumulative){
    tmp = data.mat
    tmp[is.na(tmp)] <- 0
    data.mat.cum <- apply(tmp, 2, function(x){rev(cumsum(rev(x)))})
    data.mat.cum[is.na(data.mat)] <- NA
    return(data.mat.cum)
  }
  else{
    return(data.mat)
  }
  
}