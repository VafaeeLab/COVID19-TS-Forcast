update_data_model <- function(){

  source("getECDC.R")
  source("generateModel.R")
  source("setRegressors.R")

# Update ECDC -------------------------------------------------------------

  ecdc <- getECDC()
  World <- rowSums(ecdc, na.rm = TRUE)
  ecdc <- cbind(World, ecdc)
  
  # NA padding for starting dates upto having minimum of 50 cases.
  remove <- which(ecdc < 30)
  ecdc[remove] <- NA
  
  
  # only countries with >30 non-zeros.
  keep <- apply(ecdc, 2, function(c)sum(c!=0, na.rm = TRUE))
  keep <- which(keep > 30)
  ecdc <- ecdc[,keep]

  
  
# Update Models -----------------------------------------------------------

  allModels <- list()
  keep <- c()
  pred10 <- c()
  countries <- unique(as.character(colnames(ecdc)))
  
  for (i in 1:length(countries)) {
    c <- countries[i]
    output <- try(generateModel(country = c, ecdc = ecdc, r = 0, interventions.past = c(), interventions.future = c(), day.range = c()), silent = TRUE)
    if (class(output) != "try-error") {
      allModels[[i]] <- output
      p <- output$meanp*1e-5
      pred10 <- rbind(pred10,p)
      keep <- c(keep, i)
      print(paste(c,"pass", i, sep = "  "))
    }
    else{
      print(paste(c,"fail", i))
    }
  }

# Remove Models with Errors -----------------------------------------------

  ecdc <- ecdc[,keep]
  allModels <- allModels[keep]
  
  
  startDay = as.Date(gsub("_", "-", rownames(ecdc)[1]))+1
  endDay = startDay + 9
  future = as.character(seq(startDay, endDay, by="day"))
  pred10 <- cbind(countries[keep], pred10)
  colnames(pred10) <- c("Country", future)
  pred10 <- pred10[-1,]

  saveRDS(allModels, file = "./Data/allModels.rds")
  write.csv(ecdc, file = "./Data/ecdc.csv")
  write.csv(pred10, file = "./Data/all_prediction_next_days.csv", row.names=FALSE)
}

