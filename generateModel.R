generateModel<- function(country, ecdc, r = 1, interventions.past = c(), interventions.future  =  c(), day.range = NULL){
  
library(forecast)
library(tseries)
library(readtext)
library(openxlsx)
library(spatstat)
library(tsiR)
library(timetk)
library(imputeTS)
source("setRegressors.R")
  
  
# subset ECDC -------------------------------------------------------------

# remove starting NAs

data.country <- ecdc[,country]
s <- ifelse(length(which(is.na(data.country)))==0, length(data.country), min(which(is.na(data.country)))-1)
data.country <- rev(data.country[1:s])
dates <- rev(rownames(ecdc)[1:s])

# apply range 
if(!is.null(day.range)){
  day.range = as.character(day.range)
  start <- which(dates == day.range[1]%>% gsub("-","_",.))
  start <- if(length(start) == 0) 1 else start
  end   <- which(dates == day.range[2]%>% gsub("-","_",.))
  end <- if(length(end) == 0) length(data.country) else end
  dates <- dates[start:end]
  data.country <- data.country[start:end]
}

regg <- setRegressors(dates, interventions.past, interventions.future)
reggt <- regg$reggt
reggp <- regg$reggp
data.model <- na_locf(data.country, option = "nocb")

#print(data.model)
  



# No transformation -------------------------------------------------------

data <- ts(data.model, start=c(1,1))
st <- adf.test(data)
diff1 <-0
while  (st$p.value > 0.05)
{
  data <- diff(data, differences = 1)
  diff1<- diff1+1
  st <- adf.test(data)
  #st
}
#plot(data)
if (r==1) 
{
  xregd1 <- reggt
  i<-diff1
  while (i > 0) 
  {
    xregd1 <- xregd1[-1]
    i <- i-1
  }
  xregd1 <- na.remove(xregd1)
  sample.model1 <- auto.arima (data, xreg=xregd1, allowdrift = FALSE)
  sample.model1
  sig1 <- (1-pnorm(abs(sample.model1$coef)/sqrt(diag(sample.model1$var.coef))))*2
} 

if (r==0) 
{
  sample.model1 <- auto.arima (data, allowdrift = FALSE)
  #print(sample.model1)
  forecast(sample.model1,h=10)
  sig1 <- (1-pnorm(abs(sample.model1$coef)/sqrt(diag(sample.model1$var.coef))))*2
}


# Ratio Transformation ----------------------------------------------------

data <- ts(data.model, start=c(1,1))
data <- data[-1]/data[-length(data)]
data <- ts(data, start=c(1,1))
st <- adf.test(data)
diff2 <-0
while  (st$p.value > 0.05)
{
  data <- diff(data, differences = 1)
  diff2<- diff2+1
  st <- adf.test(data)
  #st
}
#plot(data)
if (r==1)
{
  xregd2 <- reggt
  i<-diff2
  while (i > -1)
  {
    xregd2 <- xregd2[-1]
    i <- i-1
  }
  xregd2 <- na.remove(xregd2)
  sample.model2 <- auto.arima (data, xreg=xregd2, allowdrift = FALSE)
  sample.model2
  sig2 <- (1-pnorm(abs(sample.model2$coef)/sqrt(diag(sample.model2$var.coef))))*2
}

if (r==0)
{
  sample.model2 <- auto.arima (data, allowdrift = FALSE)
  #print(sample.model2)
  sig2 <- (1-pnorm(abs(sample.model2$coef)/sqrt(diag(sample.model2$var.coef))))*2
}


# Power Transformation ----------------------------------------------------

data <- ts(data.model, start=c(1,1))

tk_index(data)
data <- data^(1/tk_index(data))
data <- ts(data, start=c(1,1))
st <- adf.test(data)
diff3 <-0
while  (st$p.value > 0.05)
{
  data <- diff(data, differences = 1)
  diff3<- diff3+1
  st <- adf.test(data)
  #st
}
#plot(data)
if (r==1)
{
  xregd3 <- reggt
  i<-diff3
  while (i > 0)
  {
    xregd3 <- xregd3[-1]
    i <- i-1
  }
  xregd3 <- na.remove(xregd3)
  sample.model3 <- auto.arima (data, xreg=xregd3, allowdrift = FALSE)
  sample.model3
  sig3 <- (1-pnorm(abs(sample.model3$coef)/sqrt(diag(sample.model3$var.coef))))*2
}

if (r==0) 
{
  sample.model3 <- auto.arima (data, allowdrift = FALSE, allowmean = TRUE)
  #print(sample.model3)
  forecast(sample.model3,h=10)
  sig3 <- (1-pnorm(abs(sample.model3$coef)/sqrt(diag(sample.model3$var.coef))))*2
}


# logaritmic Transformation -----------------------------------------------


data <- ts(data.model, start=c(1,1))
data <- log(data)
data <- ts(data, start=c(1,1))
st <- adf.test(data)
diff4 <-0
while  (st$p.value > 0.05)
{
  data <- diff(data, differences = 1)
  diff4<- diff4+1
  st <- adf.test(data)
  #st
}
#plot(data)
if (r==1)
{
  xregd4 <- reggt
  i<-diff4
  while (i > 0)
  {
    xregd4 <- xregd4[-1]
    i <- i-1}
  xregd4 <- na.remove(xregd4)
  sample.model4 <- auto.arima (data, xreg=xregd4, allowdrift = FALSE)
  sample.model4
  sig4 <- (1-pnorm(abs(sample.model4$coef)/sqrt(diag(sample.model4$var.coef))))*2
}

if (r==0) 
{
  sample.model4 <- auto.arima (data, allowdrift = FALSE, allowmean = TRUE)
  #print(sample.model4)
  forecast(sample.model4,h=10)
  sig4 <- (1-pnorm(abs(sample.model4$coef)/sqrt(diag(sample.model4$var.coef))))*2
}



# model comparison --------------------------------------------------------

#Indicating last 20 percent of the data
l <- length(data)*0.2
l <- round(l,digits = 0)
l <- (l/2)

# Calculating RMSE for without transformation
data <- ts(data.model, start=c(1,1))
datae <- ts(data.model, start=c(1,1))
reggt.validation <- reggt
model<- sample.model1
ar <-model$arma[1]
ma <-model$arma[2]
i <-model$arma[6]
rmse1 <- 0

for (o in 1:l)
{
  
  observedy <- data[length(data)]
  data <- data[-length(data)]
  reggt.validation <- reggt.validation[-length(reggt.validation)]
  
 
  if (r==1)
  {
      regsig1 <- sig1[length(sig1)]
      p <- forecast(Arima(y = data, xreg =reggt.validation,  order = c(ar,diff1+i,ma),include.constant = FALSE,method = "ML"),h=1,xreg=reggt.validation) 
      meanp <-p$mean[1]
        
  }

  if (r==0)
  {
  
      p <- ( forecast(Arima(y = data, order = c(ar,diff1+i,ma),method = "CSS")))
      meanp <-p$mean[1]
  }
  rmse1 <- (((observedy)- meanp)^2)+rmse1
}
rmse1 <- ((rmse1)^0.5)/l



# Calculating RMSE for Ratio Transformation  
data <- ts(data.model, start=c(1,1))
datae <-ts(data.model, start=c(1,1))
reggt.validation <- reggt
model<- sample.model2
data <- data[-1]/data[-length(data)]
ar <-model$arma[1]
ma <-model$arma[2]
i <-model$arma[6]
rmse2 <- 0
for (o in 1:l)
{
  observedy <- data[length(data)]
  data <- data[-length(data)]
  reggt.validation <- reggt.validation[-length(reggt.validation)]
  
  if (r==1)
  {
    regsig2 <- sig2[length(sig2)]
    p <- forecast(Arima(y = data, xreg =reggt.validation[-1],  order = c(ar,diff2+i,ma),include.constant = FALSE,method = "ML"),h=1,xreg=reggt.validation) 
    le <-  datae[length(datae)-o]
    meanp <- (p$mean[1]*le)
  }
  if (r==0)
  {
    p <- ( forecast(Arima(y = data, order = c(ar,diff2+i,ma),method = "CSS"),h=1) )
    le <-  datae[length(datae)-o]
    meanp <- (p$mean[1]*le)
  }
  rmse2 <- (((datae[length(datae)-(o-1)])- meanp)^2)+rmse2

}  
rmse2 <- ((rmse2)^0.5)/l


# Calculating RMSE for Power Transformation
 data <- ts(data.model, start=c(1,1))
 datae <-ts(data.model, start=c(1,1))
 reggt.validation <- reggt
 model<- sample.model3
 tk_index(data)
 data <- data^(1/tk_index(data))
 data <- ts(data, start=c(1,1))
 ar <-model$arma[1]
 ma <-model$arma[2]
 i <-model$arma[6]
 rmse3 <- Inf
 for (o in 1:l)
 {
   observedy <- datae[length(datae)]
   datae <- datae[-length(datae)]
   data <- data[-length(data)]
   reggt.validation <- reggt.validation[-length(reggt.validation)]
   if (r==1)
   {
      regsig3 <- sig3[length(sig3)]
      p <- forecast(Arima(y = data, xreg =reggt.validation,  order = c(ar,diff3+i,ma),include.constant = FALSE, method = "ML"),h=1,xreg=reggt.validation)
      meanp <- (p$mean[1])^(length(data)+1)
   }
   if (r==0)
   {
 
      p <- ( forecast(Arima(y = data, order = c(ar,diff3+i,ma),method = "ML"),h=1 ))
      meanp <- (p$mean[1])^(length(data)+1)
   }
   rmse3 <- ((meanp- observedy)^2)+rmse3
 }
 rmse3 <- ((rmse3)^0.5)/l
#rmse3 <- Inf


# Calculating RMSE for logaritmic Transformation
data <- ts(data.model, start=c(1,1))
datae <-ts(data.model, start=c(1,1))
reggt.validation <- reggt
model <- sample.model4
data <- log(data)
ar <-model$arma[1]
ma <-model$arma[2]
i <-model$arma[6]
rmse4 <- 0
for (o in 1:l)
{
  observedy <- datae[length(datae)]
  datae <- datae[-length(datae)]
  data <- data[-length(data)]
  reggt.validation <- reggt.validation[-length(reggt.validation)]
  
  if (r==1)
  {
    regsig4 <- sig4[length(sig4)]
    p <- forecast(Arima(y = data, xreg =reggt.validation,  order = c(ar,diff4+i,ma),include.constant = FALSE,method = "ML"),h=1,xreg=reggt.validation)
    meanp <- exp(p$mean[1])
  }
  if (r==0)
  {
    p <- ( forecast(Arima(y = data, order = c(ar,diff4+i,ma))) )
    meanp <- exp(p$mean[1])
  }
  rmse4 <- ((meanp- observedy)^2)+rmse4
}
rmse4 <- ((rmse4)^0.5)/l  
  

# Model selection ---------------------------------------------------------

#model selection (could be based on AIC,BIC,AICc,RMSE)
# based on AIC   
#u <- min(sample.model1$aic,sample.model2$aic,sample.model3$aic,sample.model4$aic)
# based on RMSE
u <- min(c(0.9*rmse1,0.9*rmse2,rmse3,rmse4))

if (u==0.9*rmse1)
{
  name <- "Without Transformation"
  RMSE <- rmse1
  regpvalue <- sig1[length(sig1)]
  
  if (regpvalue > 0.05)
  {
    r <-0
  }
  
  data <- ts(data.model, start=c(1,1))
  model<- sample.model1
  diference <- diff1
  ar <-model$arma[1]
  ma <-model$arma[2]
  i <-model$arma[6]
  
  
  meanpr <- c()
  meanur <- c()
  meanlr <- c()
  if (r==1)
  {
    
    for (ii in 1:10)
    {
      t <- Arima(y = data, xreg =reggt,  order = c(ar,diff1+i,ma),include.constant = FALSE,method = "CSS")  
      p <- (forecast(t,data,xreg =reggp[ii]))
      meanpr[ii] <- (p$mean[1])
      meanur[ii] <- (p$upper[1])
      meanlr[ii] <- (p$lower[1])
      
      y <-length(data)+1
      length(data) <-y
      data [y] <- p$mean[1]
      #data <- ts(data, start=c(1,1))
      
      y <-length(reggt)+1
      length(reggt) <-y
      reggt [y] <- 0
      #reggt <- ts(reggt, start=c(1,1))
      
    }
    meanpr <- ts(meanpr, start=c(1,length(data.model)+1))
    meanur <- ts(meanur, start=c(1,length(data.model)+1))
    meanlr <- ts(meanlr, start=c(1,length(data.model)+1))
  }
 
  
  data <- ts(data.model, start=c(1,1))
  reggt <- regg$reggt
  
  p <- ( forecast(Arima(y = data, order = c(ar,diff1+i,ma),method="ML")) )
 
  meanp <-p$mean
  meanu <-p$upper
  meanl <-p$lower
  
  meanp <- ts(meanp, start=c(1,length(data.model)+1))
  meanu <- ts(meanu, start=c(1,length(data.model)+1))
  meanl <- ts(meanl, start=c(1,length(data.model)+1))
}


if (u==0.9*rmse2)
{
  name <- "Ratio Transformation"
  RMSE <- rmse2
  regpvalue <- sig2[length(sig2)]
  if (regpvalue > 0.05)
  {
    r <-0
  }
  data <- ts(data.model, start=c(1,1))
  model<- sample.model2
  diference <- diff2
  data <- data[-1]/data[-length(data)]
  ar <-model$arma[1]
  ma <-model$arma[2]
  i <-model$arma[6]
  meanpr <- c()
  meanur <- c()
  meanlr <- c()
  
  
  if (r==1)
    {
    regsig2 <- sig2[length(sig2)]
    p <- (forecast(Arima(y = data, xreg =reggt[-1],  order = c(ar,diff2+i,ma)),xreg=reggp) )
    data <- ts(data.model, start=c(1,1))
    l <-  data[length(data)]
    
    meanpr <- p$mean
    meanur <- p$upper
    meanlr <- p$lower
    for (i in (length(data)+1):(length(data)+10))
    {
      meanpr[i-length(data)] <- (p$mean[i-length(data)]*l)
      meanur[i-length(data),] <- (p$upper[i-length(data),]*l)
      meanlr[i-length(data),] <- (p$lower[i-length(data),]*l)
      l<- meanpr[i-length(data)]}
    meanpr <- ts(meanpr, start=c(1,length(data.model)+1))
    meanur <- ts(meanur, start=c(1,length(data.model)+1))
    meanlr <- ts(meanlr, start=c(1,length(data.model)+1))
    
     }
  
  data <- ts(data.model, start=c(1,1))
  data <- data[-1]/data[-length(data)]
  reggt <- regg$reggt
  ar <-model$arma[1]
  ma <-model$arma[2]
  i <-model$arma[6]
  p <- ( forecast(Arima(y = data, order = c(ar,diff2+i,ma))) )
  data <- ts(data.model, start=c(1,1))
  l <-  data[length(data)]
  meanp <- p$mean
  meanu <- p$upper
  meanl <- p$lower
  for (i in (length(data)+1):(length(data)+10))
  { 
    
    meanp[i-length(data)] <- (p$mean[i-length(data)]*l)
    meanu[i-length(data),] <- (p$upper[i-length(data),]*l)
    meanl[i-length(data),] <- (p$lower[i-length(data),]*l)
    l<- meanp[i-length(data)]
    
  }
  
  meanp <- ts(meanp, start=c(1,length(data.model)+1))
  meanu <- ts(meanu, start=c(1,length(data.model)+1))
  meanl <- ts(meanl, start=c(1,length(data.model)+1))
 
  
}


if (u==rmse3)
{
  name <- "Power Transformation"
  RMSE <- rmse3
  regpvalue <- sig3[length(sig3)]
  if (regpvalue > 0.05)
  {
    r <-0
  }
  data <- ts(data.model, start=c(1,1))
  model<- sample.model3
  diference <- diff3
  tk_index(data)
  data <- data^(1/tk_index(data))
  data <- ts(data, start=c(1,1))
  ar <-model$arma[1]
  ma <-model$arma[2]
  i <-model$arma[6]
   
  meanpr <- c()
  meanur <- c()
  meanlr <- c()
  
  if (r==1)
  {
    
    for (ii in (length(data)+1):(length(data)+10))
    {
      t <- Arima(y = data, xreg =reggt,  order = c(ar,diff3+i,ma),include.constant = FALSE,method = "CSS")  
      p <- (forecast(t,data,xreg =reggp[ii]))
      meanpr[ii-length(data)] <- (p$mean[ii-length(data)]^ii)
      meanur[ii-length(data)] <- (p$upper[ii-length(data)]^ii)
      meanlr[ii-length(data)] <- (p$lower[ii-length(data)]^ii)
      
      
      y <-length(data)+1
      length(data) <-y
      data [y] <- p$mean[1]
      #data <- ts(data, start=c(1,1))
      
      y <-length(reggt)+1
      length(reggt) <-y
      reggt [y] <- 0
      #reggt <- ts(reggt, start=c(1,1))
    }
    meanpr <- ts(meanpr, start=c(1,length(data.model)+1))
    meanur <- ts(meanur, start=c(1,length(data.model)+1))
    meanlr <- ts(meanlr, start=c(1,length(data.model)+1))
  }
  
  data <- ts(data.model, start=c(1,1))
  tk_index(data)
  data <- data^(1/tk_index(data))
  data <- ts(data, start=c(1,1))
  reggt <- regg$reggt
  p <- ( forecast(Arima(y = data, order = c(ar,diff3+i,ma))) )
  for (i in (length(data)+1):(length(data)+10))
  {
    meanp[i-length(data)] <- (p$mean[i-length(data)]^i)
    meanu[i-length(data)] <- (p$upper[i-length(data)]^i)
    meanl[i-length(data)] <- (p$lower[i-length(data)]^i)
  }
  meanp <- ts(meanp, start=c(1,length(data.model)+1))
  meanu <- ts(meanu, start=c(1,length(data.model)+1))
  meanl <- ts(meanl, start=c(1,length(data.model)+1))
  
}



if (u==rmse4)
{
  name <- "logaritmic Transformation"
  RMSE <- rmse4
  regpvalue <- sig4[length(sig4)]
  if (regpvalue > 0.05)
  {
    r <-0
  }
  data <- ts(data.model, start=c(1,1))
  model <- sample.model4
  diference <- diff4
  data <- log(data)
  ar <-model$arma[1]
  ma <-model$arma[2]
  i <-model$arma[6]
  
  meanpr <- c()
  meanur <- c()
  meanlr <- c()
  if (r==1)
  {
    
    for (ii in 1:10)
    {
      t <- Arima(y = data, xreg =reggt,  order = c(ar,diff4+i,ma),include.constant = FALSE,method = "CSS")  
      #t$coef[3] <- 0.2
      p <- (forecast(t,data,xreg =reggp[ii]))
      meanpr[ii] <- exp(p$mean[1])
      meanur[ii] <- exp(p$upper[1])
      meanlr[ii] <- exp(p$lower[1])
      
      y <-length(data)+1
      length(data) <-y
      data [y] <- p$mean[1]
      #data <- ts(data, start=c(1,1))
      
      y <-length(reggt)+1
      length(reggt) <-y
      reggt [y] <- 0
      #reggt <- ts(reggt, start=c(1,1))
      
    }
    meanpr <- ts(meanpr, start=c(1,length(data.model)+1))
    meanur <- ts(meanur, start=c(1,length(data.model)+1))
    meanlr <- ts(meanlr, start=c(1,length(data.model)+1))
  }
  
  data <- ts(data.model, start=c(1,1))
  data <- log(data)
  reggt <- regg$reggt
  p <- ( forecast(Arima(y = data, order = c(ar,diff4+i,ma)) ))
  meanp <- exp(p$mean[])
  meanu <- exp(p$upper[])
  meanl <- exp(p$lower[])
  
  meanp <- ts(meanp, start=c(1,length(data.model)+1))
  meanu <- ts(meanu, start=c(1,length(data.model)+1))
  meanl <- ts(meanl, start=c(1,length(data.model)+1))
}

model$arma[6] <- model$arma[6] + diference

# Output ------------------------------------------------------------------


output <- list()
output$dates <- dates
output$data <- ts(data.model, start=c(1,1))
output$country <- country
output$name <- name
output$model <- model
output$RMSE <- RMSE
output$regpvalue <- regpvalue
if(r==1){
  output$meanp <- meanp
  output$meanu <- meanu
  output$meanl <- meanl
  output$meanpr <- meanpr
  output$meanur <- meanur
  output$meanlr <- meanlr
}
if(r==0){
  output$meanp <- meanp
  output$meanu <- meanu
  output$meanl <- meanl
}

return(output)

}