library("neuralnet")
library("dplyr")
library("ggplot2")
library("lubridate")
library("forecast")
library("fracdiff")


wig <- read.csv(file="/Users/kamilw/add/wig20_d.csv", header=TRUE, sep=",")
wig$Data <- as.Date(wig$Data, "%Y-%m-%d")


wig$Roznica <- (1 - wig$Zamkniecie / wig$Otwarcie) * 100;
wig$Roznica2 <- apply(wig, 1, FUN = function(x) {
  d <- as.numeric(x[7]);
  print(d)
  
  if (d >= 1) return (1)
  else if (d <= -1) return (-1)
  return (0);
});

head(wig, 30)


ml <- function (mlDays) {
  
}

testFunction <- function(data, date, days, endDays, method, mlDays){
  
  if (!is.function(method)) {
    print("ml");
    ml(mlDays)
    return (null)
  } else {
    startDate <- as.Date(date)
    stopDate <- floor_date(startDate + days(days), "day")
    endDate <- floor_date(stopDate + days(endDays), "day")
    
    data <- filter(data, data$Data >= startDate & data$Data <= endDate)
    days <- seq(min(data$Data), max(data$Data), by = "day")
    values <- c()
    
    for (day in days) {
      d <- which(data$Data == day);
      
      if (is.na(d[1])) {
        values <-  c(values, last);
      } else {
        last <- data$Roznica[d[1]];
        values <-  c(values, last);
      }
    }
    
    data <- data.frame(Data = days, Roznica = values)
    
    dataF <- filter(data, data$Data >= startDate & data$Data <= stopDate)
    real <- filter(data, data$Data >= stopDate & data$Data < endDate)
    
    wn <- ts(values, start=stopDate, frequency=endDays)
    
    fit <-method(wn)
    fore <- forecast(fit, h = endDays)
    pred <- as.numeric(fore$upper)[0:endDays]
    
    arima <- real
    arima$Roznica <- pred
    
    pred <- as.numeric(fore$lower)[0:endDays]
    
    arimaB <- real
    arimaB$Roznica <- pred
    
    arimaMean <- real
    arimaMean$Roznica <-  as.numeric(fore$mean)
    
    
    xx <- ggplot() + 
      geom_point(aes(y = Roznica, x = Data), data = dataF) +
      geom_point(aes(y = Roznica, x = Data), data = real, color='purple') +
      geom_line(aes(y = Roznica, x = Data), data = arima, color='red') +
      geom_line(aes(y = Roznica, x = Data), data = arimaB, color='red') +
      geom_point(aes(y = Roznica, x = Data), data = arimaMean, color='green')
    
    
    return(xx)
  }

}

# testFunction(wig, "2012-01-05", 100, 10, HoltWinters)


