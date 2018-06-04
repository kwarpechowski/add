library("ggplot2")
library("neuralnet")
library("dplyr")


round <- 2
numberOfDays <- 5

mlDays <- seq(from = 1, to = numberOfDays, by = 1)

wig <- read.csv(file="/Users/kamilw/add/wig20_d.csv", header=TRUE, sep=",")
wig$Data <- as.Date(wig$Data, "%Y-%m-%d")

wig$Roznica <- round((1 - wig$Zamkniecie / wig$Otwarcie) * 100, digits = round);

if(FALSE) {
  wig$Roznica <- apply(wig, 1, FUN = function(x) {
    d <- as.numeric(x[7]);
    print(d)
    
    if (d >= 1) return (1)
    else if (d <= -1) return (-1)
    return (0);
  });
  
}

head(wig, 30)


row <- nrow(wig);
formulas = list()

for (day in mlDays) {
  seq <- rep(0, row)
  jedynkiSeq <- seq(from = 1 + day, to = row, by = 1)
  
  for (s in jedynkiSeq) {
    seq[s] = wig$Roznica[s - day]
  }
  name <- paste(c("Roznica", day), collapse = "")
  formulas[day] = name
  wig[,name]  <- seq;
}

for (day in mlDays) {
  wig <- wig[-1, ]
}

wig

formula <- as.formula(paste('Roznica ~ ' ,paste(formulas,collapse='+')))


formulasV <- unlist(formulas)
trainDF <- filter(wig, wig$Data >= "2012-01-01" & wig$Data <= "2016-12-31")
testDF <-  filter(wig, wig$Data >= "2017-01-01" & wig$Data <= "2017-12-31")

head(wig)

nn <- neuralnet(formula, data=trainDF, hidden=c(5,4,3), stepmax=1e6, linear.output=TRUE)

prediction <- neuralnet::compute(nn, testDF[, formulasV])

rv <-  round(prediction$net.result, digits = round)

results <- data.frame(actual = testDF$Roznica, prediction = rv)


line <- data.frame(actual = c(-5,5), prediction = c(-5,5))

par(mfrow=c(1,2))
plot(nn)
ggplot() + geom_point(aes(x = prediction, y = actual), data = results, color='red') + geom_line(aes(x = prediction, y = actual), data = line, color='green')
ggsave("plot.png")
