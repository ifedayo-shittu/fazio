library(tidyverse)
data <- read_csv("C:/Users/shitt/Desktop/Heart Failure Clinical Records/heart_failure_clinical_records_dataset (1).csv")
str(data)
library(neuralnet)


#normalization

data$age <- (data$age - min(data$age))/(max(data$age) - min(data$age))

data$ creatinine_phosphokinase <- (data$ creatinine_phosphokinase - min(data$ creatinine_phosphokinase))/(max(data$ creatinine_phosphokinase) - min(data$ creatinine_phosphokinase))
data$ejection_fraction <- (data$ejection_fraction - min(data$ejection_fraction))/(max(data$ejection_fraction) - min(data$ejection_fraction))
data$platelets <- (data$platelets - min(data$platelets))/(max(data$platelets) - min(data$platelets))
data$serum_creatinine <- (data$serum_creatinine - min(data$serum_creatinine))/(max(data$serum_creatinine) - min(data$serum_creatinine))
data$serum_sodium <- (data$serum_sodium - min(data$serum_sodium))/(max(data$serum_sodium) - min(data$serum_sodium))
data$time <- (data$time - min(data$time))/(max(data$time) - min(data$time))

#checking the histogram

hist(data$age)
hist(data$DEATH_EVENT)
hist(data$anaemia)

str(data)
summary(data)
view(data)

# Data Partition





library(rsample)

set.seed(112)
idx <- sample(nrow (data), 0.7*nrow(data), replace = FALSE)
train <- data[idx,]
test <- data[-idx,]

library(neuralnet)
model <- neuralnet( 
  DEATH_EVENT ~., 
  data=test, hidden=c(3,5), threshold=0.01,
  linear.output = FALSE)

plot(model, rep="best")

model$response
model$result.matrix


#Prediction

output <- compute(model, test[])
head(output$net.result)
head(test[1,])


#confusionMatrix
output <- compute(model, test)
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(pred1, test$DEATH_EVENT)
tab1
1-sum(diag(tab1))/sum(tab1)








