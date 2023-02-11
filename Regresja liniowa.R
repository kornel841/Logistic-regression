install.packages("tidyverse")
install.packages("caret")
install.packages("MASS")
install.packages("lmtest")
install.packages("olsrr")
install.packages("broom")
install.packages("ggpubr")
install.packages("ggplot2")
library(dplyr)
library(caret)
library(tidyverse)
library(MASS)
library(lmtest)
library(olsrr)
library(broom)
library(ggpubr)

data = read.csv("WineQT.csv", header = T)
df <- data.frame(data)
for (x_name in colnames(df)){
  for (y_name in colnames(df)){
    X = df[,x_name]
    Y = df[,y_name]
    print(paste0(y_name, "/", x_name, " corelation: ", cor.test(Y,X)[4]))
  }
  print("")
}

plot(data$density,data$fixed.acidity)

#najwieksza korelacja wyszla miedzy density (gestosc) a fixed acidity (kwasowość)

cor.test(data$density, data$fixed.acidity)
#korelacja jest widoczna ale nie doskonała 

data2 = Edited
data2 <- Edited

set.seed(123)
training.samples <- data2$Density %>%
  createDataPartition(p = 0.6, list = FALSE)
train.data  <- data2[training.samples, ]
test.data <- data2[-training.samples, ]

model <- lm(data$fixed.acidity ~ data$density, data = train.data)
summary(model)

#wykres kwantyl-kwantyl; wizuualny test dla przyjecia "normalnosci zbioru"
qqnorm(model$residuals)
qqline(model$residuals , col = "steelblue", lwd = 2) 

#test shapiro wilk dla rozkladu danych 
shapiro.test(model$residuals) #p value mniejsze od 0.05; rozklad nie jest normalny

#sprawdzanie homoskedastyczności
#wizualnie
plot(model$fitted.values , model$residuals)

#test statystyczny 
bptest(model) #p-value > 0.05 errors have constant variance

#auto-korelacja reszt
plot(model$residuals)

#test Durbina-Watsona
dwtest(model,  alternative = c("two.sided"))

#Calculating mean of errors
mean(model$residuals)

#influential point detection
#wizualnie
plot(model , 4)
#statystycznie
model_dm <- augment(model)
#sprawdzanie highest Cook's distance
max(model_dm$.cooksd)
#odleglosc cook'a nie jest wieksza od 0.05 wiec nie ma influential point w tym zbiorze 



#predykcje na zbiorze do testowania
prediction <- model %>% predict(test.data)
# wizualizacja
plot(test.data, prediction)
abline(lm(prediction ~ data2$Density, data = test.data), col = "blue")
# statystycznie
data.frame( R2 = R2(prediction, test.data$Density),
            RMSE = RMSE(prediction, test.data$Density),
            MAE = MAE(prediction, test.data$Density))



# definiowanie treningu kontroli
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 4, repeats = 3)
# trenowanie modelu
model_cv <- train(Density ~ 'fixed acidity' , data = data, method="lm",
                  trControl = train.control)
# wyniki 
print(model_cv)

