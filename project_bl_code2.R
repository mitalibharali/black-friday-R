getwd()
library(forecast)
library(leaps)
library(ggplot2)

# open BlackFirday.csv
BF.df <- read.csv("BF_cleaned.csv")
View(BF.df)
bf.df <- BF.df[,c(3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15)]
cor(bf.df)
set.seed(1)
numberOfRows <- nrow(BF.df)
train.index <- sample(numberOfRows, numberOfRows*0.6) #why *0.6???

train.df <- BF.df[train.index,]
validation.df <- BF.df[-train.index,]

Purchase1.lm <- lm(formula = Purchase ~ Occupation, data = train.df)
Purchase2.lm <- lm(formula = Purchase ~ Gender_M + Gender_F, data = train.df)
Purchase3.lm <- lm(formula = Purchase ~ Gender_M+ Gender_F + Stay_In_Current_City_Years, data = train.df)

Purchase4.lm <- lm(formula = Purchase ~ AGE + Gender_M + Gender_F + Marital_Status + City_Cat_A + City_Cat_B + City_Cat_C, data = train.df)


options(scipen = TRUE)
summary(Purchase1.lm)
summary(Purchase3.lm)
summary(Purchase4.lm)


Purchase4.pred <- predict(Purchase4.lm, validation.df)
accuracy(Purchase4.pred, validation.df$Purchase)

Purchase1.pred <- predict(Purchase1.lm, validation.df)
accuracy(Purchase1.pred, validation.df$Purchase)

Purchase3.pred <- predict(Purchase3.lm, validation.df)
accuracy(Purchase3.pred, validation.df$Purchase)

# stepwise regression.
??step
PurchaseAmount.lm <- lm(formula = Purchase ~ ., data = train.df[-14]) #why 14? is it good enuough for our data
PurchaseAmount.lm.step <- step(PurchaseAmount.lm, direction = "both")
summary(PurchaseAmount.lm.step)  # Which variables were dropped/added?
BF.lm.step.pred <- predict(PurchaseAmount.lm.step, validation.df)
accuracy(BF.lm.step.pred, validation.df$MEDV)

all.residuals <- validation.df$MEDV - BF.lm.step.pred
all.residuals.df <- data.frame(all.residuals)
ggplot(all.residuals.df) + geom_histogram(aes(x = all.residuals), binwidth = 1)

plot(density(BF.df$Purchase))


