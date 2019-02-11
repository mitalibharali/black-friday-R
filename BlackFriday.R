getwd()
library(forecast)
help("forecast")

library(leaps)
help("leaps")

# open blackfriday_cleaned.csv
blackfriday.df <- read.csv("BlackFriday_cleaned.csv")
View(blackfriday.df)

# select variables for regression
selected.var <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

# partition data
set.seed(1)  # set seed for reproducing the partition
numberOfRows <- nrow(blackfriday.df)
train.index <- sample(numberOfRows, numberOfRows*0.6)
train.df <- blackfriday.df[train.index, selected.var]
valid.df <- blackfriday.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on all 11 predictors in the
# use . after ~ to include all the remaining columns in train.df as predictors.
blackfriday.lm <- lm(Purchase ~ ., data = train.df)

#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(blackfriday.lm)


# use predict() to make predictions on a new set. 
blackfriday.lm.pred <- predict(blackfriday.lm, valid.df)
options(scipen=999, digits = 0)
some.residuals <- valid.df$Product[1:20] - blackfriday.lm.pred[1:20]
data.frame("Predicted" = blackfriday.lm.pred[1:20], "Actual" = valid.df$Purchase[1:20],
           "Residual" = some.residuals)


# use accuracy() to compute common accuracy measures.
# From help file (??accuracy) the measures calculated are:
#  ME: Mean Error
#  RMSE: Root Mean Squared Error
#  MAE: Mean Absolute Error
#  MPE: Mean Percentage Error
#  MAPE: Mean Absolute Percentage Error
#  MASE: Mean Absolute Scaled Error

options(scipen=999, digits = 3)
accuracy(car.lm.pred, valid.df$Price)


car.lm.pred <- predict(car.lm, valid.df)
all.residuals <- valid.df$Price - car.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")


# use regsubsets() in package leaps to run an exhaustive search. 
# unlike with lm, categorical predictors must be turned into dummies manually.
??regsubsets
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2


# use step() to run stepwise regression.
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)  # Which variables were dropped?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)


# create model with no predictors
car.lm.null <- lm(Price~1, data = train.df)

# use step() to run forward regression.
car.lm.step <- step(car.lm.null, scope=list(lower=car.lm.null, upper=car.lm), direction = "forward")
summary(car.lm.step)  # Which variables were added?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

# use step() to run stepwise regression.
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step) # Which variables were dropped/added?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

