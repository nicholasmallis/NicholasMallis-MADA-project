
#loading packages
library(broom)
library(here) #for data loading/saving
library(tidyverse)
library(recipes)
library(tidymodels)
library(workflowr) 
library(parsnip)
library(rsample)
library(rpart)
library(glmnet)
library(ranger)
library(modeldata)
library(rpart.plot)
library(dials)
library(workflows)
library(vip)
library(glmnet)
library(yardstick)
library(doParallel) # for parallel computing 

# setting path
data_loc <- here::here("data","processed_data","processeddata.rds")

# reading in data
mydata <- readRDS(data_loc)

glimpse(mydata)


#first things first, let's delete all data with missing observations

mydata <- na.omit(mydata)


#since all of our continous outcomes were skewed, let's apply a square root 
#transformation to them before we get to the modeling. i choose square root 
#since there are so many values at 0.

mydata <- mydata %>%
  mutate(pct_vax_sr = sqrt(pct_vax),
         pct_bach_sr = sqrt(pct_bachelors),
         unemployment_sr = sqrt(unemployment),
         pct_poverty_sr = sqrt(pct_poverty),
         median_income_sr = sqrt(median_income)
  )



# checking 

glimpse(mydata)



#define response variable
y <- mydata$pct_vax_sr

#define matrix of predictor variables
x <- data.matrix(mydata[, c('pct_bach_sr', 'unemployment_sr', 'pct_poverty_sr', 'median_income_sr', 'locality')])


library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda



#produce plot of test MSE by lambda value
plot(cv_model) 
