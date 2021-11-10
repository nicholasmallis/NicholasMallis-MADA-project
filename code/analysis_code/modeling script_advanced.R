
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


# Code that performs statistical analyses of your data using the approaches
# we cover in class, such as: train/test split, cross-validation, trying
# different models, exploring model quality (performance, uncertainty, diagnostics,
# etc.). Depending on your data and question, not all approaches will make sense for 
# your data. Choose the ones that make sense. E.g., if you happen to do an analysis 
# of text or high-dimensional data, use methods/models appropriate for that data. 
#The main point is that you should show you understand the main concepts regarding 
# analysis and model evaluation and can apply them to your data with the tools we covered.


# for reproducibility
set.seed(123)


# split train and test, using the outcome as stratifier
data_split <- rsample::initial_split(mydata,strata = 'pct_vax_sr')
# Create data frames for the two sets:
train_data <- rsample::training(data_split)
test_data  <- rsample::testing(data_split)
# create CV object from training data
cv_data <- rsample::vfold_cv(train_data, v = 5, repeats = 5, strata = 'pct_vax_sr')




# Preprocessing Create a recipe for the model fitting. We 
# don't need to remove any NA or do imputation or standardization
# or anything else.Therefore our recipe is fairly short, we just code
# all categorical variables as dummy variables.

fit_recipe <- recipe(pct_vax_sr ~ pct_bach_sr 
+ locality + median_income_sr + unemployment_sr + pct_poverty_sr , data = train_data) 



# Null model
# For a **continuous outcome**, using RMSE as our 
# performance metric, a null-model that doesn't 
# use any predictor information is one that always
# just predicts the mean of the data. We'll compute 
# the performance of such a "model" here. It's useful 
# for comparison with the real models. We'll print both 
# numbers here, and then compare with our model results below. 
# Since our performance metric is RMSE, we compute that here with
# the "model prediction" always just being the mean of the outcomes.



RMSE_null_train <- sqrt(sum( (train_data$pct_vax_sr - mean(train_data$pct_vax_sr))^2 )/nrow(train_data))
RMSE_null_test <- sqrt(sum( (test_data$pct_vax_sr - mean(test_data$pct_vax_sr))^2 )/nrow(test_data))
print(RMSE_null_train)
print(RMSE_null_test)




# Fit/tune different models

#Now let's fit a few models.

## Tree model

### Tree model setup 


# define the model
tree_model <-  decision_tree() %>% 
set_args( cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>% 
set_engine("rpart") %>% 
set_mode("regression")           

#set workflow
tree_wf <- workflow() %>%
  add_model(tree_model) %>%
  add_recipe(fit_recipe)


### Tree model tuning
# Define tune grid and do tuning. 
# This might take a while. Note that 
# there are many ways you can do the tuning with `tidymodels`.
# Also note that I'm using parallel computing below. 
# This makes things run faster, but can at times be iffy. If you get weird error messages, you might want to comment out each command that has to do with parallel running, namely the `makePSOCKcluster`, `registerDoParallel` and `stopCluster()` commands everywhere. This just means your code will run longer.



#for parallel computing
#makes things faster. If not wanted, can be commented out, together with last line of this block.
#ncores = 18 #adjust based on your computer. 
#cl <- makePSOCKcluster(ncores)
#registerDoParallel(cl)
#tuning grid
tree_grid <- dials::grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 5)
#tune the model
tree_tune_res <- tree_wf %>% 
  tune::tune_grid(
    resamples = cv_data,
    grid = tree_grid,
    metrics = yardstick::metric_set(rmse) 
  )



### Tree model evaluation

#Now that tuning is done, look at the tuning process a bit. 
# This plots model performance during tuning.

#see a plot of performance for different tuning parameters
tree_tune_res %>% autoplot()

# Getting the model that was determined to be best (via cross-validation) by tuning.


# get the tuned model that performs best 
best_tree <- tree_tune_res %>%  select_best(metric = "rmse")
# finalize workflow with best model
best_tree_wf <- tree_wf %>% finalize_workflow(best_tree)
# fitting best performing model
best_tree_fit <- best_tree_wf %>% 
  fit(data = train_data)
#predicting outcomes for final model
tree_pred <- predict(best_tree_fit, train_data)



# Plotting final tree.
rpart.plot(extract_fit_parsnip(best_tree_fit)$fit, roundint=FALSE)



# This is a bad tree. I got a different tree with a bit
# more tuning (though an overall not much better model).


# Plotting observed/predicted and residuals.


#predicted versus observed
plot(tree_pred$.pred,train_data$pct_vax_sr, xlim =c(97,103), ylim=c(97,103))
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
#residuals
plot(tree_pred$.pred-train_data$pct_vax_sr)
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall


# We can see that the model only predicts 2 different values. Not a great model.


# Looking at model performance. I think it would be more intuitive 
# to get the performance from the `best_tree_fit` object, 
# but I can't figure out how to do that.

tree_perfomance <- tree_tune_res %>% show_best(n = 1)
print(tree_perfomance) # Tree performance on train data. Mean RMSE = 2.14

print(RMSE_null_train) #Null Model performanceon train data. RMSE=2.19



# Comparing the RMSE to the null model, we see that it is not much better. 
# Based on our model evaluation, I think we can safely say here 
# that a tree-based model is no good.


# fit on the training set and evaluate on test set
final_fit <- best_tree_wf  %>% last_fit(data_split)


test_performance <- final_fit %>% collect_metrics()
print(test_performance)



# Importance of different variables
library(vip)

final_fit %>% 
  extract_fit_parsnip() %>% 
  vip()






# Linear Regression
#Fit linear models to the continuous outcome: pct_vax_sr

#Recipe() has two arguments: a formula and the data
fit_recipe_bach <- recipe(pct_vax_sr ~ pct_bach_sr, data = train_data) #all predictors

#Build a model specification using the parsnip package
lm_mod <- linear_reg() %>%
  set_engine("lm")

#Model workflow pairs a model and recipe together
lr_vax_workflow <- 
  workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(fit_recipe)

#Main predictor
lr_vax_workflow_simple <- 
  workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(fit_recipe_bach)

lr_vax_workflow
lr_vax_workflow_simple

#The last_fit() function will fit the model to the training data and 
#calculate the prediction on the test data
lr_vax_fit <- 
  lr_vax_workflow %>%
  last_fit(split = data_split)

#Main predictor
lr_vax_fit_simple <- 
  lr_vax_workflow_simple %>%
  last_fit(split = data_split)

#Extract the fitted model object and use tidy() to get a tidy tibble 
#of model coefficients
lr_vax_fit %>%
  extract_fit_parsnip() %>%
  tidy()  

#Main predicotr
lr_vax_fit_simple %>%
  extract_fit_parsnip() %>%
  tidy()  

#Model Evaluaton:
#Look at predictions and RMSE for my data
#****************************************

#Collect predictions
test_results <- lr_vax_fit %>%
  collect_predictions()

#Main predictor
test_results_main <- lr_vax_fit_simple %>%
  collect_predictions()

test_results
test_results_main

#Collect metrics
lr_vax_fit %>%
  collect_metrics() #rmse = 2.14 , r-squared = 0.0923

#Main predictor performs worse according to residual squared error and r-squared
lr_vax_fit_simple %>%
  collect_metrics() #rmse = 2.17, r-squared = 0.0639 

#R-squared plot to visualize model performance on the test dataset
#Does this scatterplot still make sense when there are binary precictors included?
test_results %>%
  ggplot(aes(x = .pred, y = pct_vax_sr)) + 
  geom_abline(lty=2) + 
  geom_point(color = "blue", alpha = 0.5) +
  coord_obs_pred() +
  labs(title = 'Linear Regression Results, all', 
       x = "Predicted Percent Vaccination (Square Root)",
       y = "Actual Percent Vaccination (Square Root)")

#Apply prediction to the training data instead
#The following steps are no longer necessary when using the above last_fit function:
#***********************************************************************************

#Models are the same, workflows are the same, just change fit()
#Not using last_fit() this time, so it won't automatically calculate predictions on the test data
#Now have to add predict() step

lr_vax_fit_train <- 
  lr_vax_workflow %>%
  fit(data = train_data) 

#Main predictor
lr_vax_fit_simple_train <- 
  lr_vax_workflow_simple %>%
  fit(data = train_data) 

#Extract the fitted model object and use tidy() to get a tidy tibble 
#of model coefficients
lr_vax_fit_train %>%
  extract_fit_parsnip() %>%
  tidy()  

#Main predicotr
lr_vax_fit_simple_train %>%
  extract_fit_parsnip() %>%
  tidy()  

#Use the trained workflow to predict with the *trained data* (isntead of the test data)
#Returns predicted class
predict(lr_vax_fit_train, train_data)
predict(lr_vax_fit_simple_train, train_data) #main predictor

#Returns predicted class probabiliies
#Compare the observed alue and the predicted value
pct_vax_aug <- augment(lr_vax_fit_train, train_data) %>%
  select(pct_vax_sr, .pred)
pct_vax_aug

rmse(pct_vax_aug , truth = pct_vax_sr, estimate = .pred) #2.11
rsq(pct_vax_aug , truth = pct_vax_sr,  estimate = .pred) # 0.0712

#Main predictor
pct_vax_aug_simple <- augment(lr_vax_fit_simple_train, train_data) %>%
  select(pct_vax_sr, .pred)
pct_vax_aug_simple

rmse(pct_vax_aug_simple, truth = pct_vax_sr, estimate = .pred) #2.14
rsq(pct_vax_aug_simple, truth = pct_vax_sr,  estimate = .pred) #0.0460

#Goal is to minimize the root mean square error (RMSE) and maximize r-squared, so
#the model with only one predictor performs worse than keeping all predictors

#R-squared plot to visualize model performance on the training dataset
#Does this scatterplot still make sense when there are binary precictors included?
pct_vax_aug %>%
  ggplot(aes(x = .pred, y = pct_vax_sr)) + 
  geom_abline(lty=2) + 
  geom_point(color = "blue", alpha = 0.5) +
  coord_obs_pred() +
  labs(title = 'Linear Regression Results, Training, all', 
       x = "Predicted Percent Vaccination (Square Root)",
       y = "Actual Percent Vaccination (Square Root)")





#LASSO


#model
lasso_model <- linear_reg() %>%
  set_mode("regression") %>%           
  set_engine("glmnet") %>%
  set_args(penalty = tune(), mixture = 1) #mixture = 1 means we use the LASSO model
#workflow
lasso_wf <- workflow() %>%
  add_model(lasso_model) %>% 
  add_recipe(fit_recipe)


### LASSO tuning


#tuning grid
lasso_reg_grid <- tibble(penalty = 10^seq(-3, 0, length.out = 30))
#tune model
lasso_tune_res <- lasso_wf %>% 
  tune_grid(resamples = cv_data,
            grid = lasso_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse)
  )


### LASSO evaluation
#see a plot of performance for different tuning parameters
lasso_tune_res %>% autoplot()


# get the tuned model that performs best 
best_lasso <- lasso_tune_res %>%  select_best(metric = "rmse")
# finalize workflow with best model
best_lasso_wf <- lasso_wf %>% finalize_workflow(best_lasso)
# fitting best performing model
best_lasso_fit <- best_lasso_wf %>% 
  fit(data = train_data)
lasso_pred <- predict(best_lasso_fit, train_data)



#Plotting LASSO variables as function of tuning parameter

x <- best_lasso_fit$fit$fit$fit
plot(x, "lambda")


# As one sees, the larger the regularization penalty,
# the fewer predictor variables that remain in the model. 
# (Once a coefficient is at 0, the corresponding variable 
# is not in the model anymore). This shows the variables 
# that are part of the best-fit LASSO model, i.e. those 
# that have a non-zero coefficient.


tidy(extract_fit_parsnip(best_lasso_fit)) %>% filter(estimate != 0)


#Plotting observed/predicted and residuals.
#predicted versus observed
plot(lasso_pred$.pred,train_data$BodyTemp, xlim =c(97,103), ylim=c(97,103))
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
#residuals
plot(lasso_pred$.pred-train_data$BodyTemp)
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall



# The diagnostic plots show that this model 
# isn't much better either. We want the points
# to be along the red lines in each plot. They are not.



#Looking at model performance. 

lasso_perfomance <- lasso_tune_res %>% show_best(n = 1)
print(lasso_perfomance)








# Loading the library
library(glmnet)

# Loading the data
data(swiss)

pct_vax_sr ~ pct_bach_sr 
+ locality + median_income_sr + unemployment_sr + pct_poverty_sr

x_vars <- model.matrix(pct_vax_sr ~ pct_bach_sr 
                       + locality + median_income_sr + unemployment_sr + pct_poverty_sr , mydata)[,-1]
y_var <- mydata$pct_vax_sr
lambda_seq <- 10^seq(2, -2, by = -.1)

# Splitting the data into test and train
set.seed(123)
train = sample(1:nrow(x_vars), nrow(x_vars)/2)
x_test = (-train)
y_test = y_var[x_test]

# for reproducibility
set.seed(123)


# split train and test, using the outcome as stratifier
data_split <- rsample::initial_split(mydata,strata = 'pct_vax_sr')
# Create data frames for the two sets:
train_data <- rsample::training(data_split)
test_data  <- rsample::testing(data_split)
# create CV object from training data
cv_data <- rsample::vfold_cv(train_data, v = 5, repeats = 5, strata = 'pct_vax_sr')


cv_output <- cv.glmnet(x_vars[train,], y_var[train],
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam




