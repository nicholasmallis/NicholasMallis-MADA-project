# ADVANCED MODELING SCRIPT

# The following script performs several different modeling approaches to 
# the data using the tidy models framework. First we do a little bit of 
# data management before modeling and also check the correlations between variables

# Then we perform a test/train split on the data, putting 3/4 into the train set.
# Then we run the following models in this order, plotting diagnostics and 
# calculating the RMSE for each. For the LASSO and Decision Tree models, 
# we used a 5-fold cross-validation, 5 times repeated, creating a 
# resample object for the training data. 

# 1) Full Model
# 2) Simple model with only Education as the predictor
# 3) LASSO Model
# 4) Decision Tree Model

# We also run a few other simple models for each variable 
# and compare the RMSE with the null.

# As you'll see, none of the models perform that well,
# but our main predictor seems to be the most important variable.


#loading packages
library(broom) #tidying tables
library(vip) #plotting decision tree importance
library(here) #for data loading/saving
library(tidyverse) #wide usage of tidying data
library(recipes) #for model recipes
library(tidymodels) #for modeling framework
library(workflowr) #creates workflow
library(parsnip) #model fitting
library(rsample) #creating resampling object for CV
library(rpart) #plotting decision tree
library(glmnet) #setting engine
library(ranger) #setting engine
library(modeldata) # for modeling
library(rpart.plot) #plotting decision tree
library(dials) #for tuning parameters
library(workflows) #creating workflows
library(yardstick) #model performance

# setting path
data_loc <- here::here("data","processed_data","processeddata.rds")

# reading in data
mydata <- readRDS(data_loc)

glimpse(mydata)



# first we need to convert locality to a factor
mydata <- mydata %>%
  mutate(locality= as.factor(locality)
         )

# now we take away the variables we don't need for modeling
mydata <- mydata[, -c(1,4,5)]

# now let's delete all data with missing observations

mydata <- na.omit(mydata)

glimpse(mydata)






#Next we'll look at how correlated some of the predictors might be
#since they rely on similar metrics

glimpse(mydata)
correlations <-  mydata[,c(-2)]

#Let's compute the correlation matrix
cor_1 <- round(cor(correlations), 2)
cor_1

correlations = here::here("results", "corr_mat.rds")
saveRDS(cor_1, file = correlations)


#It looks like median income and percent poverty are pretty highly correlated (-0.77)
#Further, percent bachelors is strongly correlated with median income (0.62)

#I am going to remove median income due the strong correlation with both our main predictor
#and percent poverty

mydata <- mydata[,c(-5)]



# NOW WE DO A DATA SPLIT 

# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(397)
# Put 3/4 of the data into the training set 
data_split <- initial_split(mydata, prop = 3/4, strata= pct_vax)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)



# THEN WE CALCULATE THE RMSE FOR THE NULL MODEL (TRAINING DATA)
RMSE_null_train <- sqrt(sum( (train_data$pct_vax - mean(train_data$pct_vax))^2 )/nrow(train_data))
print(RMSE_null_train)



# Here I create a recipe for the model fitting with all vars.
fit_recipe <- recipe(pct_vax ~ . , data = train_data)  %>%
  step_dummy(locality) # for our one categorical variable



# Set a model 
lr_mod <- 
  linear_reg() %>% 
  set_engine("lm")


# Use the workflow() package to create a
# simple workflow that fits a lm 
# to all predictors using the lm function
vax_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(fit_recipe)

# Fitting the model
vax_fit <- 
  vax_wflow %>% 
  fit(data = train_data)

# Extracting Model/Recipes with Parsnip
vax_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()


# Obtaining Predictions
predict(vax_fit , train_data)

vax_aug <- 
  augment(vax_fit , train_data)

vax_aug %>%
  select(pct_vax)


# Calculating Root RMSE 
rmse_train <- vax_aug %>% 
  rmse(truth = pct_vax, .pred)


jpeg(file = "results/full_pred_vs_obs.jpeg")
full_predicted_vs_observed <- plot(vax_aug$.pred,train_data$pct_vax, xlim =c(0,100), ylim=c(0,100), 
                                     xlab="Predicted", ylab="Observed", 
                                     main= "Predicted vs Observed from Full Model on Train Data")
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
dev.off()


jpeg(file = "results/full_residuals.jpeg")
#residuals
full_residuals <- plot(vax_aug$.pred-train_data$pct_vax, ylab="Residuals", main= "Residuals from Full Model on Train Data")
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall
dev.off()


#COMPARING TO NULL MODEL 

#Looks like our model with all predictors does better than the null at reducing
#RMSE

#12.1
rmse_train

#14.4
print(RMSE_null_train)



# Here I create a  recipe for the simple model fit with only our main predictor of interest.
fit_recipe_simple <- recipe(pct_vax ~ pct_bachelors , data = train_data)


# Use the workflow() package to create a
# simple workflow that fits a  simple lm 

vax_wflow_simple <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(fit_recipe_simple)

# Fitting the model
vax_fit_simple <- 
  vax_wflow_simple %>% 
  fit(data = train_data)

# Extracting Model/Recipes with Parsnip
vax_fit_simple %>% 
  extract_fit_parsnip() %>% 
  tidy()


# Obtaining Predictions
predict(vax_fit_simple , train_data)

vax_aug_simple <- 
  augment(vax_fit_simple , train_data)

vax_aug_simple %>%
  select(pct_vax)



jpeg(file = "results/simple_pred_vs_obs.jpeg")
simple_predicted_vs_observed <- plot(vax_aug_simple$.pred,train_data$pct_vax, xlim =c(0,100), ylim=c(0,100), 
                                    xlab="Predicted", ylab="Observed", 
                                    main= "Predicted vs Observed from Simple Model on Train Data")
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
dev.off()


jpeg(file = "results/simple_residuals.jpeg")
#residuals
simple_residuals <- plot(vax_aug_simple$.pred-train_data$pct_vax, ylab="Residuals", main= "Residuals from Simple Model on Train Data")
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall
dev.off()



# Calculating Root RMSE 
rmse_train_simple <- vax_aug_simple %>% 
  rmse(truth = pct_vax, .pred)


#COMPARING TO NULL MODEL 

#Looks like our model with all predictors does better than the null at reducing
#RMSE

#Simple Model RMSE= 12.8
rmse_train_simple

#Null Model RMSE = 14.41
print(RMSE_null_train)

#Model with all Predictors= 12.2
rmse_train


# It looks like our model with all predictors has the lowest RMSE, but this may be over fitting. Let's look
# at some other models.




# LASSO

# create CV object from training data
cv_data <- rsample::vfold_cv(train_data, v = 5, repeats = 5, strata=pct_vax)



# model
lasso_model <- linear_reg() %>%
  set_mode("regression") %>%           
  set_engine("glmnet") %>%
  set_args(penalty = tune(), mixture = 1) #mixture = 1 means we use the LASSO model

# workflow
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
# This plots shows us that model performance is generally better
# at the smaller penalty values. This suggests that the majority
# of the predictors are important to the model.
lasso_plot <- lasso_tune_res %>% autoplot()

figure_file = here::here("results","LASSO.png")
ggsave(filename = figure_file, plot= lasso_plot)

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

LASSO_tune_plot <- plot(x, "lambda")
LASSO_tune_plot

f = here::here("results","LASSO_tune_plot.png")
ggsave(filename = f, plot= LASSO_tune_plot)


# As one sees, the larger the regularization penalty,
# the fewer predictor variables that remain in the model. 
# (Once a coefficient is at 0, the corresponding variable 
# is not in the model anymore). This shows the variables 
# that are part of the best-fit LASSO model, i.e. those 
# that have a non-zero coefficient.

# It appears that all predictors stayed in the model
LASSO_vars <- tidy(extract_fit_parsnip(best_lasso_fit)) %>% filter(estimate != 0)
LASSO_vars

LASSO_table <- as.data.frame(LASSO_vars)
LASSO_table[2,1] <- "Percent Bachelor's Degree"
LASSO_table[3,1] <- "Unemployment Rate"
LASSO_table[4,1] <- "Poverty Rate"
LASSO_table[5,1] <- "Locality (Non-Metro vs Metro)"

names(LASSO_table)[1] <- "Term"
names(LASSO_table)[2] <- "Estimate"
names(LASSO_table)[3] <- "Penalty"

LASSO_table



# saved it as a file
LASSO_tab = here::here("results", "LASSO_table.rds")
saveRDS(LASSO_table, file = LASSO_tab)


#Plotting observed/predicted and residuals.
#predicted versus observed
#jpeg(file = "results/LASSO_pred_vs_obs.jpeg")
jpeg(file = "results/LASSO_pred_vs_obs2.jpeg")
LASSO_predicted_vs_observed <- plot(lasso_pred$.pred,train_data$pct_vax, xlim =c(0,100), ylim=c(0,100), 
                                    xlab="Predicted", ylab="Observed", 
                                    main= "Predicted vs Observed from LASSO Model on Train Data")
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
dev.off()


jpeg(file = "results/LASSO_residuals2.jpeg")
#residuals
LASSO_residuals <- plot(lasso_pred$.pred-train_data$pct_vax, ylab="Residuals", main= "Residuals from LASSO Model on Train Data")
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall
dev.off()

# The diagnostic plots show that this model 
# isn't much better either. We want the points
# to be along the red lines in each plot. They are not.



#Looking at model performance. 

lasso_perfomance <- lasso_tune_res %>% show_best(n = 1)
print(lasso_perfomance)

#Our best LASSO performance RMSE was 12.2 (which we would expect from the model before)

#According to the LASSO, all predictors remain in the model



# Now let's run a Decision Tree
# This might help us see which variables are most important



# define the model
tree_model <-  decision_tree() %>% 
  set_args( cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")  

# workflow
tree_wf <- workflow() %>%
  add_model(tree_model) %>%
  add_recipe(fit_recipe)


#tuning the grid

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          min_n(),
                          levels = 5)
#tune the model
tree_tune_res <- tree_wf %>% 
  tune::tune_grid(
    resamples = cv_data,
    grid = tree_grid,
    metrics = yardstick::metric_set(rmse) 
  )

tree_perform_plot <- tree_tune_res %>% autoplot()
tree_perform_plot

# get the tuned model that performs best 
best_tree <- tree_tune_res %>%  select_best(metric = "rmse")

# finalize workflow with best model
best_tree_wf <- tree_wf %>% finalize_workflow(best_tree)

# fitting best performing model
best_tree_fit <- best_tree_wf %>% 
  fit(data = train_data)

#predicting outcomes for final model
tree_pred <- predict(best_tree_fit, train_data)

#plotting the tree
rpart.plot(extract_fit_parsnip(best_tree_fit)$fit)



jpeg(file = "results/DT_pred_vs_obs.jpeg")
#predicted versus observed
Tree_predicted_vs_observed <- plot(tree_pred$.pred,train_data$pct_vax, xlim =c(0,100), ylim=c(0,100),
                                   xlab="Predicted", ylab="Observed", 
                                   main= "Predicted vs Observed from Decision Tree Model on Train Data")
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
dev.off()

jpeg(file = "results/DT_resid.jpeg")
#residuals
Tree_residuals<- plot(tree_pred$.pred-train_data$pct_vax, ylab="Residuals", main= "Residuals from Decision Tree Model on Train Data")
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall
dev.off()

tree_perfomance <- tree_tune_res %>% show_best(n = 1)

#The mean RMSE from the Tree Model was 12.7, which is higher than the LASSO.
print(tree_perfomance)



# Based on the plot, pct_bachelors is the most important variable, 
# followed by unemployment, pct_poverty, and locality
tree_important_vars_plot <- best_tree_fit %>% 
  extract_fit_parsnip() %>% 
  vip()

tree_important_vars_plot

figure = here::here("results","tree_important.png")
ggsave(filename = figure, plot= tree_important_vars_plot)



# MODELS WITH OTHER INDIVIDUAL VARIABLES

# I'm interested in how the RMSE changes when fitting models with the other predictors only

# A new recipe for unemployment...

#UNEMPLOYMENT

# Here I create a  recipe for the simple model fit with only our main predictor of interest.
fit_recipe_unmeployment <- recipe(pct_vax ~ unemployment , data = train_data)


# Use the workflow() package to create a
# simple workflow that fits a  simple lm 

vax_wflow_unemployment <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(fit_recipe_unmeployment)

# Fitting the model
vax_fit_unemployment <- 
  vax_wflow_unemployment %>% 
  fit(data = train_data)

# Extracting Model/Recipes with Parsnip
vax_fit_unemployment %>% 
  extract_fit_parsnip() %>% 
  tidy()


# Obtaining Predictions
predict(vax_fit_unemployment , train_data)

vax_aug_unemployment <- 
  augment(vax_fit_unemployment , train_data)

vax_aug_unemployment %>%
  select(pct_vax)


jpeg(file = "results/unemployment_simple_pred_vs_obs.jpeg")
unemployment_simple_predicted_vs_observed <- plot(vax_aug_unemployment$.pred,train_data$pct_vax, xlim =c(0,100), ylim=c(0,100), 
                                     xlab="Predicted", ylab="Observed", 
                                     main= "Predicted vs Observed from Unemployment Model on Train Data")
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
dev.off()


jpeg(file = "results/unemployment_simple_residuals.jpeg")
#residuals
simple_residuals <- plot(vax_aug_unemployment$.pred-train_data$pct_vax, ylab="Residuals", main= "Residuals from Unemployment Model on Train Data")
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall
dev.off()



# Calculating Root RMSE 
rmse_train_unemployment <- vax_aug_unemployment %>% 
  rmse(truth = pct_vax, .pred)

# This RMSE for unemployment = 14.2
rmse_train_unemployment

# Null Model=14.4
print(RMSE_null_train)

# Above wee see that the model with only unemployment does not perform that much 
# better than the null







#POVERTY

# Here I create a  recipe for the simple model fit with only poverty
fit_recipe_poverty <- recipe(pct_vax ~ pct_poverty , data = train_data)


# Use the workflow() package to create a
# simple workflow that fits a  simple lm 

vax_wflow_poverty <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(fit_recipe_poverty)

# Fitting the model
vax_fit_poverty <- 
  vax_wflow_poverty %>% 
  fit(data = train_data)

# Extracting Model/Recipes with Parsnip
vax_fit_poverty %>% 
  extract_fit_parsnip() %>% 
  tidy()


# Obtaining Predictions
predict(vax_fit_poverty , train_data)

vax_aug_poverty <- 
  augment(vax_fit_poverty , train_data)

vax_aug_poverty %>%
  select(pct_vax)


# Calculating Root RMSE 
rmse_train_poverty <- vax_aug_poverty %>% 
  rmse(truth = pct_vax, .pred)


jpeg(file = "results/poverty_simple_pred_vs_obs.jpeg")
poverty_simple_predicted_vs_observed <- plot(vax_aug_poverty$.pred,train_data$pct_vax, xlim =c(0,100), ylim=c(0,100), 
                                                  xlab="Predicted", ylab="Observed", 
                                                  main= "Predicted vs Observed from Poverty Model on Train Data")
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
dev.off()


jpeg(file = "results/poverty_simple_residuals.jpeg")
#residuals
poverty_simple_residuals <- plot(vax_aug_poverty$.pred-train_data$pct_vax, ylab="Residuals", main= "Residuals from Poverty Model on Train Data")
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall
dev.off()


# This RMSE for unemployment = 14.0
rmse_train_poverty

# Null Model=14.4
print(RMSE_null_train)


# Above wee see that the model with only poverty does not perform that much 
# better than the null






# LOCALITY

# Here I create a  recipe for the simple model fit with only locality
fit_recipe_locality <- recipe(pct_vax ~ locality , data = train_data) %>% 
  step_dummy(locality)


# Use the workflow() package to create a
# simple workflow that fits a  simple lm 

vax_wflow_locality <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(fit_recipe_locality)

# Fitting the model
vax_fit_locality <- 
  vax_wflow_locality %>% 
  fit(data = train_data)

# Extracting Model/Recipes with Parsnip
vax_fit_locality %>% 
  extract_fit_parsnip() %>% 
  tidy()


# Obtaining Predictions
predict(vax_fit_locality, train_data)

vax_aug_locality <- 
  augment(vax_fit_locality , train_data)

vax_aug_locality %>%
  select(pct_vax)


# Calculating Root RMSE 
rmse_train_locality <- vax_aug_locality %>% 
  rmse(truth = pct_vax, .pred)


jpeg(file = "results/locality_simple_pred_vs_obs.jpeg")
locality_simple_predicted_vs_observed <- plot(vax_aug_locality$.pred,train_data$pct_vax, xlim =c(0,100), ylim=c(0,100), 
                                                  xlab="Predicted", ylab="Observed", 
                                                  main= "Predicted vs Observed from Locality Model on Train Data")
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
dev.off()


jpeg(file = "results/locality_simple_residuals.jpeg")
#residuals
locality_simple_residuals <- plot(vax_aug_locality$.pred-train_data$pct_vax, ylab="Residuals", main= "Residuals from Locality Model on Train Data")
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall
dev.off()


# This RMSE for locality = 14.2
rmse_train_locality

# Null Model=14.4
print(RMSE_null_train)

# Above wee see that the model with only locality does not perform that much 
# better than the null







# Model Selection

# I'll go with the  LASSO. So let's give that model a final check.


final_fit <- best_lasso_wf  %>% last_fit(data_split)


#Let's look at the performance of the final fit

test_performance <- final_fit %>% collect_metrics()
print(test_performance)


test_predictions <- final_fit %>% collect_predictions()


quartz()
#predicted versus observed
plot(test_predictions$.pred,test_data$pct_vax, xlim =c(0,100), ylim=c(0,100))
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
#residuals
plot(test_predictions$.pred-test_data$pct_vax)
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall







# And now a SIMPLE MODEL

final_fit1 <- vax_wflow_simple  %>% last_fit(data_split)


#Let's look at the performance of the final fit

test_performance1 <- final_fit1 %>% collect_metrics()
print(test_performance1)


test_predictions1 <- final_fit1 %>% collect_predictions()


jpeg(file = "results/simple_pred_vs_obs_final.jpeg")
#predicted versus observed
plot(test_predictions1$.pred,test_data$pct_vax, xlim =c(0,100), ylim=c(0,100), xlab="Predicted", ylab="Observed", 
     main= "Predicted vs Observed from Simple Model on Test Data")
abline(a=0,b=1, col = 'red') #45 degree line, along which the results should fall
dev.off()



jpeg(file = "results/simple_residuals_final.jpeg")
#residuals
plot(test_predictions1$.pred-test_data$pct_vax, ylab="Residuals", main= "Residuals from Simple Model on Test Data")
abline(a=0,b=0, col = 'red') #straight line, along which the results should fall
dev.off()


