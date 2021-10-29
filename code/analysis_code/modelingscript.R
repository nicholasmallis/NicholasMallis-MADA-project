# MODELING SCRIPT

#loading packages
library(here)
library(tidyverse)
library(recipes)
library(tidymodels)
library(workflowr) 
library(parsnip)
library(rsample)


# setting path
data_loc <- here::here("data","processed_data","processeddata.rds")

# reading in data
data <- readRDS(data_loc)

glimpse(data)

# First a bivariate model with Education and Covid Cases using tidy models

# setting engine
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

# running model
lm_fit <- 
  lm_mod %>% 
  fit(pct_vax ~ pct_bachelors, data = data)
lm_fit

# saving as table
lm_fit_table <- tidy(lm_fit)

# saved it as a file
summarytable_file = here("results", "lm_fit_table.rds")
saveRDS(lm_fit_table, file = summarytable_file)

# using glance to get statistics
lm_fit_stats <- glance(lm_fit)


# saved it as a file
summarytable_file2 = here("results", "lm_fit_stats.rds")
saveRDS(lm_fit_stats, file = summarytable_file2)



summary(lm(data = data, pct_vax ~ pct_bachelors))

# Now we run another model with all predictors

lm_fit2 <- 
  lm_mod %>% 
  fit(pct_vax ~ pct_bachelors + locality + median_income + unemployment + pct_poverty, data = data)
lm_fit2

# saving as table
lm_fit_table2 <- tidy(lm_fit2)

# saved it as a file
summarytable_file3 = here("results", "lm_fit_table2.rds")
saveRDS(lm_fit_table2, file = summarytable_file3)

# using glance to get statistics
lm_fit_stats2 <- glance(lm_fit2)


# saved it as a file
summarytable_file4 = here("results", "lm_fit_stats2.rds")
saveRDS(lm_fit_stats2, file = summarytable_file4)



# other simple models with each predictor and outcome

# MEDIAN INCOME
# running model
lm_fit_median <- 
  lm_mod %>% 
  fit(pct_vax ~ median_income, data = data)
lm_fit_median

# saving as table
lm_fit_median_table <- tidy(lm_fit_median)

# saved it as a file
summarytable_file4 = here("results", "lm_fit_median.rds")
saveRDS(lm_fit_median_table, file = summarytable_file4)



# POVERTY
# running model
lm_fit_poverty <- 
  lm_mod %>% 
  fit(pct_vax ~ pct_poverty, data = data)
lm_fit_poverty

# saving as table
lm_fit_poverty_table <- tidy(lm_fit_poverty)

# saved it as a file
summarytable_file5 = here("results", "lm_fit_poverty.rds")
saveRDS(lm_fit_poverty_table, file = summarytable_file5)



# UNEMPLOYMENT
# running model
lm_fit_unemployment <- 
  lm_mod %>% 
  fit(pct_vax ~ unemployment, data = data)
lm_fit_unemployment

# saving as table
lm_fit_unemployment_table <- tidy(lm_fit_unemployment)

# saved it as a file
summarytable_file6 = here("results", "lm_fit_unemployment.rds")
saveRDS(lm_fit_unemployment_table, file = summarytable_file6)



# LOCALITY
# running model
lm_fit_locality <- 
  lm_mod %>% 
  fit(pct_vax ~ locality, data = data)
lm_fit_locality

# saving as table
lm_fit_locality_table <- tidy(lm_fit_locality)

# saved it as a file
summarytable_file7 = here("results", "lm_fit_locality.rds")
saveRDS(lm_fit_locality_table, file = summarytable_file7)



