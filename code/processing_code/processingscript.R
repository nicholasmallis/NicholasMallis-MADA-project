###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#the following code loads different packages and also installs them as needed
#if they aren't already installed
mypackages<-c("readr", "plyr", "dplyr", "here", "tidyverse", "gridExtra", "tidyr", "gridExtra", "robustbase", "usmap", "ggplot2" )

for (p in mypackages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

#load needed packages. make sure they are installed.
#library(readr) #for reading in data
#library(dplyr) #for data processing
#library(here) #to set paths
#library(tidyverse)
#library(dplyr)
#library(plyr)

#path to different dtasets
#note the use of the here() package and not absolute paths
data_location1 <- here::here("data","raw_data","countyvaccination.csv")
data_location2 <- here::here("data","raw_data","Education-2.csv")
data_location3 <- here::here("data","raw_data","ruralurbancodes2013-3.csv")
data_location4 <- here::here("data","raw_data","PovertyEstimates.csv")
data_location5 <- here::here("data","raw_data","Unemployment.csv")





#Loading in all four datasets for the outcome and predictors
vax <- read.csv(data_location1)
ed <- read.csv(data_location2)
locality <- read.csv(data_location3)
poverty <- read.csv(data_location4)
unemployment <- read.csv(data_location5)




#take a look at the datasets
dplyr::glimpse(vax)

dplyr::glimpse(ed)

dplyr::glimpse(locality)

dplyr::glimpse(poverty)

#looks pretty good on all four.
#let's print parts of them to see what they look like

head(vax)

tail(ed)

head(locality)

tail(poverty)

#before we merge, i'd like to subset each dataset to only what we need.
#let's start with the vaccination data. The main outcome will be percent 
#of eligible people (age 12+) in a county that have been fully vaccinated.
#we'll also pull in the FIPS # that will be used for merging and 'metro status'


#But before that let's get this to only the most recent data. So we'll subset today
#vax <- vax[ which(vax$Date== "09/29/2021"), ]
#NOTE: I HAD TO DO THIS STEP OUTSIDE OF THE PROJECT BECAUSE
#THE FILE WAS TOO BIG TO INCLUDE IN THE REPO

glimpse(vax)
myvars <- c("FIPS", "Series_Complete_12PlusPop_Pct","Metro_status", "Recip_County", "Recip_State")
vax <- vax[myvars]
print(vax)


#after printing this, we see that the FIPS is a character variable with some
#marked as UNK. i'd like to convert this to a numeric variable and get the unknowns 
#to be NA's. Let's start with the NA's

vax$FIPS[vax$FIPS=='UNK'] <- NA

#now let's convert to numeric.
#and while we're at it, we'll go ahead an convert Series_Complete_12PlusPop_Pct to numeric as well

vax$FIPS <- as.numeric(vax$FIPS)
vax$Series_Complete_12PlusPop_Pct <- as.numeric(vax$Series_Complete_12PlusPop_Pct)

#checking. looks good
head(vax)


#now let's look at the data on the main exposure
glimpse(ed)
myvars2 <- c("FIPS.Code", "Percent.of.adults.with.a.bachelor.s.degree.or.higher..1990")
ed <- ed[myvars2]
print(ed)

#looks like FIPS.Code is already numeric, but we need need perecent adults with bachelor to be numeric
#let's go ahead and convert that.
ed$FIPS.Code <- as.numeric(ed$FIPS.Code)

#since we'll be merging in FIPS, let's go ahead and change the var name "FIPS.Code"
names(ed)[1] <- "FIPS"

#checking
glimpse(ed)


#next dataset....median income which comes from the unemployment dataset
#we only want vars from the most recent estimates so we'll subset the FIPS_Code, 
#State, Area_name and the following...
#Unemployment_rate_2020
#Median_Household_Income_2019

glimpse(unemployment)
myvars3 <- c("FIPS_Code", "Unemployment_rate_2020", "Median_Household_Income_2019" )
unemployment <- unemployment[myvars3]

#since we'll be merging in FIPS, let's go ahead and change the var name "FIPS.Code"
names(unemployment)[1] <- "FIPS"

#checking
glimpse(unemployment)


#next dataset...Poverty Estimates. We want PCTPOVALL_2019 which is the
#Estimated percent of people of all ages in poverty 2019

glimpse(poverty)
myvars4 <- c("FIPStxt", "PCTPOVALL_2019" )
poverty <- poverty[myvars4]

#since we'll be merging in FIPS, let's go ahead and change the var name "FIPS.Code"
names(poverty)[1] <- "FIPS"


#checking
glimpse(poverty)


#the merge
#complete <- Reduce(function(x, y) merge(x, y, all=TRUE), list(vax, ed, unemployment, poverty))


#There ended up being some possible problems with what I ran above so I do it again below in several steps
complete1 <- merge(vax, ed)

complete2 <- merge(unemployment, poverty)

complete <- merge(complete1, complete2)

#checking
glimpse(complete)


#looks like Median Household income is still a character. Let's fix that.
#but first we need to remove the delimiter
complete$Median_Household_Income_2019 <- as.numeric(gsub(",","",complete$Median_Household_Income_2019))

glimpse(complete)

#now let's look at some locations stuff. i'm not particulary interested in including
#data from US territories. i really only want to look at states. 
#we might need to do some subsetting. let's table state and see what we get...
table(complete$Recip_State) 

complete <- complete[which(complete$Recip_State != 'AS'
                           &
                             complete$Recip_State != 'FM'
                           &
                             complete$Recip_State != 'GY'
                           &
                             complete$Recip_State != 'GU'
                           &
                             complete$Recip_State != 'MP'
                           &
                             complete$Recip_State != 'PW'
                           &
                             complete$Recip_State != 'PR'
                           &
                             complete$Recip_State != 'MH'
                           &
                             complete$Recip_State != 'VI'
), ]

table(complete$Recip_State)



#before we continue, i really need to change some of these variable names. they are just too long!
names(complete)[2] <- "pct_vax"
names(complete)[3] <- "locality"
names(complete)[4] <- "county"
names(complete)[5] <- "state"
names(complete)[6] <- "pct_bachelors"
names(complete)[7] <- "unemployment"
names(complete)[8] <- "median_income"
names(complete)[9] <- "pct_poverty"


glimpse(complete)

#now let's do a five number summary for each variable
fivenum(complete$pct_vax)
fivenum(complete$pct_bachelors)
fivenum(complete$unemployment)
fivenum(complete$median_income)
fivenum(complete$pct_poverty)


#and some plotting of distribution for each variable

#here we do % vaccinated
a <- complete %>% ggplot(aes(x=pct_vax)) + geom_histogram(binwidth = 5)

#here we do % bachelors
b <- complete %>% ggplot(aes(x=pct_bachelors)) + geom_histogram(binwidth = 5) + xlim(0, 100)

#here we do % unemployment
c <- complete %>% ggplot(aes(x=unemployment)) + geom_histogram(binwidth = 5) + xlim(0, 100)

#here we do % poverty
d <- complete %>% ggplot(aes(x=pct_poverty)) + geom_histogram(binwidth = 5) + xlim(0, 100)

#here we do % poverty
e <- complete %>% ggplot(aes(x=median_income)) + geom_histogram(binwidth = 10000) 

#and finally we look at locality
f <- complete %>% ggplot(aes(x=locality)) + geom_bar()




#now let's do a simple scatter plot of our redictor and outcome to explore the relationship
p1 <- complete %>% ggplot(aes(x=pct_bachelors, y=pct_vax)) + geom_point() + geom_smooth(method='lm')

print(p1)

#at first glance, there appears to be a linear relationship between our predictor and outcome,
#but it looks like there a lot of 0s for our outcome (% vaccinated over 12). Let's take a closer look.

count <- length(which(complete$pct_vax == 0))      

count

#it looks like we have 317 of the counties with 0% vaccination. i'm wondering if this could be an error or 
#if some counties haven't reported.

#now some more exploration with other variables
#let's look at income...

p2 <- complete %>% ggplot(aes(x=median_income, y=pct_vax)) + geom_point() + geom_smooth(method='lm')

p3 <- complete %>% ggplot(aes(x=pct_poverty, y=pct_vax)) + geom_point() + geom_smooth(method='lm')

p4 <- complete %>% ggplot(aes(x=unemployment, y=pct_vax)) + geom_point() + geom_smooth(method='lm')

p5 <- complete %>% ggplot(aes(x=locality, y=pct_vax)) + geom_boxplot()



