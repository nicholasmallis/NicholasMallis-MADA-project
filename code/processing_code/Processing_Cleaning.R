# Script that loads data from all sources, merges them, and cleans the merged dataset

# Packages

#the following code loads different packages and also installs them as needed
#if they aren't already installed
mypackages<-c("readr", "plyr", "dplyr", "here", "tidyverse", "gridExtra", "tidyr", "gridExtra", "robustbase", "usmap", "ggplot2" )

for (p in mypackages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}


###############################
#processing script
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



#path to different datasets
#note the use of the here() package and not absolute paths
data_location1 <- here::here("data","raw_data","countyvaccination_new.csv")
data_location2 <- here::here("data","raw_data","Education-2.csv")
data_location3 <- here::here("data","raw_data","ruralurbancodes2013-3.csv")
data_location4 <- here::here("data","raw_data","PovertyEstimates.csv")
data_location5 <- here::here("data","raw_data","Unemployment.csv")




#Loading in all five datasets for the outcome and predictors
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


#Vax Data


#But before that let's get this to only the most recent data. So we'll subset today
#vax <- vax[ which(vax$Date== "09/29/2021"), ]
#NOTE: I HAD TO DO THIS STEP OUTSIDE OF THE PROJECT BECAUSE
#THE FILE WAS TOO BIG TO INCLUDE IN THE REPO

#glimpse(vax) I commented this out for ease of reading the RMD

#subsetting what we need
myvars <- c("FIPS", "Series_Complete_12PlusPop_Pct","Metro_status", "Recip_County", "Recip_State")
vax <- vax[myvars]


#after printing this, we see that the FIPS is a character variable with #some marked as UNK. i'd like to convert this to a numeric variable and get #the unknowns to be NA's. Let's start with the NA's

vax$FIPS[vax$FIPS=='UNK'] <- NA

#now let's convert to numeric.
#and while we're at it, we'll go ahead an convert Series_Complete_12PlusPop_Pct to numeric as well

vax$FIPS <- as.numeric(vax$FIPS)
vax$Series_Complete_12PlusPop_Pct <- as.numeric(vax$Series_Complete_12PlusPop_Pct)

#looks like it was recorded as 0 for 74 counties.
table(vax$Series_Complete_12PlusPop_Pct==0)

#let's convert those who report vaccination as 0 to unknown
vax$Series_Complete_12PlusPop_Pct[vax$Series_Complete_12PlusPop_Pct==0] <- NA

#checking. looks good
head(vax)
glimpse(vax)






# Education


#now let's look at the data on the main exposure
glimpse(ed)
#commenting this out for ease of reading RMD 

#subsetting what we need
myvars2 <- c("FIPS.Code", "Percent.of.adults.with.a.bachelor.s.degree.or.higher..1990")
ed <- ed[myvars2]

#print(ed) I comment this out to make the document shorter

#since we'll be merging in FIPS, let's go ahead and change the var name "FIPS.Code"
names(ed)[1] <- "FIPS"

#checking. looks good
glimpse(ed)
head(ed)




# UNEMPLOYMENT

glimpse(unemployment)

#subsetting what we need
myvars3 <- c("FIPS_Code", "Unemployment_rate_2020", "Median_Household_Income_2019" )
unemployment <- unemployment[myvars3]

#since we'll be merging in FIPS, let's go ahead and change the var name "FIPS.Code"
names(unemployment)[1] <- "FIPS"

#checking
glimpse(unemployment)
head(unemployment)




# Poverty Estimates. 
# We want PCTPOVALL_2019 which is the
# Estimated percent of people of all ages in poverty 2019

#subsetting what we need
myvars4 <- c("FIPStxt", "PCTPOVALL_2019" )
poverty <- poverty[myvars4]

#since we'll be merging in FIPS, let's go ahead and change the var name "FIPS.Code"
names(poverty)[1] <- "FIPS"

#checking
glimpse(poverty)
head(poverty)




#The merge. 
#I really wanted to merge all in one step and
#I was having trouble. I ended up finding this code using Reduce that #worked though.

#complete <- Reduce(function(x, y) merge(x, y, all=TRUE), list(vax, ed, unemployment, poverty))

#There ended up being some possible problems with what I ran above so I do it again below in several steps

complete1 <- merge(vax, ed)

glimpse(complete1)

complete2 <- merge(unemployment, poverty)
glimpse(complete2)

complete <- merge(complete1, complete2)

#checking. looks good!
glimpse(complete)




#It looks like Median Household income is still a character. Let's fix that.
#But first we need to remove the delimiter
complete$Median_Household_Income_2019 <- as.numeric(gsub(",","",complete$Median_Household_Income_2019))

glimpse(complete)

#now let's look at some locations stuff. i'm not particulary interested in including data from US territories. #i really only want to look at states. 
#we might need to do some subsetting. let's table state and see what we get...
table(complete$Recip_State) 

#this subsets the data to only having the states we need
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

#checking. looks good!
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

#checking. looks good!
glimpse(complete)



#now let's save the proccessed data

save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(complete, file = save_data_location)


