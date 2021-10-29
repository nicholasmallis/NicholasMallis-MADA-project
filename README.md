# Hello Dr. Handel. Welcome to my project github. 

The following analysis examines the relationship between education level and vaccination rate (for people over age 12) at the county level for all counties in the 50 US states. 

Part 3 of my project is located in the products tab under the manuscript folder. It includes a more polished draft of the paper with plots and tables. If you knit that to HTML, it should reproduce what I did. I also added modelingscript.R to the analysis_code folder. Here you can see my steps for running the simple and multivariate linear regression models used in the paper. 

In processing_code folder, the Processing_Exploration.Rmd goes through all the steps for reading in, merging, and wrangling the data. It also does some exploration and plotting. It also saves the plots as PNGs and tables as RDSs

In analysis_code folder, the modelingscript.R goes through all the modeling steps and saves the tables as RDSs.

In order to repeat what I did, you would first run Processing_Exploration.Rmd, then modelingscript.R, and then Project Part 3.Rmd.

