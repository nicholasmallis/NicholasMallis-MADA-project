# Hello. Welcome to my project github. 

The following analysis examines the relationship between education level and vaccination rate (for people over age 12) at the county level for all counties in the 50 US states. 

The final draft my project is located in the products tab under the manuscript folder. It includes a more polished draft of the paper with plots and tables and full analysis trying several different models (simple, multivariate, LASSO, decision tree). We use a test/train split and Cross-Validation for the LASSO and decision tree models. If you knit that to HTML, it should reproduce what I did. 

HERE ARE THE STEPS TO REPRODUCE MY ANALYSIS

1) Run the Processing_Cleaning.R located in the processing_code folder. This loads in all the datasets, merges, and cleans them.

2) Run the Descriptive_Exploration.Rmd located in the analysis_code folder. This performs descriptive analysis on the data, producing several tables and plots to be used in the manuscript.

3) Run the Modeling_Simple.Rmd located in the analysis_code folder. This runs simple and multivariate models on the data and saves different tables needed for the manuscript.

4) Run the Modeling_Advanced.R located in the analysis_code folder. This runs the full analysis trying several different models (simple, multivariate, LASSO, decision tree) and saves different plots and tables needed for the manuscript.

5) Run the Final_Draft.Rmd located in the products-> manuscript folder. Knitting this document will produce my submission for this part of the project. It now includes references that were imported from Endnote. The reference file is located in the manuscript tab and named bibliography.bibtex. Supplemental_Material.Rmd contains plots and tables mentioned in the main text, but not included. 



