# Hello. Welcome to my project github. 


The following analysis aims to examine the relationship between education level and vaccination rate (for people over age 12) at the county level for all counties in the 50 US states and explore other county characteristics that might predict vaccination rates. For the outcome, we use public data from CDC on vaccination rates by county. In order to obtain county characteristics, we pull data from several different sources that are available on the USDA’s Economic Research website. 

The final draft is located in the products tab under the manuscript folder. It includes a polished mansucscript of the paper with plots and tables and full analysis trying several different models (simple, multivariate, LASSO, decision tree). There is also a supplemental file that includes plots and tables referenced in the text. 

HERE ARE THE STEPS TO REPRODUCE MY ANALYSIS

1) Run the Processing_Cleaning.R located in the processing_code folder. This loads in all the datasets, merges, and cleans them.

2) Run the Descriptive_Exploration.Rmd located in the analysis_code folder. This performs descriptive analysis on the data, producing several tables and plots to be used in the manuscript.

3) Run the Modeling_Simple.Rmd located in the analysis_code folder. This runs simple and multivariate models on the data and saves different tables needed for the manuscript.

4) Run the Modeling_Advanced.R located in the analysis_code folder. This runs the full analysis trying several different models (simple, multivariate, LASSO, decision tree) and saves different plots and tables needed for the manuscript.

5) Run the Final_Draft.Rmd located in the products-> manuscript folder. Knitting this document will produce my manuscript. It includes references that were imported from Endnote. The reference file is located in the manuscript tab and named bibliography.bibtex. Supplemental_Material.Rmd contains plots and tables mentioned in the main text, but not included. 



PROJECT ORGANIZATION

All raw and processed data is located in the 'data' folder 

The 'code' folder contains all code scripts/RMD files for data processing/cleaning, exploration, and analysis.

The 'results' folder contains all results from the analysis (figures, tables, computed values)

The 'products' folder contains the final draft manuscript, supplemental materials, and materials used for citations. It also contains an 'archive' folder which houses earlier drafts and notes.

The 'reviews' folder contains peer reviews from two classmates.
