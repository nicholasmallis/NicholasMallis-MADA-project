# Hello. Welcome to my project github. 

The 'products' folder contains the final draft manuscript, supplemental materials, and materials used for citations. It also contains an 'archive' folder which houses earlier drafts and notes.

The final draft is located in the products tab under the manuscript folder. It includes a polished mansucscript of the paper with plots and tables and full analysis trying several different models (simple, multivariate, LASSO, decision tree). There is also a supplemental file that includes plots and tables referenced in the text. 

HERE ARE THE STEPS TO REPRODUCE MY ANALYSIS

1) Run the Processing_Cleaning.R located in the processing_code folder. This loads in all the datasets, merges, and cleans them.

2) Run the Descriptive_Exploration.Rmd located in the analysis_code folder. This performs descriptive analysis on the data, producing several tables and plots to be used in the manuscript.

3) Run the Modeling_Simple.Rmd located in the analysis_code folder. This runs simple and multivariate models on the data and saves different tables needed for the manuscript.

4) Run the Modeling_Advanced.R located in the analysis_code folder. This runs the full analysis trying several different models (simple, multivariate, LASSO, decision tree) and saves different plots and tables needed for the manuscript.

5) Run the Final_Draft.Rmd located in the products-> manuscript folder. Knitting this document will produce my manuscript. It includes references that were imported from Endnote. The reference file is located in the manuscript tab and named bibliography.bibtex. Supplemental_Material.Rmd contains plots and tables mentioned in the main text, but not included. 

