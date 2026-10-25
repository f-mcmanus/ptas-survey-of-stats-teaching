# ptas-survey-of-stats-teaching
Code and pseudonymised data for the PTAS-funded project: **Survey of undergraduate statistics teaching in the life sciences**.

`survey/` - Folder containing three versions of the survey as presented on the Qualtrics survey platform. Most participants completed our main survey (survey_of_undergraduate teaching.pdf).

`data/clean_data` - The survey results as used in the analysis and the data collection tracker used to generate a list of eligible universities for response rate calculations

`R/functions` - Folder containing R scripts for functions used to pseudonymise, clean, analyse, and plot data. The model and plot functions are grouped in their own scripts (q8_plot_functions.R and q8_model_functions.R).

`R/cleaning` Folder with code for preparing the data for analysis

`R/analysis` - Folder with code for running models and generating figures. The scripts `q8_models.R` and `q8_plots.R` generate lists which are loaded in the Bernoulli models for question 8 of the survey ( `q8_ ... .Rmd`).

Includes:
`q8_models.R` - runs the binomial models related to question 8 of the survey. The script stores model output and calculations related to the models (e.g. model summaries, conditional effects, credible intervals) as lists. If the models are cached in the "models" or "store" folder, then the script will load these, rather than re-running them. If you would like to make changes to the models, delete the cached models or change the names they are stored under.

`q8_plots.R` - either loads the lists of model output and calculations (e.g. model summaries, conditional effects, credible intervals) or sources `q8_models.R`. It outputs figures, which are stored in the 'figures' folder.

`store/` - Folder for storing models and model output (in the form of lists) generated in Rmd scripts

`figures/` - The code saves plots here

`metadata/` - Folder containing metadata, such as a data dictionary for the survey results data set

## Generating Models and Figures for the Binomial Data (Question 8 of the survey)
`q8_models.R` runs all binomial models (i.e. relating to question 8 of the survey) and stores them in rds lists. If there are already models cached in the `models` folder, then these will be used instead of generating new models (even if you have made changes to the model - to put these changes in effect, delete the cached models or rename the new models).

The RMarkdown files `q8_ ... .Rmd` each either load the cached lists of models (if they exist) or source `q8_models.R`. These RMarkdown files are used to present the model output (summaries and plots) which was generated in q8_models.R and stored as lists. There is one Markdown file for each sub-question within question 8 of the survey.

'q8_plots.R` generates all the figures associated with question 8. It eithers load the cached model lists (if they exist) or sources `q8_models.R`.

