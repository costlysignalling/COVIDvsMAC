# COVID19 vs MAC
Code used for the analysis of the relationship between morality (measured with Morality As Cooperation relevance scale) and intentions to minimize infection during COVID-19 pandemic.

**Results in a nutshell:**\
Intentions to cooperate (i.e. to minimize infection) were predicted by two underlying factors - precaution and prosociality. US and Indian men differed in MAC scores. Heroism message consistently elevated prosocial tendencies, bet lowered the precaution. MAC message congruent with held moral intuitions elevated precaution in the US sample only. 

**Data:**\
Raw data exported from Qualtrics are stored in QexportUSA.csv and QexportIndia.csv files.

**You can track the preprint at:**\
TBA

**How to run the code:**\
You should initiate the session from the corona.Rproj and start perhaps from *01_data_prepare.R* (if you are interested how the raw data are handeled) or directly from the evaluation of structural Bayesian model: *02_analysis_single_estimate.R* or similar that directly sources the cleaned data. Than build your way up thhrough the scripts that summarize and visualize the results. We used rethinking packege with STAN infrastructure, so make sure all neccesary elements are properly installed on your computer. See https://github.com/rmcelreath/rethinking for details about the package and installation. Custom-build functions for data visualizations are stored in a separate scripts (03_...), details (colours, positions) on posetrior visualizations are stored in params....txt files.
Script starting with 00 runs multiple scripst sequentially and conducts a model comparison between the single sample and varying intercepts model.
