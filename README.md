![Primary Logo - 950x250 - Transparent Background](https://github.com/user-attachments/assets/b89e0ba6-ac56-4ec4-a3c8-372ae4a44101)

# Innovation Indicators
This repository contains the code necessary to recreate the results presented through the Innovation Indicators website (insert link when available). It pulls, cleans, analyzes, and visualizes the data from public sources.
EPIC is developing and maintaining these metrics iteratively over time. Please review our methodology and data dictionary for context and interpretations (links). If you have any feedback or would like to propose an additional indicator, 
please reach out to Cole von Glahn at cvonglahn@policyinnovation.org.

## Navigation and Use
The repository is organized by indicator according to the following structure. See individual folder ReadMes for further file specifics. Since this project utilizes EPIC's private amazon web service s3 buckets throughout the data pipeline, 
it is recommended that you redirect links in the code to your own private buckets. However, slight modification of this code (i.e., downloading the data to your local machine instead of uploading/downloading it from s3 buckets) will allow 
you to run this collection of files without amazon web services.

# challenges 
Code for investigating the use of Challenges/Prize Competitions by federal agencies with a particular focus on technology-oriented challenges. 
* Section 1 of `challenge_innovation_indicators_figures.R` in `innovation_indicator_website_code` will pull new and archived Challenges data.
* Code in `challenges_analysis.py` will clean, analyze, and output data for use.
* Section 2 of `challenge_innovation_indicators_figures.R` produces visualizations from the cleaned data.

# hiring
Code for investigating federal hiring actions for innovative talent at environmental agencies/bureaus.
* Use the files in `hiring_analysis_code` to pull data from USAJobs and perform the qualitative analysis using ChatGPT.
* Run that data through `usaj_analysis.py` to pull out the core statistics used on the website.
* Run `hiring_innovation_indicators_figures` in `innovation_indicator_website_code` to produce the visualizations. 

# open-source
Code for investigating the use of open source software by environmental agencies.
* `github-scraper.Rmd` contains all of the code needed to pull, analyze, and visualize the website results.

# staffing
Code for investigating technology talent staffing trends at environmental agencies.
* Use the files in `functions` to download and assemble the OPM data cubes.
* Follow the steps outlined in `tech_capacity_analysis.Rmd` to clean and analyze the data.
* Use `staffing_innovation_indicator_figures.R` in `innovation_indicator_website_code` to recreate the visualizations.

## Attributions and license:
Developed in partnership with the Patrick J McGovern Foundation (link) by Environmental Policy Innovation Center (link).
EPIC makes no assurances to the accuracy of the data. All underlying code, methods, and data are available under a Creative Commons License. 

