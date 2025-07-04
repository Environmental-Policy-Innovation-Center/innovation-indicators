![Primary Logo - 950x250 - Transparent Background](https://github.com/user-attachments/assets/b89e0ba6-ac56-4ec4-a3c8-372ae4a44101)

# Innovation Indicators
This repository contains the code necessary to recreate the results presented through the Innovation Indicators [website](https://www.policyinnovation.org/innovation-indicators). It pulls, cleans, analyzes, and visualizes the data from public sources.
EPIC is developing and maintaining these metrics iteratively over time. Please review our [methodology](https://docs.google.com/document/d/1-vAbGiwHhInAUFn--gdIFjGaFHfY9d6zGogqB0_r_Nw/edit?tab=t.0) and [data dictionary](https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=0#gid=0) for context and interpretations. 

## Navigation and Use
The repository is organized by indicator according to the following structure. See individual folder ReadMes for further file specifics. Since this project utilizes EPIC's private amazon web service s3 buckets throughout the data pipeline, 
it is recommended that you redirect links in the code to your own private buckets. However, slight modification of this code (i.e., downloading the data to your local machine instead of uploading/downloading it from s3 buckets) will allow 
you to run this collection of files without amazon web services.

### challenges 
Code for investigating the use of Challenges/Prize Competitions by federal agencies with a particular focus on technology-oriented challenges. 
* Section 1 of `challenge_innovation_indicators_figures.R` in `innovation_indicator_website_code` will help pull new and archived Challenges data. Note some of these steps require manually downloading data. 
* Code in `challenges_analysis.py` will clean, analyze, and output data for use.
* Section 2 of `challenge_innovation_indicators_figures.R` produces visualizations from the cleaned data.

### hiring
Code for investigating federal hiring actions for innovative talent at environmental agencies/bureaus.
* Use the files in `innovation_indicator_website_code` to pull the latest historic USAJobs API data for our agencies of interest and produce the visualizations on our website. 
* The files in `hiring_analysis_code` show previous analyses using the USAJobs search API and performs qualitative analysis using ChatGPT.
* Run the data produced above through `usaj_analysis.py` to pull out the core statistics used on the website.

### open-source
Code for investigating the use of open source software by environmental agencies.
* `github-scraper.Rmd` contains all of the code needed to pull, analyze, and visualize the website results.

### staffing
Code for investigating technology talent staffing trends at environmental agencies.
* Follow the steps outlined in `tech_capacity_analysis.Rmd` to download the OPM staffing cubes, clean and analyze the data.
* Use `staffing_innovation_indicator_figures.R` in `innovation_indicator_website_code` to recreate the visualizations.

## Attributions and license:
Developed in partnership with the [Patrick J McGovern Foundation](https://www.mcgovern.org/) by the [Environmental Policy Innovation Center](https://www.policyinnovation.org/).
EPIC makes no assurances to the accuracy of the data. If you have any feedback or would like to propose an additional indicator, 
please reach out to Cole von Glahn at cole@policyinnovation.org. All underlying code, methods, and data are available under a Creative Commons License. 

