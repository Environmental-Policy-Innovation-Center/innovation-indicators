# install.packages("cronR")
library(cronR)
usa_jobs_scraper_file <- "/Users/emmalitsai/policy-tech-sprints/enviro-hiring-trends/functions/usa-jobs-scraper-code.R"
cmd <- cron_rscript(usa_jobs_scraper_file)

cron_add(command = cmd, frequency = 'daily', at = "9:05AM", 
         id = 'usajobs_scraper_daily', 
         description = 'Task_Scraping_USAJobs_Data')

cron_njobs()
cron_ls()
# cron_rm(id = "usajobs_scraper")
