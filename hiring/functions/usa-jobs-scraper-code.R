# libraries 
library(httr)
library(jsonlite)
library(tidyverse)
library(aws.s3)
source("./functions/grab-keyword-data.R")

# NOTE - this has been migrated to an AWS worker! 
# for logs: 
print("I'm running!")

# add your keys
host <- ""
useragent <- ""
authkey <- ""

# checking each keyword: 
data_jobs <- grab_keyword_data(keywords = "Data", results_per_page = 100)
tech_jobs <- grab_keyword_data(keywords = "Technology", results_per_page = 100)
software_jobs <- grab_keyword_data(keywords = "Software", results_per_page = 100)
programming_jobs <- grab_keyword_data(keywords = "Programming", results_per_page = 100)
prod_manager_jobs <- grab_keyword_data(keywords = "Product", results_per_page = 100)
gis_jobs <- grab_keyword_data(keywords = "Geospatial", results_per_page = 10)
stewards_jobs <- grab_keyword_data(keywords = "Steward", results_per_page = 100)
innov_jobs <- grab_keyword_data(keywords = "Innovation", results_per_page = 100)
# added below keywords for FAS folks - starting Sept 26th 2024
permit_jobs <- grab_keyword_data(keywords = "Permit", results_per_page = 10)
nepa_jobs <- grab_keyword_data(keywords = "NEPA", results_per_page = 10)
# added below keywords - starting Oct 1st 2024
enviro_review_jobs <- grab_keyword_data(keywords = "Environmental+Review", results_per_page = 10)


# binding! 
all_searched_jobs <- bind_rows(data_jobs, tech_jobs, software_jobs, 
                               programming_jobs, prod_manager_jobs, 
                               gis_jobs, stewards_jobs, innov_jobs, 
                               permit_jobs, nepa_jobs, enviro_review_jobs) 


# seeing which jobs matched to multiple keywords:
# Note: there may be two identical entries with the same object_id, and this will  
# paste the same keyword together in a single row in our data frame. I think 
# it's reasonable to assume they're duplicates. 
# Object_ids CAN repeat, but from looking at instances where this happens 
# (rare, ~5 on March 25th, 2025), the data changed for the job. In our dataframe, 
# they become two different rows. 
id_keywords <- all_searched_jobs %>%
  group_by(matched_object_id) %>%
  summarize(all_keywords = paste(keyword, collapse = ", "))

# merging to capture all keywords for a specific job id: 
all_searched_jobs_keys <- merge(all_searched_jobs, id_keywords, by = "matched_object_id")

# removing page, keyword, a problem inconsistently entered, and selecting unique entries
all_unique_jobs_keys <- all_searched_jobs_keys %>%
  select(-c(page, keyword, matched_object_descriptor_user_area_details_key_requirements)) %>%
  unique()

# comparing with what has already been saved: 
# this only runs the FIRST time I get this up and going: 
# saveRDS(all_unique_jobs_keys, file = paste0("/Users/emmalitsai/policy-tech-sprints/enviro-hiring-trends/data/usajobs_data-", Sys.time(), ".rds"))

# grabbing og data pull: 
usajobs_old <- aws.s3::s3read_using(readRDS,
                                    object = "s3://tech-team-data/enviro-hiring-trends/worker-data/usajobs_data_AWSWORKER.rds")


# which jobs are new? 
usajobs_data_actually_new <- all_unique_jobs_keys %>% 
  filter(!(matched_object_id %in% usajobs_old$matched_object_id)) # 587 opened 

# bind the new jobs to the bottom of the old file
updated_usajobs_data <- bind_rows(usajobs_old, usajobs_data_actually_new) # 9744 in total

# save this locally
file_name <- paste0("./data/usajobs_data-", Sys.Date(), ".rds")
saveRDS(updated_usajobs_data, file = file_name)

# overwrite existing file in aws: 
# put_object(
#   file = file_name,
#   object = "/enviro-hiring-trends/data/usajobs_data.rds",
#   bucket = "tech-team-data",
# )


# old testing code to prove that this works: 
# file_name <- list.files("/Users/emmalitsai/policy-tech-sprints/enviro-hiring-trends/data/")
# old_file_name <- paste0("/Users/emmalitsai/policy-tech-sprints/enviro-hiring-trends/data/", file_name)
# usajobs_old <- readRDS(old_file_name)

# usajobs_data_old <- readRDS("./data/usajobs_data-2024-09-10 17:32:03.992299.rds") # 9157
# usajobs_data_new <- readRDS("./data/usajobs_data-2024-09-11 09:07:06.700917.rds") # 9123

# usajobs_data_actually_closed <- usajobs_data_old %>% 
#   filter(!(matched_object_id %in% usajobs_data_new$matched_object_id)) # 621 closed, but we want to keep these 
# difference of 34
# unique(usajobs_data_actually_closed$matched_object_descriptor_application_close_date)
# some of these are later, but honestly makes sense since some of them cap at certain # of applications
