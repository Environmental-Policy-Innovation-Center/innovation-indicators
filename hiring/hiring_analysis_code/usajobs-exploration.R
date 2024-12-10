# Code for scoping/exploring the USAJobs API
# Author: EmmaLi Tsai
# Date: Jul 23rd 2024

# libraries 
library(httr)
library(jsonlite)
library(tidyverse)
source("./functions/grab-keyword-data.R")


# urls for current jobs posted:
url_active <- "https://data.usajobs.gov/api/search?ResultsPerPage=10000"
url_active <- "https://data.usajobs.gov/api/Search?Keyword=Data&Page=1&ResultsPerPage=10"
url_active <- "https://data.usajobs.gov/api/Search?Page=1&ResultsPerPage=10000"
url_active <- "https://data.usajobs.gov/api/Search"

# connect w/ API and transfer to readable format:
get_job_active <- httr::GET(url_active, httr::add_headers("Host"=host,
                                                          "Authorization-Key"=authkey,
                                                          "User-Agent"=useragent))
get_job_text_active <- content(get_job_active, "text")
get_job_json_active <- fromJSON(get_job_text_active, flatten = TRUE)

# looking at output
jobs <- get_job_json_active$SearchResult$SearchResultItems

# looping through pages:
pages <- as.numeric(get_job_json_active$SearchResult$UserArea$NumberOfPages)
page_seq <- seq(1, pages, by = 1)

# add page # & sys date
jobs_tidy <- jobs %>%
  janitor::clean_names() %>%
  mutate(page = page_seq[1],
         pull_date = Sys.Date())

# loop & append:
for(i in 2:length(page_seq)){
  print(page_seq[i])
  url_i <- paste0("https://data.usajobs.gov/api/Search?Keyword=Data&Page=", page_seq[i], "&ResultsPerPage=10")
  get_job_i <- httr::GET(url_i, httr::add_headers("Host"=host,
                                                  "Authorization-Key"=authkey,
                                                  "User-Agent"=useragent))

  get_job_i <-content(get_job_i, "text")
  get_job_json_i <- fromJSON(get_job_i, flatten = TRUE)

  # looking out output
  jobs_i <- get_job_json_i$SearchResult$SearchResultItems
  jobs_tidy_i <- jobs_i %>%
    janitor::clean_names() %>%
    mutate(page = page_seq[i],
           pull_date = Sys.Date())

  jobs_tidy <<- bind_rows(jobs_tidy, jobs_tidy_i)

}


# general API tests: 
url_total <- "https://data.usajobs.gov/api/Search?SearchResultCount"
total <- httr::GET(url_total, httr::add_headers("Host"=host,
                                                "Authorization-Key"=authkey,
                                                "User-Agent"=useragent))
get_job_total <-content(total, "text")
get_job_total_active <- fromJSON(get_job_total, flatten = TRUE)

get_job_json_active$SearchResult$SearchResultItems
get_job_total_active$SearchResult$SearchResultCount

# duties <- scientist_jobs$MatchedObjectDescriptor.UserArea.Details.MajorDuties

# here, this would read whatever in aws, add new data (w/ timestamp of
# latest api query), and then re-push back to aws



# testing historic API
url_hist <- 'https://data.usajobs.gov/api/historicjoa?PositionSeries=2210'

# connect w/ API and transfer to readable format:
get_job_hist <- httr::GET(url_hist, httr::add_headers("Host"=host,
                                                      "Authorization-Key"=authkey,
                                                      "User-Agent"=useragent))
get_job_hist$status_code
get_job_hist$content
# this is 403, meaning that I don't have access :-(
# check out postman - makes testing easier

get_job_text_hist <-content(get_job_hist, "text")
get_job_json_hist <- fromJSON(get_job_text_hist, flatten = TRUE)

# looking out output
scientist_jobs <- get_job_json_active$SearchResult$SearchResultItems


# get_job_active$status_code
#
rawToChar(get_job_hist$content)
#
# get_job_hist$headers
# get_job_active$headers
#
get_job_text_hist <-content(get_job_hist)
get_job_json_hist <- fromJSON(get_job_text_hist, flatten = TRUE)

