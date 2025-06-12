### Libraires & Creds###########################################################
library(httr)
library(jsonlite)
library(tidyverse)

# credentials: 


### Data exploration ###########################################################
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
scientist_jobs <- get_job_json_hist$data

# can I figure out hiring agency codes and hiring department codes?
codes_df <- scientist_jobs %>% 
  select(hiringAgencyCode:hiringDepartmentName) %>%
  unique()

### Agy Codes ##################################################################
# let's get agency codes: 
url_codes <- 'https://data.usajobs.gov/api/codelist/agencysubelements'
# connect w/ API and transfer to readable format:
get_codes <- httr::GET(url_codes, httr::add_headers("Host"=host,
                                                      "Authorization-Key"=authkey,
                                                      "User-Agent"=useragent))
codes <-content(get_codes, "text")
codes_text <- fromJSON(codes, flatten = TRUE)
codes_df <- as.data.frame(codes_text$CodeList$ValidValue)

# only grabbing the ones we're interested in: (note there are some "Recovery Jobs" - what does this mean?)
enviro_codes <- codes_df %>%
  # USFWS, USFS, USACE under two codes: ARCD and ARCE, 
  filter(Code %in% c("IN15", "AG11", 
                     # "ARCD", <- removing this one for now because it created "trailing garbage"
                     "ARCE", 
                     # NOAA, USGS, BLM (they also have one for a hiring fair? INJ1), BOR 
                     "CM54", "IN08", "IN05", "IN07", 
                     # NPS, NRCS, EPA (note they also have a hiring fair "EPJF )
                     "IN10", "AG16", "EP00"))
# comp agy codes: 
comp_codes <- codes_df %>%
  # NASA, NIH, DOT, DOL, homeland security, department of state
  filter(Code %in% c("NN", "HE38", "TD", "DL", "HS", "ST"))


### Grabbing Enviro Data #######################################################
enviro_jobs_hist <- data.frame() 
for(x in 5:nrow(enviro_codes)) {
  message("Working on: ", enviro_codes$Value[x])
  
  # grabbing base url 
  base_url <- paste0("https://data.usajobs.gov/api/historicjoa?HiringAgencyCodes=", enviro_codes$Code[x])
  
  # connecting w/ API 
  pg1 <- httr::GET(base_url, httr::add_headers("Host"=host,
                                                      "Authorization-Key"=authkey,
                                                      "User-Agent"=useragent))
  pg1_content <-content(pg1, "text")
  pg1_content <- fromJSON(pg1_content, flatten = TRUE)
  
  # the data: 
  pg1_data <- pg1_content$data %>%
    mutate(page = 1, agy = enviro_codes$Code[x])
  
  enviro_jobs_hist <<- bind_rows(enviro_jobs_hist, pg1_data)
  
  # finding number of loops needed: - dividing total number of jobs by the page
  # size (500)
  total_loops <- ceiling(pg1_content$paging$metadata$totalCount/pg1_content$paging$metadata$pageSize)
  
  # for qa/qc: 
  # num_jobs <- usgs_hist_content$paging$metadata$totalCount - 9,975
  
  # finding token for next page: 
  next_page_url <- paste0("https://data.usajobs.gov/", pg1_content$paging$`next`)
  # this was helpful to keep track of for troubleshooting purposes: 
  next_page_df <<- data.frame(page = 1, next_page_url = next_page_url, agy = enviro_codes$Code[x])
  
  # looooopin'
  for(i in 1:total_loops){
    message("on page ", i, " of ", total_loops)
    
    # checking for if the next page is blank - then just move to the next agency
    if (next_page_url == "https://data.usajobs.gov/"){
      break
    }
    
    # grabbing the next page based on what is saved globally
    Sys.sleep(0.3)
    agy_hist_i <- httr::GET(next_page_url, 
                             httr::add_headers("Host"=host,
                                               "Authorization-Key"=authkey,
                                               "User-Agent"=useragent))
    agy_hist_content_i <- content(agy_hist_i, "text")
    agy_hist_content_i <- fromJSON(agy_hist_content_i, flatten = TRUE)
    agy_job_postings_i <- agy_hist_content_i$data %>%
      # we already scraped 1, so this should be i + 1
      mutate(page = i + 1, agy = enviro_codes$Code[x])
    
    # binding: 
    # usgs <<- bind_rows(usgs, usgs_job_postings_i) %>%
    #   unique()
    enviro_jobs_hist <<- bind_rows(enviro_jobs_hist, agy_job_postings_i)
    
    
    # giving things a second to update: 
    Sys.sleep(0.8)
    
    # going to the next page - updating this in the global enviro cuz it should be
    # using info from the previous loop 
    next_page_url <<- paste0("https://data.usajobs.gov/", agy_hist_content_i$paging$`next`)
    next_page_df_i <- data.frame(page = i + 1, next_page_url = next_page_url, 
                                 agy = enviro_codes$Code[x])
    next_page_df <<- bind_rows(next_page_df, next_page_df_i)
    
    # giving things a second to update: 
    Sys.sleep(0.8)
    message("next token: ", next_page_url)
  }
  
}


###### OG CODE: ###########
#### okay - we should grab all of the jobs posted by agencies 
# starting with USGS
usgs_hist <- 'https://data.usajobs.gov/api/historicjoa?HiringAgencyCodes=IN08'
usgs_hist <- httr::GET(usgs_hist, httr::add_headers("Host"=host,
                                                    "Authorization-Key"=authkey,
                                                    "User-Agent"=useragent))
usgs_hist_content <-content(usgs_hist, "text")
usgs_hist_content <- fromJSON(usgs_hist_content, flatten = TRUE)

# the data: 
usgs <- usgs_hist_content$data %>%
  mutate(page = 1)

# finding number of loops needed: - dividing total number of jobs by the page
# size (500)
total_loops <- ceiling(usgs_hist_content$paging$metadata$totalCount/usgs_hist_content$paging$metadata$pageSize)

# for qa/qc: 
# num_jobs <- usgs_hist_content$paging$metadata$totalCount - 9,975

# finding token for next page: 
next_page_url <- paste0("https://data.usajobs.gov/", usgs_hist_content$paging$`next`)
# this was helpful to keep track of for troubleshooting purposes: 
next_page_df <<- data.frame(page = 1, next_page_url = next_page_url)

for(i in 1:total_loops){
  message("on page ", i, " of ", total_loops)
  # grabbing the next page based on what is saved globally
  usgs_hist_i <- httr::GET(next_page_url, 
                           httr::add_headers("Host"=host,
                                             "Authorization-Key"=authkey,
                                             "User-Agent"=useragent))
  usgs_hist_content_i <- content(usgs_hist_i, "text")
  usgs_hist_content_i <- fromJSON(usgs_hist_content_i, flatten = TRUE)
  usgs_job_postings_i <- usgs_hist_content_i$data %>%
    # we already scraped 1, so this should be i + 1
    mutate(page = i + 1)
  
  # binding: 
  usgs <<- bind_rows(usgs, usgs_job_postings_i) %>%
    unique()
  
  # giving things a second to update: 
  Sys.sleep(0.8)
  
  # going to the next page - updating this in the global enviro cuz it should be
  # using info from the previous loop 
  next_page_url <<- paste0("https://data.usajobs.gov/", usgs_hist_content_i$paging$`next`)
  next_page_df_i <- data.frame(page = i + 1, next_page_url = next_page_url)
  next_page_df <<- bind_rows(next_page_df, next_page_df_i)
  
  # giving things a second to update: 
  Sys.sleep(0.8)
  message("next token: ", next_page_url)
  

}

# for testing: 
# next_page_url <- next_page_df$next_page_url[18]
# NOTE - that the totalcount in the metadata field decreases as you go through 
# each page 
