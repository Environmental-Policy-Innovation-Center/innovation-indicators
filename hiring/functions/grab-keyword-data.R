grab_keyword_data <- function(keywords = "Data", results_per_page = 100) {
  
  url_keyword <- paste0("https://data.usajobs.gov/api/Search?Keyword=", keywords, "&Page=1&ResultsPerPage=", results_per_page)
  
  # connect w/ API and transfer to readable format: 
  get_job_active <- httr::GET(url_keyword, httr::add_headers("Host"=host, 
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
           pull_date = Sys.Date(), 
           keyword = keywords)
  
  # loop & append: 
  for(i in 2:length(page_seq)){
    print(paste0("on page: ", page_seq[i], " out of: ", page_seq[length(page_seq)], " for keyword: ", keywords))
    url_i <- paste0("https://data.usajobs.gov/api/Search?Keyword=Data&Page=", page_seq[i], "&ResultsPerPage=", results_per_page)
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
             pull_date = Sys.Date(), 
             keyword = keywords) 
    
    jobs_tidy <- bind_rows(jobs_tidy, jobs_tidy_i)
    
  }
  
  return(jobs_tidy)
}