# agy_codes = data.frame of the USAJobs Code and Value (name of the agency)
grab_hist_joa <- function(agy_codes, is_dept = F){
  
  # loop through each agency in agy_codes
  for(x in 1:nrow(agy_codes)) {
    message("Working on: ", agy_codes$Value[x])
    if (is_dept == F) {
      # grabbing base url 
      base_url <- paste0("https://data.usajobs.gov/api/historicjoa?HiringAgencyCodes=", agy_codes$Code[x])
      
    } else {
      # grabbing base url 
      base_url <- paste0("https://data.usajobs.gov/api/historicjoa?HiringDepartmentCodes=", agy_codes$Code[x])
      
    }
    
    # empty data frame to bind to
    hist_agy_jobs <- data.frame()
    
    # connecting w/ API 
    pg1 <- httr::GET(base_url, httr::add_headers("Host"=host,
                                                 "Authorization-Key"=authkey,
                                                 "User-Agent"=useragent))
    pg1_content <-content(pg1, "text")
    pg1_content <- fromJSON(pg1_content, flatten = TRUE)
    
    # the data: 
    pg1_data <- pg1_content$data %>%
      mutate(page = 1, agy = agy_codes$Code[x])
    
    hist_agy_jobs <- bind_rows(hist_agy_jobs, pg1_data)
    
    # finding number of loops needed: - dividing total number of jobs by the page
    # size (500)
    total_loops <- ceiling(pg1_content$paging$metadata$totalCount/pg1_content$paging$metadata$pageSize)
    
    # for qa/qc: 
    # num_jobs <- usgs_hist_content$paging$metadata$totalCount - 9,975
    
    # finding token for next page: 
    next_page_url <- paste0("https://data.usajobs.gov/", pg1_content$paging$`next`)
    # this was helpful to keep track of for troubleshooting purposes: 
    next_page_df <- data.frame(page = 1, next_page_url = next_page_url, agy = agy_codes$Code[x])
    
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
        mutate(page = i + 1, agy = agy_codes$Code[x])
      
      # binding: 
      # usgs <<- bind_rows(usgs, usgs_job_postings_i) %>%
      #   unique()
      hist_agy_jobs <- bind_rows(hist_agy_jobs, agy_job_postings_i)
      
      
      # giving things a second to update: 
      Sys.sleep(0.8)
      
      # going to the next page - updating this in the global enviro cuz it should be
      # using info from the previous loop 
      next_page_url <- paste0("https://data.usajobs.gov/", agy_hist_content_i$paging$`next`)
      next_page_df_i <- data.frame(page = i + 1, next_page_url = next_page_url, 
                                   agy = agy_codes$Code[x])
      next_page_df <- bind_rows(next_page_df, next_page_df_i)
      
      # giving things a second to update: 
      Sys.sleep(0.8)
      message("next token: ", next_page_url)
    }
    
    # save over each iteration if the loop breaks: 
    save_agy_file <- paste0("./data/", agy_codes$Value[x])
    if (!(dir.exists(save_agy_file))) {
      message("Saving data for: ", agy_codes$Value[x])
      # create the directory and save the data
      dir.create(save_agy_file)
      Sys.sleep(0.3)
      saveRDS(hist_agy_jobs, paste0(save_agy_file, "/hist_jobs.RDS"))
      Sys.sleep(0.3)
    }
    
    
  }
  
  # final return
  # return(hist_agy_jobs)
}
