# Function: download_cubes()
# Author: EmmaLi Tsai 
# Date: Jan 25 2024
# 
# Function to automatically pull all of the employment cubes from opm's website
# and add them to a local folder: 
#
# inputs: 
#   -  url = link to opm's website. Default is set to the FedScope page, which 
#            contains all employment cubes 
#   - dest_folder = destination folder where you would like to store downloaded
#            employment cubes 
#   - up_to_year = the year you would like to go back to. 
#   - month = the month you're interested in. Ones includes on opm's website are 
#             often June, March, December, and September. 
# 
# outputs: a folder containing all downloaded employment cubes. 
# NOTE - this function now queries the entire opm index website, since filtering 
# the data for fedscope beforehand missed cubes for years past 2022 since they
# are missing an html tag
download_cubes <- function(url = "https://www.opm.gov/data/datasets/Index.aspx", 
                                 dest_folder = "./data/test/", 
                                 up_to_year = 2013, 
                                 month = "March"){
  
  # read the fedscope table from opm: 
  opm <- url %>% 
    read_html() %>%
    html_nodes("table") %>%
    html_table(fill = T)
  # as.data.frame()
  # we only want the second list entry 
  opm <- opm[[2]] %>%
    as.data.frame()
  
  # just grab the lines with w/ zipped files:
  downloads <- opm[nchar(opm$Downloads) > 0, ]
  # just grab the employment cubes: 
  downloads <- downloads %>% filter(grepl("Cube|Call", downloads$`Data Set`))
  
  # grab the fedscope zipped folder links: 
  page <- xml2::read_html(url)
  links <- page %>%
    html_nodes("a") %>%     
    html_attr("href") %>%     
    str_subset("\\.zip") 
  links <- paste0("https://www.opm.gov", links)
  
  # connecting zipper folder links with the correct employment cube: 
  fedscope_zipped <- cbind(downloads, links)  %>%
    janitor::clean_names()
  emp_cubes <- fedscope_zipped[grepl("Employment Cube", fedscope_zipped$data_set),]
  
  # filtering before year & month specified: 
  emp_cubes <- emp_cubes[grepl(rebus::number_range(up_to_year, year(Sys.Date())), 
                               emp_cubes$data_set),]
  emp_cubes <- emp_cubes[grepl(month, emp_cubes$data_set),]
  
  # looping through and adding employment cubes to a folder: 
  for(i in 1:nrow(emp_cubes)){
    message("downloading", emp_cubes[i,]$data_set)
    link_i <- emp_cubes[i,]$links
    file_loc <- paste0(dest_folder, emp_cubes[i,]$data_set)
    # trimming white space
    file_loc <- gsub(" ", "", file_loc)
    download.file(link_i, 
                  destfile = paste0(file_loc, ".zip"))
    unzip(zipfile = paste0(file_loc, ".zip"), exdir = file_loc) 
    file.remove(paste0(file_loc, ".zip"))
  }
}


