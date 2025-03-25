assemble_cube <- function(parent_folder = "./data/", 
                          folder_list){
  
  all_cube_data <- data.frame(loslvl = "", agysub = "", occ = "", loc = "", 
                              agelvl = "", edlvl = "", gsegrd = "", patco = "", 
                           #   pp = "",
                              ppgrd = "", sallvl = "", stemocc = "", 
                              supervis = "", toa = "", worksch = "", workstat = "", 
                              datecode = "", employment = "", salary = "", los = "", 
                              occtyp = "", occfam = "", occfamt = "",
                              occtypt = "", occt = "", agytyp = "", agytypt = "", agy = "", 
                              agyt = "", agysubt = "", loslvlt = "", year = "")
  
  for (i in 1:length(folder_list)){
    # folder and file tracking: 
    folder_i <- folder_list[i]
    folder_i_path <- paste0(parent_folder, folder_i)
    # files within this folder: 
    folder_files <- list.files(folder_i_path)
    
    # year: 
    year_i <- as.numeric(gsub("\\D", "", folder_i))    
    print(paste0("Working on year: ", year_i))
    
    # grabbing center of cube: 
    cube_data_file <- folder_files[grepl("FACTDATA", folder_files)]
    cube_data_file <- paste0(folder_i_path, "/", cube_data_file)
    # occupation data: 
    occ_i_path <- paste0(folder_i_path, "/", "DTocc.txt")
    agency_i_path <- paste0(folder_i_path, "/", "DTagy.txt")
    los_i_path <- paste0(folder_i_path, "/", "DTloslvl.txt")
    
    # for some reason, the 2024 cube has fact data from 2024
    if(length(cube_data_file) > 1){
      cube_data_file <- cube_data_file[grepl("FACTDATA_MAR2024.TXT", cube_data_file)]
    } else {
      cube_data_file <- cube_data_file
    }
    
    # reading center of cube: 
    cube_data <- read.table(cube_data_file, sep = ",", 
                            header = TRUE, fill = TRUE) %>%
      janitor::clean_names()
    
    # occupations: 
    occ_data <- read.table(occ_i_path, 
                           sep = ",", header = TRUE, fill = TRUE, 
                           quote = "\"") %>%
      janitor::clean_names() %>%
      mutate(tidy_occ_code = occ) 
    
        # need to fix preceding zeros 
    for(i in 1:nrow(occ_data)){
      occ_data$tidy_occ_code[i] <- paste0(paste(rep(0, (4-nchar(occ_data$occ[i]))), 
                                                collapse = ""), occ_data$occ[i], collapse = "")
    }
    # merging with data cube: 
    cube_data_occ <- merge(cube_data, occ_data, by.x = "occ", 
                           by.y = "tidy_occ_code", all.x = T) %>%
      select(-c("occ.y"))
    
    # agency: 
    agency_data <- read.table(agency_i_path, 
                              sep = ",", header = TRUE, fill = TRUE) %>%
      janitor::clean_names()
    # merging with data cube: 
    cube_data_occ <- merge(cube_data_occ, agency_data, by = "agysub", all.x = T)
    
    # length of service: 
    los <- read.table(los_i_path, 
                      sep = ",", header = TRUE, fill = TRUE) %>%
      janitor::clean_names()
    # merging with data cube: 
    cube_data_occ <- merge(cube_data_occ, los, by = "loslvl", all.x = T)
    
    # tagging on a year column: 
    cube_data_occ$year <- year_i
    
    # selecting for necessary columns: 
    # cube_data_occ <- cube_data_occ %>%
    #   select(names(cube_data_occ) %in% all_cube_data)
    cube_data_occ <-cube_data_occ[, names(all_cube_data)]
    
    # rbinding with data frame: 
    all_cube_data <- rbind(all_cube_data, cube_data_occ)
  }
  
  return(all_cube_data)
}
