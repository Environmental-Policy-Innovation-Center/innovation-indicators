# Scoping tech capacity data 
# Author: EmmaLi Tsai
# Date: 1/11/2024 
# 
# libraries: 
library(tidyverse)
library(janitor)
# 
# assembling the data cube: ####################################################
# the center of the cube (March 2023 data - would have to repeat to look at 
# trends through time)
cube_data <-  read.table("./data/FedScope_Employment_Cube_March2023/FACTDATA_MAR2023.txt", 
                         sep = ",", header = TRUE) %>%
  clean_names()

# occupations: 
occ_data <- read.table("./data/FedScope_Employment_Cube_March2023/DTocc.txt", 
                       sep = ",", header = TRUE) %>%
  mutate(tidy_occ_code = OCC) %>%
  select(-c("OCCTYP", "OCCFAM", "OCCFAMT")) %>%
  clean_names()
# need to fix preceding zeros 
for(i in 1:nrow(occ_data)){
  occ_data$tidy_occ_code[i] <- paste0(paste(rep(0, (4-nchar(occ_data$occ[i]))), 
                                       collapse = ""), occ_data$occ[i], collapse = "")
}

# merging with data cube: 
cube_data_occ <- merge(cube_data, occ_data, by.x = "occ", 
                       by.y = "tidy_occ_code") %>%
  select(-c("occ.y"))

# agency: 
agency_data <- read.table("./data/FedScope_Employment_Cube_March2023/DTagy.txt", 
                       sep = ",", header = TRUE, fill = TRUE) %>%
  clean_names()
cube_data_occ <- merge(cube_data_occ, agency_data, by = "agysub")

# length of service: 
los <- read.table("./data/FedScope_Employment_Cube_March2023/DTloslvl.txt", 
                sep = ",", header = TRUE) %>%
  clean_names()
cube_data_occ <- merge(cube_data_occ, los, by = "loslvl")

# quick look at trends: #######################################################
usfs_stem <- cube_data_occ %>%
  filter(agysub == "AG11") %>%
  filter(stemocc != "XXXX") %>%
  group_by(occt, ppgrd) %>%
  summarize(total_employees = sum(employment), 
            mean_salary = mean(salary), 
            length_of_service = mean(los))

ggplot(usfs_stem, aes(x = occt, y = total_employees, color = ppgrd)) + 
  geom_point(size = 2) +
  theme_bw() + 
  coord_flip() + 
  labs(y = "Total Employees", x = "Occupation")


usfs_stem_long <- pivot_longer(usfs_stem, total_employees:length_of_service)

ggplot(usfs_stem_long, aes(x = ppgrd, y = value, color = occt)) + 
  geom_point() +
  facet_wrap(~name, scales = "free", ncol = 1) + 
  theme_bw()



