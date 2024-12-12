################################################################################
## Updating challenges data for innovation indicator analysis 
################################################################################
# libraries: 
library(tidyverse)
library(googlesheets4)
library(aws.s3)

# Important note about the raw challenges data:
# There isn't an API or full raw data download (as far as I can tell), so 
# the code in section 1 is manually pulling each year and binding them 
# together. After binding the data, check for duplicates, as some challenges 
# span multiple years. 

# If you're interested in simply updating the data from our data dictionary,
# I recommend downloading the latest data from challenges.gov and binding this
# updated data to our google spreadsheet (and checking for duplicates).

#### Section 1: ################################################################
# updating old data with new challenges: 
################################################################################
gs4_deauth()
# gs4_auth()
URL <- "https://docs.google.com/spreadsheets/d/1poMJIFPdHLN8mHLa-hVMsZRFEWn7jIBJSrFsTUQCws8/edit?gid=1005692776#gid=1005692776"
# this contains data pulled from 2020 to the later half of 2023
old_chal <- read_sheet(URL, sheet = "Nov2023") %>%
  janitor::clean_names()

# grabbing the archived challenges of 2023 to see if we have any missing: 
arch_2023 <- read_sheet(URL, sheet = "Archived_2023")%>%
  janitor::clean_names()
# which ones are new?
missing_2023 <- arch_2023 %>% 
  filter(!(challenge_id %in% old_chal$challenge_id))

# doing the same for 2024 archived challenges: 
arch_2024 <- read_sheet(URL, sheet = "Archived_2024")%>%
  janitor::clean_names()
# which ones are new? 
missing_2024 <- arch_2024 %>% 
  filter(!(challenge_id %in% old_chal$challenge_id))

# active challenges! 
active <- read_sheet(URL, sheet = "Active_Oct28_2024") %>%
  janitor::clean_names()
new_active <- active %>%
  filter(!(challenge_id %in% old_chal$challenge_id))


## binding this back with the OG data 
updated_challenges <- bind_rows(new_active, missing_2024, missing_2023) 

# recreating env flag : 
env_flagged <- old_chal %>%
  filter(env_flag == "Environmental / Natural Resource Agencies") 
env_flag <- c(unique(env_flagged$primary_agency_name), "Environmental Protection Agency - Office of Water")

# adding additional columns that were originally created in Python
updated_challenges_tidy <- updated_challenges %>% 
  separate(primary_agency_name, 
           into = c("department", "sub_agency"), 
           remove = FALSE, 
           sep = "\\s*(–|-)\\s*")  %>%
  mutate(start_date = str_extract(updated_challenges$challenge_start_date, "[^T]+"), 
         start_date = as.Date(start_date, tryFormats = c("%Y-%m-%d")), 
         start_year = year(start_date), 
         end_date = str_extract(updated_challenges$challenge_end_date, "[^T]+"), 
         end_date = as.Date(end_date, tryFormats = c("%Y-%m-%d")), 
         end_year = year(end_date), 
         duration = end_date - start_date, 
         prize_num = case_when(
           grepl("no", prize_amount, ignore.case = T) ~ 0, 
           TRUE ~ as.numeric(prize_amount)
         ), 
         env_flag = case_when(primary_agency_name %in% env_flag ~ "Environmental / Natural Resource Agencies", 
                              TRUE ~ "All Other Agencies"), 
         key_agency_name = case_when(env_flag == "Environmental / Natural Resource Agencies" ~ primary_agency_name, 
                                     TRUE ~ NA)) %>%
  select(-c(end_date))

# fixing duration and converting it to a character: 
updated_challenges_tidy$duration <- as.character(updated_challenges_tidy$duration)


# binding rows with original data: 
fed_challenges <- bind_rows(old_chal, updated_challenges_tidy) %>%
  unique()

# adding to s3: 
# tmp <- tempfile()
# write.csv(fed_challenges, file = paste0(tmp, ".csv"))
# on.exit(unlink(tmp))
# put_object(
#   file = paste0(tmp, ".csv"),
#   object = "/fed-challenges/federal_challenges.csv",
#   bucket = "tech-team-data",
# )

# this is the same file as the one in the google drive 

#### Section 2: ################################################################
# challenges data viz for blog: 
###############################################################################
# pulling in URL from Cole's analysis: 
# This file is the output of challenges_analysis.py - run the python code 
# to remake these figures!
URL <- "https://docs.google.com/spreadsheets/d/1TzIFs04m4wbns5RuqRS0knWxtEFRRm3VTPQzPCuT1lg/edit?gid=1806015268#gid=1806015268"
challenges_summary <- read_sheet(URL) %>%
  select(...8:other_trend...14) %>%
  select(-...11)

# specifying new names: 
names(challenges_summary) <- c("year", "env_trend_total", "other_trend_total", 
                               "year_tech", "env_trend_tech", "other_trend_tech")

# themes and palettes: 
sysfonts::font_add_google("Lato")
showtext::showtext_auto()

epic_chart_theme <- theme_minimal() + 
  theme(legend.position = "right", 
        text = element_text(size = 13, family = "Lato"), 
        legend.text = element_text(size = 50, family = "Lato"), 
        legend.title = element_text(size = 11, family = "Lato"), 
        axis.text.x = element_text(margin = margin(t = 10, r = 0, 
                                                   b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, 
                                                    b = 0, l = 0)), 
        axis.text.y = element_text(margin = margin(t = 0, r = 10, 
                                                   b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0))) 
cat_palette <- colorRampPalette(c("#172f60","#1054a8",
                                  "#791a7b","#de9d29", 
                                  "#b15712","#4ea324"))

## plotting totals: 
# pivoting to long format for easier plotting: 
totals <- challenges_summary %>% 
  select(year:other_trend_total) %>%
  pivot_longer(., cols = 2:3) %>%
  mutate(name = case_when(name == "env_trend_total" ~ "Environmental Agencies", 
                          TRUE ~ "Other Agencies"))
# plotting total challenges 
challenges_totals <- ggplot(totals, aes(x = year, y = value, 
                                        color = name)) + 
  geom_line(linewidth = 2, alpha = 0.75) + 
  geom_point(size = 3, alpha = 0.8) + 
  epic_chart_theme + 
  scale_colour_manual(values = cat_palette(2), 
                      name = "") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 50)) + 
  labs(x = "Challenge Start Year", y = "Total Challenges/Agency per Year") + 
  theme(text=element_text(size=80))

# ggsave("./results/challenge_totals.png", challenges_totals, dpi = 600)


## plotting tech challenges: 
# tech challenges plot: 
tech <- challenges_summary %>%
  select(year_tech:other_trend_tech)%>%
  pivot_longer(., cols = 2:3) %>%
  mutate(name = case_when(name == "env_trend_tech" ~ "Environmental Agencies", 
                          TRUE ~ "Other Agencies"))


challenges_tech <- ggplot(tech, aes(x = year_tech, y = value, 
                 color = name)) + 
  geom_line(linewidth = 2, alpha = 0.75) + 
  geom_point(size = 3, alpha = 0.8) + 
  epic_chart_theme + 
  scale_colour_manual(values = cat_palette(2), 
                      name = "") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "Challenge Start Year", y = "Tech Challenges/Agency per Year")+ 
  theme(text=element_text(size=80))

# ggsave("./results/challenge_tech.png", challenges_tech, dpi = 600)

