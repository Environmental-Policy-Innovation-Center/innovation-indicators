################################################################################
## Updating challenges data for innovation indicator analysis 
################################################################################
# libraries: 
library(tidyverse)
library(googlesheets4)
library(aws.s3)
library(plotly)

# Important note about the raw challenges data:
# There isn't an API or full raw data download (as far as I can tell), so 
# the code in section 1 is manually pulling each year and binding them 
# together. After binding the data, check for duplicates, as some challenges 
# span multiple years. 

# If you're interested in simply updating the data from our data dictionary,
# I recommend downloading the latest data from challenges.gov and binding this
# updated data to our google spreadsheet (and checking for duplicates).

# themes and palettes: 
sysfonts::font_add_google("Lato")
showtext::showtext_auto()

epic_chart_theme <- theme_minimal() + 
  theme(legend.position = "right", 
        text = element_text(size = 13, family = "Lato"), 
        legend.text = element_text(size = 130, family = "Lato"), 
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

###############################################################################
# step one: updating old data with new challenges: 
################################################################################
# this should stay constant if we're interested in data after 2016 
min_date = as.Date("01-01-2016", tryFormats = c("%m-%d-%Y"))
min_year <- year(min_date)

# TODO - update the max year with each manual update of these data! 
max_date = as.Date("10-28-2024", tryFormats = c("%m-%d-%Y"))
max_year = year(max_date)

# essentially, I created a spreadsheet with some extra tabs containing 
# the archived 2024 - 2023 challenges, and active 2024 october challenges. 
# we already have a decent archive, so I just need to add new archived challenges  
# nov 2023 - october 2024, and add the ones that are currently active
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


###############################################################################
# step two: clean and organize data
################################################################################
challenges_tidy_gs <- bind_rows(old_chal, updated_challenges) %>%
  separate(primary_agency_name, 
           into = c("department", "sub_agency"), 
           remove = FALSE, 
           sep = "\\s*(–|-)\\s*") %>%
  mutate(start_date = str_extract(challenge_start_date, "[^T]+"), 
         start_date = as.Date(start_date, tryFormats = c("%Y-%m-%d")), 
         start_year = year(start_date), 
         end_date = str_extract(challenge_end_date, "[^T]+"), 
         end_date = as.Date(end_date, tryFormats = c("%Y-%m-%d")), 
         end_year = year(end_date), 
         # NOTE - in duration, the end day is not included in calculations 
         duration_days = as.character(end_date - start_date), 
         prize_num = case_when(grepl("no", prize_amount, ignore.case = T) ~ 0, 
                               TRUE ~ as.numeric(prize_amount))) %>% 
  select(-c(end_date, start_date, key_agency_name, count, duration)) %>%
  unique() %>%
  # tidying names to our agencies of interest: 
  mutate(tidy_agy = case_when(
    department == "National Aeronautics and Space Administration" ~ "NASA", 
    department == "Department of State" ~ "State", 
    department == "Department of Transportation" ~ "DOT",
    department == "Department of Homeland Security" ~ "DHS",
    department == "Department of Labor" ~ "DOL",
    department == "Environmental Protection Agency" ~ "EPA",
    sub_agency == "National Institutes of Health" ~ "NIH",
    sub_agency == "National Park Service" ~ "NPS",
    sub_agency == "United States Fish and Wildlife Service" ~ "USFWS",
    sub_agency == "Bureau of Reclamation" ~ "BOR", 
    sub_agency == "National Oceanic and Atmospheric Administration" ~ "NOAA",
    sub_agency == "United States Geological Survey" ~ "USGS",
    sub_agency == "Bureau of Land Management" ~ "BLM",
    # note - USACE and USFS don't have any challenges 
    sub_agency == "Army Corps of Engineers" ~ "USACE", 
    sub_agency == "United States Forest Service" ~ "USFS")) %>%
  # NOTE - this env flag has been fixed to remove challenges that are JUST DOI 
  # (so no subagency was specified) - this is to keep our definition of 
  # environmental agency consistent across analyses, but will be a discrepancy 
  # in i2v2 vs i2v1
  mutate(env_flag = case_when(tidy_agy %in% c("BLM", "USGS", "USFS", "USFWS", "BOR", 
                                              "NOAA", "EPA", "NPS", "NRCS", "USACE") ~ "Environmental Agency", 
                              tidy_agy %in% c("NASA", "State", "DOT", "DHS", "DOL", 
                                              "NIH") ~ "Benchmark Agency", 
                              TRUE ~ "Other Agency")) %>%
  # setting our category 
  mutate(category = case_when(tidy_agy %in% c("BLM", "BOR", "DHS", 
                                              "NPS", "NRCS", "State", "USACE",
                                              "USFS", "USFWS") ~ "Field", 
                              tidy_agy %in% c("DOL", "DOT", "EPA") ~ "Regulatory",
                              tidy_agy %in% c("NASA", "NIH", "NOAA", "USGS") ~ "Research")) %>%
  relocate(end_year, .after = start_year) %>% 
  relocate(env_flag, .after = tidy_agy) %>%
  # recategorizing challenges types: 
  mutate(tidy_challenge_type = case_when(
    primary_challenge_type %in% c("Analytics, visualizations, algorithms",
                                  "Technology demonstration and hardware", 
                                  "Software and apps") ~ "Analysis, Tech Software", 
    primary_challenge_type %in% c("Creative (multimedia & design)") ~ "Creative", 
    primary_challenge_type %in% c("Ideas", 
                                  "Business plans", 
                                  "Nominations") ~ "Ideas, Plans, Nominations", 
    primary_challenge_type %in% c("Scientific") ~ "Scientific")) %>%
  mutate(date_last_data = max_date) 


###############################################################################
# step three: update s3 and google sheets
###############################################################################
# adding to s3: NOTE - don't think we need this since 
# we're going to google sheet route 
# tmp <- tempfile()
# write.csv(fed_challenges, file = paste0(tmp, ".csv"))
# on.exit(unlink(tmp))
# put_object(
#   file = paste0(tmp, ".csv"),
#   object = "/fed-challenges/federal_challenges.csv",
#   bucket = "tech-team-data",
# )

# push to google workbook file
# write_sheet(challenges_tidy_gs,
#             "https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=0#gid=0",
#             sheet = "challenges")

###############################################################################
# step four: get total challenges by year for all agencies:
###############################################################################
type_summary <- challenges_tidy_gs %>%
  filter(start_year >= min_year) %>%
  group_by(start_year, env_flag, tidy_challenge_type) %>%
  summarize(total_challenges = length(unique(challenge_id))) %>%
  pivot_wider(., names_from = tidy_challenge_type, values_from = total_challenges) %>%
  mutate_all(., ~replace_na(., 0)) %>%
  rowwise() %>%
  mutate(total_env_challenges = sum(`Analysis, Tech Software` + Creative + `Ideas, Plans, Nominations` + Scientific)) 

# get total number of departments participating in a given year: 
agy_summary <- challenges_tidy_gs %>% 
  filter(start_year >= min_year) %>%
  group_by(start_year, env_flag) %>%
  summarize(env_agencies = length(unique(department)))

# merge and divide number of tech challenges by participating departments: 
env_trend_summary <- merge(type_summary, agy_summary, by = c("start_year", "env_flag")) %>%
  mutate(tech_enviro =  `Analysis, Tech Software` / env_agencies) %>%
  filter(env_flag != "Other Agency")


###############################################################################
# step five: challenges data viz for website 
###############################################################################
# fixing names for plot: 
env_trend_plot_data <- env_trend_summary %>%
  mutate(`Tech Challenges / Agency` = round(tech_enviro, 3)) %>%
  rename(Year = start_year, 
         `Agency Type` = env_flag)

# plottin' 
challenges_tech <- ggplot(env_trend_plot_data, aes(x = Year, 
                                                 y = `Tech Challenges / Agency`, 
                                                 color =  `Agency Type`)) + 
  geom_line(linewidth = 2, alpha = 0.75) + 
  xlim(min_year, max_year) +
  geom_point(size = 3, alpha = 0.8) + 
  scale_colour_manual(values = rev(c("#4EA324","#172F60")), 
                      name = "") + 
  theme_minimal() + 
  theme(legend.position = "bottom", 
        text = element_text(size = 18), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 16), 
        axis.text.x = element_text(margin = margin(t = 0, r = 0, 
                                                   b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = -15, r = 0, 
                                                    b = 0, l = 0)), 
        axis.text.y = element_text(margin = margin(t = 0, r = 0, 
                                                   b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 0, 
                                                    b = 0, l = 0))) +
  labs(x = "Challenge Start Year", y = "Tech Challenges/Agency per Year")

challenges_plotly <- plotly::ggplotly(challenges_tech)%>%
  layout(legend = list(
    orientation = "h",
    x = -0.1, 
    y = -0.2))

challenges_plotly

###############################################################################
# step six - saving figure as an html widget and putting it on aws: 
###############################################################################
# htmlwidgets::saveWidget(partial_bundle(challenges_plotly) %>%
#                           config(displayModeBar = FALSE) %>%
#                           config(displaylogo = FALSE) %>%
#                           layout(xaxis = list(fixedrange = TRUE),
#                                  yaxis = list(fixedrange = TRUE))%>%
#                           layout(plot_bgcolor='transparent') %>%
#                           layout(paper_bgcolor='transparent'),
#                         "results/tech_challenges.html")
# 
# put_object(
#   file = file.path("results/tech_challenges.html"),
#   object = "/innovation-indicators/I2V2/tech_challenges.html",
#   acl = "public-read",
#   bucket = "tech-team-data"
# )

# updating our assets: 
# fig <- data.frame(figure = "Tech Challenges",
#                   s3_link = "s3://tech-team-data/innovation-indicators/I2V2/tech_challenges.html",
#                   public_link = "https://tech-team-data.s3.us-east-1.amazonaws.com/innovation-indicators/I2V2/tech_challenges.html")
# 
# range_write("https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=1922074841#gid=1922074841",
#             data = fig, range = "assets!A4:C4", col_names = FALSE)

###############################################################################
# step seven - summary tables
###############################################################################
# summary table for first page 
# tech challenges by subagency - total count of those across the universe of data.
challenges_summary_table <- challenges_tidy_gs %>%
  # we don't need other agencies for this summary table: 
  filter(!is.na(tidy_agy)) %>%
  group_by(tidy_agy) %>%
  summarize(total_challenges = n(), 
            tech_chal = sum(start_year >= min_year & tidy_challenge_type == "Analysis, Tech Software"))

# these two don't have any challenges but I still need to add so the 
# rows line up with the summary_tables spreadsheet
# TODO - this may change in the future
filler_rows <- data.frame(tidy_agy = c("USFS", "USACE", "NRCS"), 
                          total_challenges = c(0,0, 0), 
                          tech_chal = c(0,0, 0))

# binding filler rows and renaming: 
col_title <- paste0(min_year, " - ", max_year, " Tech Challenges")
final_summary_table <- bind_rows(filler_rows, challenges_summary_table) %>%
  arrange(tidy_agy) %>%
  rename(!!sym(col_title) := tech_chal) %>%
  select(-c(total_challenges, tidy_agy))

# gs4_deauth()
# gs4_auth()
# range_write("https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=1922074841#gid=1922074841",
#             data = final_summary_table, range = "summary_tables!E1:E17", col_names = TRUE)


# summary tables for challenges page 
# I2v1 data came from here: https://docs.google.com/spreadsheets/d/1jbohDzXp9AP81o22cOyv64JBPWNrZh_N9mQjfqCNk-4/edit?gid=1181720149#gid=1181720149
chal_page_summary <- challenges_tidy_gs %>%
  filter(start_year >= min_year & start_year <= max_year) %>%
  group_by(start_year, tidy_agy, env_flag, tidy_challenge_type) %>%
  summarize(total_challenges = length(unique(challenge_id))) %>%
  pivot_wider(., names_from = tidy_challenge_type, values_from = total_challenges) %>%
  mutate_all(., ~replace_na(., 0)) %>%
  rowwise() %>%
  mutate(total_challenges = sum(`Analysis, Tech Software` + Creative + `Ideas, Plans, Nominations` + Scientific)) 

# agency breakdown: 
agy_breakdown <- chal_page_summary %>%
  filter(env_flag != "Other Agency") %>%
  group_by(tidy_agy, env_flag) %>%
  summarize(total_challenges = sum(total_challenges), 
            total_tech_challenges = sum(`Analysis, Tech Software`)) %>%
  mutate(tech_nontech_chal = round(total_tech_challenges/total_challenges, 3))

# adding to our workbook! 
# range_write("https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=1922074841#gid=1922074841",
#             data = agy_breakdown, range = "challenge_summary_tables!A1:E12", col_names = TRUE)


# means and number of agencies participating: 
flag_breakdown <- challenges_tidy_gs %>%
  filter(start_year >= min_year & start_year <= max_year) %>%
  group_by(start_year, primary_agency_name, 
           tidy_agy, env_flag, tidy_challenge_type) %>%
  summarize(total_challenges = length(unique(challenge_id))) %>%
  pivot_wider(., names_from = tidy_challenge_type, values_from = total_challenges) %>%
  mutate_all(., ~replace_na(., 0)) %>%
  # gotta grab the primary agency name and tidy agency because the EPA is sometimes
  # just EPA and other times its EPA - office of water but we want to keep 
  # that as one unit 
  mutate(unique_agencies = case_when(env_flag != "Other Agency" ~ tidy_agy,
                                     TRUE ~ primary_agency_name)) %>%
  rowwise() %>%
  mutate(total_challenges = sum(`Analysis, Tech Software` + Creative + `Ideas, Plans, Nominations` + Scientific)) %>%
  group_by(env_flag) %>%
  summarize(unique_agencies = length(unique(unique_agencies)),
            total_challenges = sum(total_challenges),
            total_tech_challenges = sum(`Analysis, Tech Software`)) %>%
  mutate(tech_nontech_chal = round(total_tech_challenges/total_challenges, 3), 
         average_total_chal = round(total_challenges/unique_agencies, 3), 
         average_tech_chal = round(total_tech_challenges/unique_agencies, 3)) 

# adding to our workbook! 
# range_write("https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=1922074841#gid=1922074841",
#             data = flag_breakdown, range = "challenge_summary_tables!A15:G18", col_names = TRUE)
# 
