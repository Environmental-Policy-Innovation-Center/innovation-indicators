###############################################################################
# USAJobs Data & Figures - Innovation Indicators
# last updated: March 18th 2025
# code to pull our latest USAJobs data, tidy the data, update our spreadsheet,
# and develop the latest top 5 hiring trends graph! 
###############################################################################
library(tidyverse)
library(googlesheets4)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(aws.s3)
library(rvest)

# themes and palettes: 
sysfonts::font_add_google("Lato")
showtext::showtext_auto()

epic_chart_theme <- theme_minimal() + 
  theme(legend.position = "right", 
        text = element_text(size = 13, family = "Lato"), 
        legend.text = element_text(size = 110, family = "Lato"), 
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

##############################################################################
# step one - pull latest usajobs data and tidy!
##############################################################################
# LAST DATE PULLED 
date_pulled <- "2025-03-25"

# read latest data: 
usajobs_data <- aws.s3::s3read_using(readRDS,
                                     object = "s3://tech-team-data/enviro-hiring-trends/worker-data/usajobs_data_AWSWORKER.rds")

# tidying - lots of column names to organize 
usajobs_tidy <- usajobs_data %>%
  select(matched_object_id, 
         matched_object_descriptor_department_name, 
         matched_object_descriptor_organization_name, 
         matched_object_descriptor_job_category, 
         matched_object_descriptor_position_title, 
         matched_object_descriptor_job_grade,
         matched_object_descriptor_user_area_details_low_grade,
         matched_object_descriptor_user_area_details_high_grade,
         matched_object_descriptor_position_location, 
         matched_object_descriptor_publication_start_date, 
         matched_object_descriptor_application_close_date,
         matched_object_descriptor_qualification_summary, 
         matched_object_descriptor_user_area_details_hiring_path,
         matched_object_descriptor_user_area_details_job_summary,
         matched_object_descriptor_position_remuneration,
         matched_object_descriptor_user_area_details_major_duties,
         matched_object_descriptor_user_area_details_total_openings,
         matched_object_descriptor_user_area_details_mco_tags,
         all_keywords) %>%
  # renaming these looooong colnames
  rename(id = matched_object_id, 
         agency = matched_object_descriptor_department_name, 
         subagency = matched_object_descriptor_organization_name, 
         series = matched_object_descriptor_job_category, 
         position = matched_object_descriptor_position_title, 
         location = matched_object_descriptor_position_location, 
         grade_type = matched_object_descriptor_job_grade, 
         low_grade = matched_object_descriptor_user_area_details_low_grade,
         high_grade = matched_object_descriptor_user_area_details_high_grade,
         date_posted = matched_object_descriptor_publication_start_date, 
         date_closes = matched_object_descriptor_application_close_date,
         hiring_path = matched_object_descriptor_user_area_details_hiring_path,
         salary_rage = matched_object_descriptor_position_remuneration,
         qual_summary = matched_object_descriptor_qualification_summary, 
         job_summary = matched_object_descriptor_user_area_details_job_summary, 
         major_duties = matched_object_descriptor_user_area_details_major_duties, 
         openings = matched_object_descriptor_user_area_details_total_openings, 
         mco_tags = matched_object_descriptor_user_area_details_mco_tags) %>%
  # formatting dates and main character columns
  mutate(date_posted = as.Date(date_posted, tryFormats = c("%Y-%M-%d")), 
         date_closes = as.Date(date_closes, tryFormats = c("%Y-%M-%d")), 
         days_open = date_closes - date_posted, 
         agency = str_to_title(agency), 
         subagency = str_to_title(subagency), 
         position = str_to_title(position), 
         openings = str_to_title(openings)) %>%
  # unnesting grade column: 
  unnest(., grade_type) %>%
  rename(grade_type_code = Code) 


# unnesting ALL series associated with an announcement, and fixing 
# such that there is only one row per announcement and the series are 
# appended together in a comma-separated list 
series_tidy <- usajobs_tidy %>%
  unnest(., series) %>%
  rename(series_name = Name, 
         series_code = Code) %>%
  group_by(id, agency, subagency, position) %>%
  summarize(all_series = paste(unique(series_name), collapse = ", "), 
            all_series_code = paste(unique(series_code), collapse = ", ")) 

# merge data back with usajobs data such that the series are as a comma sep
# list
usajobs_tidy_series <- merge(usajobs_tidy, series_tidy, 
                             by = c("id", "agency", 
                                    "subagency", "position")) %>%
  select(-series)


# grabbing hiring paths and mco tags: 
hiring_paths <- unnest(usajobs_tidy_series, hiring_path) %>%
  mutate(mco_tags = sapply(mco_tags, paste, collapse = ", ")) %>%
  group_by(id, agency, subagency, position, all_series,
           all_series_code, date_posted, date_closes) %>%
  summarize(hiring_paths = paste(unique(hiring_path), collapse =", "), 
            all_mco_tags = paste(unique(mco_tags), collapse = ", "))

# merging back together: 
usajobs_simple <- merge(usajobs_tidy_series, hiring_paths, 
                        by = c("id", "agency", "subagency", 
                               "all_series_code", "all_series",
                               "position", 
                               "date_posted", 
                               "date_closes")) %>%
  select(-c(hiring_path, mco_tags)) %>%
  # grabbing columns to indicate the keyword that was hit by the API search
  mutate(data = case_when(grepl("Data", all_keywords) ~ 1, 
                          TRUE ~ 0), 
         tech = case_when(grepl("Tech", all_keywords) ~ 1, 
                          TRUE ~ 0),
         software = case_when(grepl("Soft", all_keywords) ~ 1, 
                              TRUE ~ 0),
         programming = case_when(grepl("Programming", all_keywords) ~ 1, 
                                 TRUE ~ 0), 
         prod_manager = case_when(grepl("Product", all_keywords) ~ 1, 
                                  TRUE ~ 0), 
         steward = case_when(grepl("Steward", all_keywords) ~ 1, 
                             TRUE ~ 0),
         innov = case_when(grepl("Innovation", all_keywords) ~ 1, 
                           TRUE ~ 0), 
         gis = case_when(grepl("Geospatial", all_keywords) ~ 1, 
                         TRUE ~ 0)) %>%
  # fixing the str_to_title case for U.S. 
  mutate(subagency = gsub("U.s.", "U.S.", subagency)) %>%
  mutate(total_keywords = select(., data:gis) %>% rowSums(na.rm = TRUE)) %>%
  # these are keywords not within our scope (i.e., permits)
  filter(total_keywords != 0) %>%
  # adding salary bands - unnesting them from this dataframe:
  unnest(salary_rage) %>%
  janitor::clean_names() %>%
  select(-rate_interval_code) %>%
  # creating tidy duties to pass to chatGPT - needed because of inconsistent 
  # formatting 
  mutate(tidy_duties = sapply(major_duties, paste, collapse = ". ")) %>%
  # creating a unique ID since the same job description may be present in 
  # multiple jobs: 
  mutate(tidy_duties = paste(id, tidy_duties))

# just grabbing enviro agencies of interest: ##################################
enviro_jobs <- usajobs_simple %>%
  filter(subagency %in% c("Environmental Protection Agency", 
                          "Natural Resources Conservation Service", 
                          "Forest Service", 
                          "National Oceanic And Atmospheric Administration", 
                          "National Park Service",
                          "U.S. Fish And Wildlife Service",
                          "Bureau Of Land Management",
                          "Bureau Of Reclamation",
                          "Geological Survey",
                          "U.S. Army Corps Of Engineers")) %>%
  mutate(analysis_flag = "enviro_agy")

# grabbing benchmark agencies of interest: #################################### 
benchmark_agy <- usajobs_simple %>%
  filter(subagency %in% c("National Institutes Of Health", 
                          "Department Of State - Agency Wide") |
           agency %in% c("National Aeronautics And Space Administration", 
                         "Department Of Transportation", 
                         "Department Of Labor", 
                         "Department Of Homeland Security")) %>%
  mutate(analysis_flag = "benchmark_agy")


# binding together & organizing: ##############################################
agy_tidy_jobs <- bind_rows(enviro_jobs, benchmark_agy)

# developing a summary file of key columns: 
summary_file <- agy_tidy_jobs %>%
  # fixing minimum annual salary if there was a per hour rate
  mutate(min_annual_salary = case_when(
    # assuming 40 hours a week over 52 weeks
    description == "Per Hour" ~ as.numeric(minimum_range)*40*52,
    TRUE ~ as.numeric(minimum_range))) %>%
  # flag for IT specialists: 
  mutate(it_specialist = case_when(
    grepl("1550|2210|1515|0853", all_series_code) ~ "TRUE", 
    TRUE ~ "FALSE"
  ), 
  openings = as.character(openings)) %>%
  # grabbing clean names we can use for plotting: 
  mutate(clean_name = case_when(subagency == "U.S. Army Corps Of Engineers" ~ "USACE", 
                                subagency == "Environmental Protection Agency" ~ "EPA", 
                                subagency == "Natural Resources Conservation Service" ~ "NRCS", 
                                subagency == "Forest Service" ~ "USFS", 
                                subagency == "National Oceanic And Atmospheric Administration" ~ "NOAA", 
                                subagency == "National Park Service" ~ "NPS", 
                                subagency == "U.S. Fish And Wildlife Service" ~ "USFWS", 
                                subagency == "Bureau Of Land Management" ~ "BLM", 
                                subagency == "Geological Survey" ~ "USGS", 
                                subagency == "Bureau Of Reclamation" ~ "BOR", 
                                subagency == "Department Of State - Agency Wide" ~ "State", 
                                subagency == "National Institutes Of Health" ~ "NIH", 
                                agency == "National Aeronautics And Space Administration" ~ "NASA",
                                agency == "Department Of Transportation" ~ "DOT", 
                                agency =="Department Of Labor" ~ "DOL", 
                                agency =="Department Of Homeland Security" ~ "DHS")) %>%
  # setting our category 
  mutate(category = case_when(clean_name %in% c("BLM", "BOR", "DHS", 
                                                "NPS", "NRCS", "State", "USACE",
                                                "USFS", "USFWS") ~ "Field", 
                              clean_name %in% c("DOL", "DOT", "EPA") ~ "Regulatory",
                              clean_name %in% c("NASA", "NIH", "NOAA", "USGS") ~ "Research")) %>%
  select(id, subagency, clean_name, category, position, min_annual_salary, 
         openings, all_series_code, all_series, it_specialist, analysis_flag)
  

##############################################################################
# step two - update relevant datasets
##############################################################################
# making sure we have a date last run col: 
summary_file_gs <- summary_file %>%
  mutate(date_last_data = date_pulled)

# push to google workbook file
# write_sheet(summary_file_gs,
#             "https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=0#gid=0",
#             sheet = "tech_hiring")

##############################################################################
# step three - figure development
##############################################################################
# 1) A stacked bar per agency showing the number of non-IT 
# specialists categorized by top 5 and then an "other" category.
# 2) Side-by-side bars per agency with one bar showing the number of IT 
# specialist listings and one bar showing the expected number of IT specialists?
# simple_job_postings <- readRDS("./data/simple_job_codes.rds")

# merging back with OG names: trying to create a simple key for later use
# note - some of the names have commas in them, such that they'd be difficult 
# to split: 
code_keys <- summary_file %>% 
  filter(nchar(all_series_code) == 4) %>% 
  select(all_series_code, all_series) %>%
  unique()

# splitting codes into multiple columns 
split_codes <- as.data.frame(str_split_fixed(summary_file$all_series_code, ", ", 25)) 
job_postings_split <- cbind(summary_file, split_codes) %>%
  # pivoting to long for grouping and summarizing: 
  pivot_longer(., cols = V1:V25)

# grouping and summarizing by agency and codes: 
agy_code_totals <- job_postings_split %>%
  group_by(clean_name, analysis_flag, value) %>%
  summarize(totals = n()) %>%
  filter(value != "") 

# grabbing top 5 by agency 
agy_top_five <- agy_code_totals %>% 
  group_by(clean_name, analysis_flag) %>%
  arrange(desc(totals)) %>% 
  slice(1:5) %>%
  mutate(uniq_key = paste0(value, clean_name, totals))

# organizing and renaming columns: 
agy_top_five_names <- merge(agy_top_five, code_keys, 
                            by.x = "value", by.y = "all_series_code", 
                            all.x = T) %>%
  # this is the only series that didn't match to a name in our keys dataset: 
  mutate(all_series = case_when(is.na(all_series) ~ "Agricultural Engineering", 
                                TRUE ~ all_series)) %>%
  # cleaning names for the legend (removed for i2v2 but still nice to have)
  mutate(all_series = case_when(all_series == "Information Technology Management" ~ "IT Management", 
                                TRUE ~ all_series),
         all_series = case_when(all_series == "Archeology" ~ "Archaeology", 
                                TRUE ~ all_series),
         all_series = case_when(all_series == "General Inspection, Investigation, Enforcement, And Compliance Series" ~ "General Inspection", 
                                TRUE ~ all_series),
         all_series = case_when(all_series == "General Natural Resources Management And Biological Sciences" ~ "NRM & Bio Sciences", 
                                TRUE ~ all_series)) %>%
  mutate(`Total Listings` = paste0( "Total Listings: ", totals), 
         `Job Series` = all_series) %>%
  mutate(IT_flag = case_when(all_series == "IT Management" ~ "IT", 
                             TRUE ~ "Not IT"), 
         analysis_flag =  case_when(analysis_flag == "enviro_agy" ~ "Environmental Agency", 
                                   TRUE ~ "Benchmark Agency")) %>%
 # creating a flag to order the y-axis by
  mutate(num_it = case_when(IT_flag == "IT" ~ totals, 
                            TRUE ~ 0))


# plottin' 
agy_summary_plot <- ggplot(agy_top_five_names, 
                           aes(x = totals, 
                               y = reorder(clean_name, num_it), 
                               fill = IT_flag)) + 
  geom_bar(aes(text = paste0(`Total Listings`, " - ", all_series), 
               fill = IT_flag),
           colour = "black",
           linewidth = 0.3, 
           stat = "identity", 
           # position = "stack", 
           alpha = 0.75) + 
  labs(x = "Times Job Series Listed", y = "") + 
  theme_minimal() + 
  facet_wrap(~analysis_flag, scales = "free_y", 
             ncol = 1) + 
  scale_fill_manual(values = rev(cat_palette(2)), 
                    name = "") + 
  theme(legend.position = "right", 
        text = element_text(size = 18), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 16), 
        axis.text.x = element_text(margin = margin(t = -5, r = 0, 
                                                   b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, 
                                                    b = 0, l = 0)), 
        axis.text.y = element_text(margin = margin(t = 0, r = 10, 
                                                   b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0)))
agy_top_five_plot <- plotly::ggplotly(agy_summary_plot, 
                                      tooltip = c("text"))
agy_top_five_plot


###############################################################################
# step four - saving figure as an html widget and putting it on aws: 
###############################################################################
# htmlwidgets::saveWidget(partial_bundle(agy_top_five_plot) %>%
#                           layout(xaxis = list(fixedrange = TRUE),
#                                  yaxis = list(fixedrange = TRUE))%>%
#                           layout(plot_bgcolor='transparent') %>%
#                           layout(paper_bgcolor='transparent'),
#                         "results/top_five_hiring.html")
# 
# put_object(
#   file = file.path("results/top_five_hiring.html"),
#   object = "/innovation-indicators/I2V2/top_five_hiring.html",
#   acl = "public-read",
#   bucket = "tech-team-data"
# )

# updating link for asset management: 
# fig <- data.frame(figure = "Top 5 Hiring", 
#                   s3_link = "s3://tech-team-data/innovation-indicators/I2V2/top_five_hiring.html", 
#                   public_link = "https://tech-team-data.s3.us-east-1.amazonaws.com/innovation-indicators/I2V2/top_five_hiring.html")
#
# range_write("https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=1922074841#gid=1922074841",
#             data = fig, range = "assets!A2:C2", col_names = FALSE)

###############################################################################
# step five - summary table stats for front page
###############################################################################
# grabbing tidy column name based on temporal scale: 
min_date <- min(usajobs_data$pull_date)
max_date <- max(usajobs_data$pull_date)
tidy_title <- paste0(min_date, " - ", max_date, " IT Jobs Posted")

# grab number of IT jobs posted since the scraper started: 
it_summary <- summary_file %>%
  group_by(clean_name) %>%
  # I'm also curious about the totals to check my math: 
  summarize(total = n(), 
            it_postings = sum(it_specialist == TRUE)) %>%
  arrange(clean_name) %>%
  # rename the title to match the last pull date range: 
  rename(!!sym(tidy_title) := it_postings) %>%
  select(-c(clean_name, total))

# range_write("https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=1922074841#gid=1922074841",
#             data = it_summary, range = "summary_tables!F1:F17", col_names = TRUE)
# 

