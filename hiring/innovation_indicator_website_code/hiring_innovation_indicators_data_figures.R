### Libraires & Creds###########################################################
library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(plotly)
library(googlesheets4)
library(aws.s3)

# credentials: 


# new function that takes agency or dept codes and grabs data from historic
# USAJobs API
source("./functions/grab_hist_joa.R")

# themes and palettes: 
sysfonts::font_add_google("Lato")
showtext::showtext_auto()

cat_palette <- colorRampPalette(c("#172f60","#1054a8",
                                  "#791a7b","#de9d29", 
                                  "#b15712","#4ea324"))

### Data exploration ###########################################################
# testing historic API
url_hist <- 'https://data.usajobs.gov/api/historicjoa?PositionSeries=2210'

# connect w/ API and transfer to readable format:
get_job_hist <- httr::GET(url_hist, httr::add_headers("Host"=host,
                                                      "Authorization-Key"=authkey,
                                                      "User-Agent"=useragent))
# grabbin' data
get_job_text_hist <-content(get_job_hist, "text")
get_job_json_hist <- fromJSON(get_job_text_hist, flatten = TRUE)
# looking out output
scientist_jobs <- get_job_json_hist$data

################################################################################
# step one: grab agency codes
################################################################################
# let's get agency codes: 
url_codes <- 'https://data.usajobs.gov/api/codelist/agencysubelements'
# connect w/ API and transfer to readable format:
get_codes <- httr::GET(url_codes, httr::add_headers("Host"=host,
                                                    "Authorization-Key"=authkey,
                                                    "User-Agent"=useragent))
codes <-content(get_codes, "text")
codes_text <- fromJSON(codes, flatten = TRUE)
codes_df <- as.data.frame(codes_text$CodeList$ValidValue)

################################################################################
# step two: grab environmental agency data
################################################################################
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

# NOTE - due to either traffic or something, I often get kicked off of the API
# and have to re-run loops. It's really annoying for USACE and DHS (which both 
# have > 100 pages of information) - so I often have to split the loop
# into different agencies/departments to help grab all of the data we want. 

# I already grabbed USGS and NRCS while testing this function, so let's grab 
# the rest! 
enviro_codes_test <- enviro_codes %>% 
  filter((Value %in% c("U.S. Army Corps of Engineers")))
                       # "National Park Service", 
                       # "U.S. Army Corps of Engineers")))
grab_hist_joa(enviro_codes_test, is_dept = F)

################################################################################
# step three: grab comparative agency data
################################################################################
# comp agy codes: 
comp_codes_dept <- codes_df %>%
  # NASA,DOT, DOL, homeland security, department of state
  filter(Code %in% c("NN", "TD", "DL", "HS", "ST"))
comp_codes_agy <- codes_df %>%
  #  NIH
  filter(Code %in% c("HE38"))

# go ahead and grab NIH: 
grab_hist_joa(comp_codes_agy, is_dept = F)

# grab other depatments that are a lil easier get: 
comp_codes_dept <- codes_df %>%
  # NASA,DOT, DOL, homeland security, department of state
  filter(Code %in% c("NN", "TD", "DL", "ST"))
grab_hist_joa(comp_codes_dept, is_dept = T)

# still need HS - it's just huge (286 pages) and will take a long time 
comp_codes_dept <- codes_df %>%
  filter(Code %in% c("HS"))
grab_hist_joa(comp_codes_dept, is_dept = T)
# I'll probably have to grab the department ones separately from the agency 
# ones 

################################################################################
# step four: combining data
################################################################################
# grabbing data from comparison agencies: 
nasa <- readRDS("./data/National Aeronautics and Space Administration/hist_jobs.RDS") %>%
  mutate(clean_name = "NASA", 
         category = "Research")
nih <- readRDS("./data/National Institutes of Health/hist_jobs.RDS")%>%
  mutate(clean_name = "NIH", 
         category = "Research")
dot <- readRDS("./data/Department of Transportation/hist_jobs.RDS")%>%
  mutate(clean_name = "DOT", 
         category = "Regulatory")
dos <- readRDS("./data/Department of State/hist_jobs.RDS")%>%
  mutate(clean_name = "State", 
         category = "Field")
dol <- readRDS("./data/Department of Labor/hist_jobs.RDS")%>%
  mutate(clean_name = "DOL", 
         category = "Regulatory")
dhs <- readRDS("./data/Department of Homeland Security/hist_jobs.RDS")%>%
  mutate(clean_name = "DHS", 
         category = "Field")

# combining: 
comp_agy <- bind_rows(nasa, nih, dot, dos, dol, dhs) %>%
  mutate(date_pulled = "06/16/2025", 
         type = "benchmark_agy") 

# grabbing enviro agencies: 
noaa <- readRDS("./data/National Oceanic and Atmospheric Administration/hist_jobs.RDS")%>%
  mutate(clean_name = "NOAA", 
         category = "Research")
usgs <- readRDS("./data/Geological Survey/hist_jobs.RDS")%>%
  mutate(clean_name = "USGS", 
         category = "Research")
epa <- readRDS("./data/Environmental Protection Agency/hist_jobs.RDS")%>%
  mutate(clean_name = "EPA", 
         category = "Regulatory")
usace <- readRDS("./data/U.S. Army Corps of Engineers/hist_jobs.RDS")%>%
  mutate(clean_name = "USACE", 
         category = "Field")
nps <- readRDS("./data/National Park Service/hist_jobs.RDS")%>%
  mutate(clean_name = "NPS", 
         category = "Field")
bor <- readRDS("./data/Bureau of Reclamation/hist_jobs.RDS")%>%
  mutate(clean_name = "BOR", 
         category = "Field")
blm <- readRDS("./data/Bureau of Land Management/hist_jobs.RDS")%>%
  mutate(clean_name = "BLM", 
         category = "Field")
usfws <- readRDS("./data/U.S. Fish and Wildlife Service/hist_jobs.RDS")%>%
  mutate(clean_name = "USFWS", 
         category = "Field")
usfs <- readRDS("./data/Forest Service/hist_jobs.RDS")%>%
  mutate(clean_name = "USFS", 
         category = "Field")
nrcs <- readRDS("./data/Natural Resources Conservation Service/hist_jobs.RDS")%>%
  mutate(clean_name = "NRCS", 
         category = "Field")

# combining
enviro_agy <- bind_rows(noaa, usgs, epa, usace, nps, bor, blm, usfws, usfs, nrcs)%>%
  mutate(date_pulled = "06/16/2025", 
         type = "enviro_agy") 

# grabbin everything: 
hist_jobs <- bind_rows(enviro_agy, comp_agy) %>%
  clean_names()

# cleanin' 
hist_jobs_clean <- hist_jobs %>%
  mutate(open_date_clean = as.Date(position_open_date, tryFormats = c("%Y-%m-%d")), 
         open_year = year(open_date_clean), 
         close_date_clean = as.Date(position_close_date, tryFormats = c("%Y-%m-%d")),
         exp_date_clean = as.Date(position_expire_date, tryFormats = c("%Y-%m-%d")), 
         it_flag = case_when(grepl(c("1550|2210|1515|0853"), hist_jobs$job_categories) ~ "IT", 
                             TRUE ~ "not_IT"), 
         unique_id = paste0(usajobs_control_number, "--", announcement_number))

# although I don't need to unnest the job categories and locations to capture 
# IT jobs, I'll need to do that in order to add the data to google sheets: 
job_series_clean <- hist_jobs_clean %>%
  unnest(job_categories) %>%
  group_by(usajobs_control_number) %>%
  reframe(job_series = paste(unique(series), collapse = ", "))

pos_loc_clean <- hist_jobs_clean %>%
  unnest(position_locations) %>%
  group_by(usajobs_control_number) %>%
  reframe(job_city = paste(unique(positionLocationCity), collapse = ", "), 
          job_state = paste(unique(positionLocationState), collapse = ", "), 
          job_country = paste(unique(positionLocationCountry), collapse = ", "))

hiring_paths_clean <- hist_jobs_clean %>%
  unnest(hiring_paths) %>%
  group_by(usajobs_control_number)%>%
  reframe(hiring_paths_all = paste(unique(hiringPath), collapse = ", "))

# merging together before adding to the larger df: 
job_pos <- merge(pos_loc_clean, job_series_clean, by = "usajobs_control_number")
job_pos_hir <- merge(job_pos, hiring_paths_clean, all.x = T)

# meging back with og
hist_jobs_gs <- merge(hist_jobs_clean, job_pos_hir, 
                      by = "usajobs_control_number", all.x = T) %>%
  select(-c(position_locations, job_categories, hiring_paths)) %>%
  as.data.frame()
################################################################################
# step five: data checks
################################################################################
# making sure nothing got dropped in the cleaning process and that we don't 
# have any duplicates 
nrow(hist_jobs_clean)
nrow(hist_jobs)
nrow(unique(hist_jobs_clean))
hist_jobs_clean %>% 
  select(-page) %>% 
  unique() %>% 
  nrow()

# there are no duplicated control numbers - woo! 
dup_control <- hist_jobs_clean[duplicated(hist_jobs_clean$usajobs_control_number),]
length(unique(hist_jobs_clean$usajobs_control_number))
# we still can't use the "total openings" - mix of NA, 1, or "FEW", or 
# my personal favorite "mANY" 
unique(hist_jobs_clean$total_openings)

# what are the duplicated announcement numbers? 
dup_ann <- hist_jobs_clean[duplicated(hist_jobs_clean$announcement_number),]
unique(dup_ann$hiring_agency_name)
# checking a couple of them out: hmm - these are almost identical but with very 
# sliightly different dates ... what's going on here? 
hist_jobs_clean %>% filter(announcement_number == "NMFS-NWC-2017-0031")
hist_jobs_clean %>% filter(announcement_number == "NMFS-NWC-2017-0032")
hist_jobs_clean %>% filter(announcement_number == "NSDIS-OSPO-2021-0024")
# this doesn't really matter for our work, since we're tracking announcements 
# (rows), rather than unique entries in this field 

# checking the IT flag: 
test_it <- hist_jobs_clean %>%
  filter(it_flag == "IT") %>%
  unnest(job_categories)
# spot check for ones that I found were weird: 
hist_jobs_clean %>%
  filter(usajobs_control_number == "470720700")
# okay so 1515 was in the job_categories object of this entry, along w/ 0482 
# (operations resarch). There aren't any 0853 series 

# seems like data collection started for agencies 
# in 2014-2017, so we should only analyze data past 2017. Maybe over the 
# last 5 years?
hist_jobs_clean %>%
  group_by(clean_name) %>%
  summarize(min_year = min(open_year), 
            max_year = max(open_year), 
            announcements = n(), 
            num_it = sum(it_flag == "IT"))

################################################################################
# step six: tidy data for googlesheets
################################################################################
hist_jobs_gs_f <- hist_jobs_gs %>%
  # too much data for google sheets to handle :(
  filter(open_year >= 2020 & open_year <= 2025) %>%
  group_by(clean_name, type, category, open_year) %>%
  summarize(announcements = n(), 
            num_it = sum(it_flag == "IT")) %>%
  mutate(pct_it = round(100*(num_it/announcements), 2)) %>%
  mutate(clean_type = case_when(type == "enviro_agy" ~ "Environmental Agency", 
                                TRUE ~ "Benchmark Agency")) %>%
  rename(Year = open_year, 
         `% IT` = pct_it, 
         `Agency` = clean_name) %>%
  mutate(date_last_data = "06/16/2025") 
  

# push to google workbook file - I can't add the raw data (way too big!)
# write_sheet(hist_jobs_gs_f,
#             "https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=0#gid=0",
#             sheet = "tech_hiring")

# also adding raw file to aws s3
# write.csv(hist_jobs_gs, "./data/historic_jobs_clean_gs.csv")
# put_object(
#   file = file.path("./data/historic_jobs_clean_gs.csv"),
#   object = "/innovation-indicators/I2V2/data/historic_jobs_clean_gs.csv",
#   acl = "public-read",
#   bucket = "tech-team-data"
# )

################################################################################
# step seven: figure dev
################################################################################
# grabbing a year summary 
hist_jobs_summary <- hist_jobs_clean %>%
  # note that 2025 only contains half of the year
  filter(open_year >= 2020 & open_year <= 2025) %>%
  group_by(clean_name, type, category, open_year) %>%
  summarize(announcements = n(), 
            num_it = sum(it_flag == "IT")) %>%
  mutate(pct_it = round(100*(num_it/announcements), 2)) %>%
  mutate(clean_type = case_when(type == "enviro_agy" ~ "Environmental Agency", 
                                TRUE ~ "Benchmark Agency")) %>%
  rename(Year = open_year, 
         `% IT` = pct_it, 
         `Agency` = clean_name)

# quick plot: 
hiring_plot <- ggplot(hist_jobs_summary, 
                      aes(x = Year, y = `% IT`, color = `Agency`, group = clean_type)) + 
  geom_line(linewidth = 1.5, alpha = 0.75) + 
  geom_point(aes(text = paste0( "Agency: ", `Agency`,
                                "; Category: ", category, 
                                "; Announcements: ", announcements, 
                                "; % IT: ", `% IT`, "%")), size = 2) + 
  geom_smooth(method = "lm", color = "black", se = F, lty = "dashed") + 
  scale_color_manual(values = cat_palette(16), 
                     name = "") + 
  facet_wrap(~clean_type) + 
  # geom_abline(intercept = 0, slope = 0) + 
  labs(x = "Year of Job Announcement", 
       y = "% of IT Announcements")+ 
  theme_minimal() + 
  theme(legend.position = "right", 
        text = element_text(size = 13), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 11), 
        axis.text.x = element_text(margin = margin(t = 10, r = 0, 
                                                   b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = -5, r = 0, 
                                                    b = 0, l = 0)), 
        axis.text.y = element_text(margin = margin(t = 0, r = 10, 
                                                   b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = -5, 
                                                    b = 0, l = 0))) +
  theme(legend.position = "bottom")

hiring_plotly <- plotly::ggplotly(hiring_plot, 
                                  tooltip = c("text"))
hiring_plotly

###############################################################################
# step eight - saving figure as an html widget and putting it on aws: 
###############################################################################
# htmlwidgets::saveWidget(partial_bundle(hiring_plotly) %>%
#                           config(displayModeBar = FALSE) %>%
#                           config(displaylogo = FALSE) %>%
#                           layout(xaxis = list(fixedrange = TRUE),
#                                  yaxis = list(fixedrange = TRUE))%>%
#                           layout(plot_bgcolor='transparent') %>%
#                           layout(paper_bgcolor='transparent'),
#                         "results/historic_hiring.html")
# 
# put_object(
#   file = file.path("results/historic_hiring.html"),
#   object = "/innovation-indicators/I2V2/historic_hiring.html",
#   acl = "public-read",
#   bucket = "tech-team-data"
# )
# 
# # updating our assets: 
# fig <- data.frame(figure = "Tech Hiring Over Time",
#                   s3_link = "s3://tech-team-data/innovation-indicators/I2V2/historic_hiring.html",
#                   public_link = "https://tech-team-data.s3.us-east-1.amazonaws.com/innovation-indicators/I2V2/historic_hiring.html")
# 
# range_write("https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=1922074841#gid=1922074841",
#             data = fig, range = "assets!A7:C7", col_names = FALSE)
# 

