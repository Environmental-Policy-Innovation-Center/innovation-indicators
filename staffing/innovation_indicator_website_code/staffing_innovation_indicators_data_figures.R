###############################################################################
# OPM Data & Figures - Innovation Indicators
# last updated: March 12th 2025
# code to pull the latest staffing data, tidy the data, update our spreadsheet,
# and develop the tech % and tech ratio graphs
###############################################################################
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(aws.s3)
library(rvest)
library(googlesheets4)

# read in functions 
source("./functions/assemble_cube.R")
source("./functions/download_cubes.R")

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

###############################################################################
# step one - download and assemble data cubes from OPM
###############################################################################
## Grabbing & assembling data cubes from March 2018 - March 2024 
# note the folder you'd like to store and pull cubes from: 
cube_folder <- "./data/"
download_cubes(url = "https://www.opm.gov/data/datasets/Index.aspx", 
               dest_folder = cube_folder, up_to_year = 2013, 
               month = "March")

# looking at 10-year trends: 
folder_list <- list.files(cube_folder)

# extracting year range for our cubes: 
min_cube <- folder_list[1]
max_cube <- folder_list[length(folder_list)]
min_year <- gsub(".*?([0-9]+).*", "\\1", min_cube)
max_year <- gsub(".*?([0-9]+).*", "\\1", max_cube)

# NOTE - you might need to do some fiddling with this function if a specfiic 
# year has an inconsistent data cube file format. 
cube_data_test <- assemble_cube(parent_folder = cube_folder, 
                                folder_list = folder_list) %>%
  # the first row was just a dummy row for rbinding 
  slice(-1)
# wowza we have 25 million federal employees 

cube_data <- cube_data_test 

###############################################################################
# step two - filtering for agencies of interest
###############################################################################
# names <- data.frame(x = unique(paste0(cube_data$agyt, " - ", cube_data$agysubt))) %>%
#   View()
# the agencies we care about! note we care about some specific subagencies 
# within here, but it makes the data cube not crash my computer :-)
cube_mini_agy <- cube_data %>%
  filter(agyt %in% c("AG-DEPARTMENT OF AGRICULTURE", 
                     "IN-DEPARTMENT OF THE INTERIOR",
                     "EP-ENVIRONMENTAL PROTECTION AGENCY",
                     "AR-DEPARTMENT OF THE ARMY",
                     "CM-DEPARTMENT OF COMMERCE",
                     "NN-NATIONAL AERONAUTICS AND SPACE ADMINISTRATION", 
                     "TD-DEPARTMENT OF TRANSPORTATION", 
                     "DL-DEPARTMENT OF LABOR",
                     "HS-DEPARTMENT OF HOMELAND SECURITY")) 

# names_of_interest <- data.frame(x = unique(cube_mini_agy$agyt)) %>%
#   View()
# getting our bureaus of interest: 
bureaus <- c("AG11-FOREST SERVICE", 
             "AG16-NATURAL RESOURCES CONSERVATION SERVICE", 
             "IN15-U.S. FISH AND WILDLIFE SERVICE", "IN08-GEOLOGICAL SURVEY", 
             "IN07-BUREAU OF RECLAMATION", "IN05-BUREAU OF LAND MANAGEMENT", 
             "IN10-NATIONAL PARK SERVICE", "EP00-ENVIRONMENTAL PROTECTION AGENCY", 
             "ARCE-U.S. ARMY CORPS OF ENGINEERS",
             "CM54-NATIONAL OCEANIC AND ATMOSPHERIC ADMINISTRATION")

# filtering for enviro agencies
enviro <- cube_mini_agy %>%
  filter(agysubt %in% bureaus) %>%
  mutate(name = case_when(
    agysubt == "AG11-FOREST SERVICE" ~ "USFS",
    agysubt == "AG16-NATURAL RESOURCES CONSERVATION SERVICE" ~ "NRCS",
    agysubt == "IN15-U.S. FISH AND WILDLIFE SERVICE" ~ "USFWS",
    agysubt == "IN08-GEOLOGICAL SURVEY" ~ "USGS",
    agysubt == "IN07-BUREAU OF RECLAMATION" ~ "BOR",
    agysubt == "IN05-BUREAU OF LAND MANAGEMENT" ~ "BLM",
    agysubt == "IN10-NATIONAL PARK SERVICE" ~ "NPS",
    agysubt == "EP00-ENVIRONMENTAL PROTECTION AGENCY" ~ "EPA",
    agysubt == "ARCE-U.S. ARMY CORPS OF ENGINEERS" ~ "USACE",
    agysubt == "CM54-NATIONAL OCEANIC AND ATMOSPHERIC ADMINISTRATION" ~ "NOAA"),
    flag = "enviro_agy")

# NASA: agyt == Nn-National Aeronautics And Space Administration
# DOT: agyt == Td-Department Of Transportation
# DOL: agyt == Dl-Department Of Labor
# DHS: agyt == Hs-Department Of Homeland Security
new_depts <- cube_mini_agy %>%
  filter(agyt %in% c("NN-NATIONAL AERONAUTICS AND SPACE ADMINISTRATION", 
                     "TD-DEPARTMENT OF TRANSPORTATION", 
                     "DL-DEPARTMENT OF LABOR",
                     "HS-DEPARTMENT OF HOMELAND SECURITY")) %>%
  mutate(name = case_when(
    agyt == "NN-NATIONAL AERONAUTICS AND SPACE ADMINISTRATION" ~ "NASA",
    agyt == "TD-DEPARTMENT OF TRANSPORTATION" ~ "DOT",
    agyt == "DL-DEPARTMENT OF LABOR" ~ "DOL",
    agyt == "HS-DEPARTMENT OF HOMELAND SECURITY" ~ "DHS"),
    flag = "benchmark_agy")

# NIH: name == National Institutes Of Health, 
# department of state == "Department Of State"
new_agys <- cube_data %>%
  filter(agysubt %in% c("HE38-NATIONAL INSTITUTES OF HEALTH", 
                        "ST00-DEPARTMENT OF STATE"))%>%
  mutate(name = case_when(
    agysubt == "HE38-NATIONAL INSTITUTES OF HEALTH" ~ "NIH",
    agysubt == "ST00-DEPARTMENT OF STATE" ~ "State"),
    flag = "benchmark_agy")

# adding together and cleaning the names up: 
comp_agy <- bind_rows(new_depts, new_agys, enviro) %>%
  mutate(year = as.numeric(year)) %>%
  rename(clean_name = name)

# cleaning up our global environment
# rm(list=setdiff(ls(), "comp_agy"))

###############################################################################
# step three - adding flag for tech jobs & pivoting
###############################################################################

# in stem: 2210, 1550, 0854, 1515
tech_job <- c("1550", "2210", "1515", "0854")
# computer scientists, IT technology management, operations research, 
# computer engineering

# in stem: "1530" "1529" "1560" "0150" "1370"
data_analyst <- c("1560", "1370", "0150", "1530", "1529")
# data scientists, cartographers, geographers, cartographic technician, 
# geodetic technician, statistics, mathematical statistics 


# Grabbing stem jobs: 
comp_agy_tidy <- comp_agy %>%
  mutate(job_type_broad = case_when(!(stemocc %in% c("XXXX", "****")) ~ "STEM_research", 
                                    TRUE ~ "NOT_STEM")) %>%
  mutate(job_type_specific = case_when(stemocc %in% tech_job ~ "STEM_tech",
                                       stemocc %in% data_analyst ~ "STEM_analyst", 
                                       TRUE ~ job_type_broad)) %>%
  mutate(job_type_broad = case_when(job_type_broad == "STEM_research" ~ "STEM", 
                                    TRUE ~ job_type_broad))

# grabbing percentage of staff in each category: 
agy_per <- comp_agy_tidy %>%
  mutate(employment = as.numeric(employment)) %>%
  group_by(clean_name, year, job_type_specific, flag) %>%
  summarize(job_specific_employment = sum(employment)) %>%
  pivot_wider(names_from = job_type_specific, values_from = job_specific_employment) %>%
  # NOTE - FOR NASA - stem_analyst is 0, which creates NAs that break the summed 
  # values below
  mutate_all(., ~replace_na(., 0)) %>%
  mutate(STEM = STEM_analyst + STEM_research, 
         Tech = STEM_tech, 
         `Not STEM` = NOT_STEM) %>%
  select(clean_name, year, flag, STEM:`Not STEM`) %>%
  group_by(clean_name, year, flag) %>%
  summarize(STEM = sum(STEM, na.rm = T), 
            Tech = sum(Tech, na.rm = T), 
            `Not STEM` = sum(`Not STEM`)) %>%
  mutate(total_employees = STEM + Tech + `Not STEM`) %>%
  pivot_longer(STEM:`Not STEM`) %>%
  mutate(percent_workforce = (value/total_employees)*100)

# grabbing ratio of it to non-IT staff: 
agy_ratio <- agy_per %>% 
  select(-c(total_employees, percent_workforce)) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(non_it = STEM + `Not STEM`, 
         ratio_tech = (Tech/non_it)) %>%
  mutate(year = as.numeric(year)) %>%
  rowwise() %>%
  mutate(total_workforce = (Tech + non_it), 
         tech_pct = 100*(Tech/total_workforce)) %>%
  mutate(flag = case_when(flag == "enviro_agy" ~ " Environmental Agency", 
                          TRUE ~ " Benchmark Agency")) %>%
  mutate(category = case_when(clean_name %in% c("BLM", "BOR", "DHS", 
                                                "NPS", "NRCS", "State", "USACE",
                                                "USFS", "USFWS") ~ "Field", 
                              clean_name %in% c("DOL", "DOT", "EPA") ~ "Regulatory",
                              clean_name %in% c("NASA", "NIH", "NOAA", "USGS") ~ "Research")) %>%
  relocate(category, .after = clean_name)

###############################################################################
# step four - update relevant datasets
###############################################################################
# push to excel file or s3 bucket 
agy_ratio_gs <- agy_ratio %>%
  # this is when the last data cube is :-(
  mutate(date_last_data = paste0(max_year, "-03"))

# push to google workbook file
# write_sheet(agy_ratio_gs, 
#             "https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=0#gid=0", 
#             sheet = "tech_staffing")

###############################################################################
# step five - figures for innovation indicators website -staffing trends
###############################################################################
# first pivoting to long format for faceted graphs: 
staffing_long <- pivot_longer(agy_ratio, cols = c(ratio_tech, tech_pct), 
                              names_to = c("n")) %>%
  # creating better names: 
  mutate(n = case_when(
    n == "ratio_tech" ~ "Tech Ratio", 
    TRUE ~ "Tech Percentage")) %>%
  rename(Agency = clean_name) %>%
  rename(Year = year) %>%
  mutate(Value = round(value, 3)) %>%
  # in later iterations, we decided to just keep the tech % 
  filter(n == "Tech Percentage")

# grabbing legend order: 
agy_order <- staffing_long %>%
  group_by(Agency) %>%
  top_n(1, Year) %>%
  select(Agency, Value) %>%
  arrange(-Value) %>%
  select(Agency)
staffing_long$Agency <- factor(staffing_long$Agency, levels = agy_order$Agency)

# plottin' 
staffing_plot <- ggplot(staffing_long, aes(x = Year, y = Value, 
                                           shape = flag,
                                           color = Agency)) + 
  geom_line(linewidth = 1.2, alpha = 0.75) + 
  geom_point(size = 4, alpha = 0.8) + 
  epic_chart_theme + 
  scale_colour_manual(values = cat_palette(16), 
                      name = "") + 
  theme_minimal() + 
  theme(legend.position = "right", 
        text = element_text(size = 18), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 16), 
        axis.text.x = element_text(margin = margin(t = 10, r = 0, 
                                                   b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = -3, r = 0, 
                                                    b = 0, l = 0)), 
        axis.text.y = element_text(margin = margin(t = 0, r = 0, 
                                                   b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 0, 
                                                    b = 0, l = 0))) +
  xlim(as.numeric(min_year) - 1, as.numeric(max_year)) +
  labs(x = "Year", y = "Tech Percentage (%)") + 
  labs(shape = "") 

# creating a title for the legend based on our years: 
legend_title <- paste0("Agencies: ", min_year, "-", max_year, " <br />")
staffing_trends_innov_indicators <- plotly::ggplotly(staffing_plot)%>% 
  layout(legend = list(title = list(text = legend_title), 
                       traceorder = "normal"))

# cleaning up the legend
# from here: https://stackoverflow.com/questions/49133395/strange-formatting-of-legend-in-ggplotly-in-r
for (i in 1:length(staffing_trends_innov_indicators$x$data)){
  if (!is.null(staffing_trends_innov_indicators$x$data[[i]]$name)){
    staffing_trends_innov_indicators$x$data[[i]]$name =  gsub("\\(","",str_split(staffing_trends_innov_indicators$x$data[[i]]$name,",")[[1]][1])
  }
}

staffing_trends_innov_indicators

###############################################################################
# step six - saving figure as an html widget and putting it on aws: 
###############################################################################
# htmlwidgets::saveWidget(partial_bundle(staffing_trends_innov_indicators) %>%
#                           config(displayModeBar = FALSE) %>%
#                           config(displaylogo = FALSE) %>%
#                           layout(xaxis = list(fixedrange = TRUE),
#                                  yaxis = list(fixedrange = TRUE))%>%
#                           layout(plot_bgcolor='transparent') %>%
#                           layout(paper_bgcolor='transparent'),
#                         "results/staffing_trends.html")
# 
# put_object(
#   file = file.path("results/staffing_trends.html"),
#   object = "/innovation-indicators/I2V2/staffing_trends.html",
#   acl = "public-read",
#   bucket = "tech-team-data"
# )

# updating our assets: 
# fig <- data.frame(figure = "Tech Staffing Trends", 
#                   s3_link = "s3://tech-team-data/innovation-indicators/I2V2/staffing_trends.html", 
#                   public_link = "https://tech-team-data.s3.us-east-1.amazonaws.com/innovation-indicators/I2V2/staffing_trends.html")
# 
# range_write("https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=1922074841#gid=1922074841",
#             data = fig, range = "assets!A3:C3", col_names = FALSE)


###############################################################################
# step seven - getting summary stats for front page table
###############################################################################
# creating a column title: 
col_title <- paste0(max_year, " Workforce - Tech Percentage")

# grabbing our latest year by agency: 
summary_table <- staffing_long %>%
  group_by(Agency) %>%
  top_n(1, Year) %>%
  rename(!!sym(col_title) := Value) %>%
  ungroup() %>%
  select(!!sym(col_title))

# range_write("https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=1922074841#gid=1922074841",
#             data = summary_table, range = "summary_tables!A1:D17", col_names = TRUE)
