###############################################################################
# figures for innovation indicators report
# Nov 22nd data viz: 
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
        legend.text = element_text(size = 60, family = "Lato"), 
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
# usajobs: 
##############################################################################
# grabbing usajobs summary: 
url <- "https://docs.google.com/spreadsheets/d/1fauEOelZXwrVGpvZo3e2NwUBdMCctgzwsRdABBdtB74/edit?gid=0#gid=0"
usajobs_summary <- read_sheet(url, sheet = "tech_hiring") 

# fixing hiring codes: 
unlisted_codes <- unlist(usajobs_summary$all_series_code)
usajobs_tidy <- cbind(usajobs_summary, unlisted_codes) %>%
  mutate(tidy_code = case_when(
    # there's definitely a better way to do this 
    nchar(unlisted_codes) == 3 ~ paste0("0", unlisted_codes), 
    nchar(unlisted_codes) == 2 ~ paste0("00", unlisted_codes), 
    nchar(unlisted_codes) == 1 ~ paste0("000", unlisted_codes), 
    TRUE ~ unlisted_codes)) %>%
  select(id, subagency, tidy_code, all_series) %>%
  rename(all_series_code = tidy_code)

# renaming: 
simple_job_postings <- usajobs_tidy

# 1) A stacked bar per agency showing the number of non-IT 
# specialists categorized by top 5 and then an "other" category.
# 2) Side-by-side bars per agency with one bar showing the number of IT 
# specialist listings and one bar showing the expected number of IT specialists?
# simple_job_postings <- readRDS("./data/simple_job_codes.rds")

# merging back with OG names: trying to create a simple key for later use
# note - some of the names have commas in them, such that they'd be difficult 
# to split: 
code_keys <- simple_job_postings %>% 
  filter(nchar(all_series_code) == 4) %>% 
  select(-c(id, subagency)) %>%
  unique()

# splitting codes into multiple columns 
split_codes <- as.data.frame(str_split_fixed(simple_job_postings$all_series_code, ", ", 25)) 
job_postings_split <- cbind(simple_job_postings, split_codes) %>%
  # pivoting to long for grouping and summarizing: 
  pivot_longer(., cols = V1:V25)

# grouping and summarizing by agency and codes: 
agy_code_totals <- job_postings_split %>%
  group_by(subagency, value) %>%
  summarize(totals = n()) %>%
  filter(value != "") 

# agy_totals_noIT <- agy_code_totals 
# # removing IT SPECIALISTS !!!
# filter(!(value %in% c(1550, 2210, 1515,0853)))

# grabbing top 5 by agency 
agy_top_five <- agy_code_totals %>% 
  group_by(subagency) %>%
  arrange(desc(totals)) %>% 
  slice(1:5) %>%
  mutate(uniq_key = paste0(value, subagency, totals))

# grabbing all of the other totals: 
# agy_others <- agy_totals_noIT %>%
#   mutate(uniq_key = paste0(value, subagency, totals)) %>%
#   filter(!(uniq_key %in% agy_top_five$uniq_key)) %>%
#   group_by(subagency) %>%
#   summarize(totals = sum(totals)) %>%
#   mutate(value = "other", 
#          all_series = "Other")

agy_top_five_names <- merge(agy_top_five, code_keys, 
                            by.x = "value", by.y = "all_series_code", 
                            all.x = T) %>%
  # this is the only series that didn't match to a name in our keys dataset: 
  mutate(all_series = case_when(is.na(all_series) ~ "Agricultural Engineering", 
                                TRUE ~ all_series)) %>%
  mutate(all_series = case_when(all_series == "Information Technology Management" ~ "*Information Technology Management", 
                                TRUE ~ all_series))
agy_summary_plot <- ggplot(agy_top_five_names, 
                           aes(x = totals, 
                               y = reorder(subagency, totals), 
                               color = all_series, 
                               fill = all_series)) + 
  geom_bar(stat = "identity", position = "stack", alpha = 0.75) + 
  labs(x = "Times Job Series Listed", y = "") + 
  theme_minimal() + 
  scale_colour_manual(values = cat_palette(31), 
                      name = "") + 
  scale_fill_manual(values = cat_palette(31), 
                    name = "") + 
  theme(legend.position = "right", 
        text = element_text(size = 13), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 11), 
        axis.text.x = element_text(margin = margin(t = 10, r = 0, 
                                                   b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, 
                                                    b = 0, l = 0)), 
        axis.text.y = element_text(margin = margin(t = 0, r = 10, 
                                                   b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0)))
agy_top_five_plot <- plotly::ggplotly(agy_summary_plot)
###############################################################################
# saving this as an html widget and putting it on aws: 
htmlwidgets::saveWidget(partial_bundle(agy_top_five_plot) %>%
                          config(displayModeBar = FALSE) %>%
                          config(displaylogo = FALSE) %>%
                          layout(xaxis = list(fixedrange = TRUE), 
                                 yaxis = list(fixedrange = TRUE))%>%
                          layout(plot_bgcolor='transparent') %>%
                          layout(paper_bgcolor='transparent'),
                        "results/top_five_hiring.html")

# put_object(
#   file = file.path("results/top_five_hiring.html"),
#   object = "/innovation-indicators/github/top_five_hiring.html",
#   bucket = "tech-team-data"
# )

# 2) Side-by-side bars per agency with one bar showing the number of IT 
# specialist listings and one bar showing the expected number of IT specialists?
tech_summary <- usajobs_summary %>%
  group_by(subagency) %>%
  summarize(total_listings = n(), 
            Actual = sum(it_specialist)) %>%
  mutate(tech_ratio = Actual/total_listings)

# mean tech ratio: this is the same as summing them and dividing by 10
mean_tech_ratio <- mean(tech_summary$tech_ratio)

# multiplying the mean by total listings 
expected_tech <- tech_summary %>%
  mutate(Expected = total_listings * mean_tech_ratio) %>%
  pivot_longer(., cols = c(Actual, Expected))

expected_tech_figure <- ggplot(expected_tech, aes(x = value, y = subagency, 
                                                  fill = name, 
                                                  color = name)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.75) + 
  epic_chart_theme + 
  labs(x = "Number of Tech Listings", y = "") + 
  scale_fill_manual(values = cat_palette(2), 
                    name = "") +
  scale_color_manual(values = cat_palette(2), 
                     name = "") +
  theme(text=element_text(size=80),
        axis.text.y = element_text(size = 90))

# ggsave("./results/expected_tech.png", expected_tech_figure, dpi = 600, bg = "white")
