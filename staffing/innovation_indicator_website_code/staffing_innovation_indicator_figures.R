###############################################################################
# Figures for innovation indicators website -staffing trends
###############################################################################
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
        legend.text = element_text(size = 10, family = "Lato"), 
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
# reading in staffing data: 
url <- "https://docs.google.com/spreadsheets/d/1fauEOelZXwrVGpvZo3e2NwUBdMCctgzwsRdABBdtB74/edit?gid=0#gid=0"
staffing <- read_sheet(url, sheet = "tech_staffing") %>%
  select(-1) 
###############################################################################
# creating a stacked bar chart: 
# first pivoting to long format for faceted graphs: 
staffing_long <- pivot_longer(staffing, cols = c(ratio_tech, `tech_%`), 
                              names_to = c("n")) %>%
  # creating better names: 
  mutate(n = case_when(
    n == "ratio_tech" ~ "Tech Ratio", 
    TRUE ~ "Tech Percentage"
  )) %>%
  # keeping agency names consistent: 
  mutate(name_tidy = case_when(name %in% c("Army Corps Of Engineers", 
                                           "Fish And Wildlife Service") ~ paste0("U.S. ", name), 
                               TRUE ~ name))

staffing_plot <- ggplot(staffing_long, aes(x = year, y = value, 
                                          color = name_tidy)) + 
  geom_line(linewidth = 2, alpha = 0.75) + 
  geom_point(size = 3, alpha = 0.8) + 
  epic_chart_theme + 
  facet_wrap(~n, ncol = 1) + 
  scale_colour_manual(values = cat_palette(10), 
                      name = "") + 
  theme_minimal() + 
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
                                                    b = 0, l = 0))) +
  labs(x = "Year", y = "") + 
  xlim(2012, 2024)

staffing_trends_innov_indicators <- plotly::ggplotly(staffing_plot)

###############################################################################
# saving this as an html widget and putting it on aws: 
htmlwidgets::saveWidget(partial_bundle(staffing_trends_innov_indicators) %>%
                          config(displayModeBar = FALSE) %>%
                          config(displaylogo = FALSE) %>%
                          layout(xaxis = list(fixedrange = TRUE), 
                                 yaxis = list(fixedrange = TRUE))%>%
                          layout(plot_bgcolor='transparent') %>%
                          layout(paper_bgcolor='transparent'),
                        "results/innov_indicators_figures/staffing_trends.html")

# put_object(
#   file = file.path("results/innov_indicators_figures/staffing_trends.html"),
#   object = "/innovation-indicators/github/staffing_trends.html",
#   bucket = "tech-team-data"
# )

