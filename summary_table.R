# summary table for front page! 
# emmali tsai 
# March 31st 2025

# libraries: 
library(googlesheets4)
library(reactable)
library(reactablefmtr)

# colors ! 
green_palette <- c("#bdd9b6", "#9cc791", "#4ea324",
                  "#3c8e1a", "#2b7911", "darkgreen")

# grab summary data from google sheets - updated automatically in scripts 
URL <- "https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=1770424829#gid=1770424829"
# this contains data pulled from 2020 to the later half of 2023
summary_table <- read_sheet(URL, sheet = "summary_tables") %>%
  mutate(color_col = case_when(`Environmental Flag` == "Yes" ~ 1, 
                               TRUE ~ 0),
         enviro_flag = `Environmental Flag`, 
         `'21 - '25 Repos Per Million` = as.numeric(`'21 - '25 Repos Per Million`))

# build table! 
summary_table_reactable <- summary_table %>% 
  reactable(
    defaultColDef = colDef(
      align = 'center'),
    defaultSorted = "Agency", 
    columns = list(
      Agency = colDef(
        cell = color_tiles(., color_by = 'color_col', colors = c("#172f60", "#4ea324"))), 
      `'24 Workforce Tech Percentage` = colDef(
        style = color_scales(summary_table, colors = green_palette)),
      `'16 - '24 Tech Challenges` = colDef(
        style = color_scales(summary_table, colors = green_palette)),
      `'24 - '25 IT Jobs Posted` = colDef(
        style = color_scales(summary_table, colors = green_palette)),
      `'21 - '25 Repos Per Million` = colDef(
        style = color_scales(summary_table, colors = green_palette)),
      color_col = colDef(
        show = FALSE
      ),
    enviro_flag = colDef(
      show = FALSE
    )),
    highlight = TRUE,
    bordered = TRUE,
    resizable = TRUE,
    showSortable = TRUE,
    searchable = TRUE,
    defaultPageSize = 16,
    theme = reactableTheme(
      style = list(fontFamily = "-system-ui, -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")))

# having trouble getting this to format properly 
  # add_legend(
  #   data = summary_table,
  #   align = "left",
  #   title = 'Agency Type',
  #   col_name = 'enviro_flag',
  #   colors = c("#172f60", "#4ea324"))

summary_table_reactable


###############################################################################
# saving table as an html widget and putting it on aws: 
###############################################################################
# htmlwidgets::saveWidget((summary_table_reactable),
#                         "results/summary_table.html")
# 
# put_object(
#   file = file.path("results/summary_table.html"),
#   object = "/innovation-indicators/I2V2/summary_table.html",
#   acl = "public-read",
#   bucket = "tech-team-data"
# )

# updating our assets: 
# fig <- data.frame(figure = "Summary Table",
#                   s3_link = "s3://tech-team-data/innovation-indicators/I2V2/summary_table.html",
#                   public_link = "https://tech-team-data.s3.us-east-1.amazonaws.com/innovation-indicators/I2V2/summary_table.html")
# 

# range_write("https://docs.google.com/spreadsheets/d/1nGUFCxrHxb7B9sN6MXAn02qJOgnlFB9T8vnb_OfV33c/edit?gid=1922074841#gid=1922074841",
#             data = fig, range = "assets!A6:C6", col_names = FALSE)
