###############
# Challenge Analysis 
# EmmaLi Tsai, Nov 2023
##############

# libraries: 
library(tidyverse)
library(plotly)
library(RColorBrewer)

# reading in clean dataset: 
chal <- read.csv("./Data/Challenges_Nov24_FinalTidy_v2.csv") %>%
  select(-X)

# fixing use of the env_flag 
chal[chal$env_flag == "Environmental / Natural Resource Agencies", ]$env_flag <- "Environmental & Natural Resource Agencies"

# changing fonts: 
# library(showtext)
# font_add_google("Lato", "lato")
# showtext_auto()

# library(extrafont)
# font_import()
############
# Investigating duplicate data 
###########
chal_dupes <- chal %>%
  select(-X)

duplicated_chal <- chal_dupes[duplicated(chal_dupes),]
# 128 entries are duplicated (an unknown number of times)
unique(duplicated_chal$start_year) 
# the only duplicated entries are after 2020
investigate_dupes <- duplicated_chal %>%
  group_by(Challenge.ID, start_year, env_flag) %>%
  summarize(total = n())

# total number of duplicated challenges/year at env vs non env agencies 
plot_one <- duplicated_chal %>%
  group_by(start_year, env_flag) %>%
  summarize(count = n())  

# let's check this out using a graph: 
ggplot(chal, aes(x = start_year, y = prize_num, 
                            color = env_flag)) + 
  geom_jitter(stat = "identity") + 
  scale_color_manual(values = c("#7181bf", "#76bf71", "red"), 
                     name = "") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "Start Year", y = "Prize Amount") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_jitter(data = duplicated_chal, aes(x = start_year, y = prize_num, color = "red")) + 
  facet_wrap(~env_flag)

# how many challenges have 1 day in duration?
dur_check <- chal[which(chal$duration_days <= 1),]
# yikes - I gotta fix this 

# for duplicated challenges, which ones have different challenge types? 
chal_types <- chal %>%
  group_by(Challenge.ID) %>%
  summarize(chal_types = length(unique(Primary.Challenge.Type)))
# great - no duplicated challenges have different primary.challenge.types, which 
# makes things a bit easier. Basically they're just exact duplicates and can 
# be easily removed from the data 


# Figures for Challenges Blog - 12/20/2023 ####################################
# which departments  make the most use of them?
dept_all <- chal %>%
  group_by(env_flag, department) %>%
  summarize(count = n())  

# grabbing the top 20, but also trying to keep the entry for NOAA:
dept_all <- dept_all[order(dept_all$count, decreasing = TRUE),]
dept_mini <- dept_all[c(1:20, 26),]
dept_mini$department <- as.factor(dept_mini$department)
dept_breakdown <- ggplot(dept_mini, 
                         aes(x = reorder(department, -count, min), 
                             y = count, fill = env_flag)) + 
  ggchicklet::geom_chicklet() + 
  theme_bw() + 
  scale_fill_manual(values = c("#7181bf", "#76bf71"), 
                    name = "") + 
  theme(legend.position = "none") +
  coord_flip() + 
  labs(x = "", y = "Count") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title.x = element_text(size = 23),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 23), 
        axis.text.y = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        strip.text.x = element_text(size = 20))

dept_breakdown
ggsave("./results/dept_breakdown_R.png", dept_breakdown, dpi = 144)

# removing nominations and business plans, and regrouping the data for 
# visualization purposes: 
plot_two_v2 <- chal %>%
  subset(!(Primary.Challenge.Type %in% c("Nominations", "Business plans"))) %>%
  mutate(new_cat = case_when(
    Primary.Challenge.Type %in% c("Analytics, visualizations, algorithms", 
                                  "Software and apps", 
                                  "Technology demonstration and hardware") ~ "Analytics, Tech, Software", 
    Primary.Challenge.Type == "Creative (multimedia & design)" ~ "Creative", 
    TRUE ~ Primary.Challenge.Type
  )) %>%
  group_by(new_cat, start_year, env_flag) %>%
  summarize(total_counts = sum(count), 
            total_agencies = length(unique(department))) %>%
  mutate(chal_per_agency = total_counts/total_agencies)

# final plotting: 
plot_two_v2_graph <- ggplot(plot_two_v2) + 
  geom_line(aes(x = start_year, y = chal_per_agency, color = env_flag)) + 
  geom_smooth(aes(x = start_year, y = chal_per_agency, 
                  color = env_flag), method = "lm", alpha = 0.2, 
              se = F) + 
  geom_point(aes(x = start_year, y = chal_per_agency, color = env_flag), 
             size = 5) +
  facet_wrap(~new_cat) + 
  theme_bw() + 
  labs(x = "Start Year", y = "# Challenges per Agency") + 
  scale_color_manual(values = c("#7181bf", "#76bf71"), 
                     name = "") +
  theme(legend.position = "bottom") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title.x = element_text(size = 23),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 23), 
        axis.text.y = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        strip.text.x = element_text(size = 20))
plot_two_v2_graph
ggsave("./results/faceted_chaltype.png", plot_two_v2_graph, dpi = 144)


# What do these linear models look like?
sci <- plot_two_v2 %>%
  subset(new_cat == "Analytics, Tech, Software")
x <- lm(total_counts ~ start_year, data = sci %>% filter(env_flag == "All Other Agencies"))
x <- lm(total_counts ~ start_year, data = sci %>% filter(env_flag != "All Other Agencies")) 
summary(x)

# also need to workshop figure 1: 
# knocking out major departments for ~sensitivity analysis~ 
chal_p1 <- chal %>%
  filter(!(department %in% c("Department of Health and Human Services", 
                           "Department of Defense")))

# total number of challenges/year at env vs non env agencies 
plot_one <- chal_p1 %>%
  group_by(start_year, env_flag) %>%
  summarize(count = n())  

# plotting trend over the years for environmental and non-env: 
ggplot(plot_one, aes(x = start_year, y = count, color = env_flag)) + 
  geom_point(size = 5) + 
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("#7181bf", "#76bf71"), 
                     name = "") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "Start Year", y = "Count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

model_non_env <- lm(count ~ start_year, data = plot_one %>% filter(env_flag != "Environmental / Natural Resource Agencies"))
model_env <- lm(count ~ start_year, data = plot_one %>% filter(env_flag == "Environmental / Natural Resource Agencies"))
summary(model_non_env) # linear model summaries for number of new challenges/year
summary(model_env)
# interesting! so there is a clear difference when DOD and DHHS are removed, but 
# the overall trend is still the exact same. 

# what do the data look like as a heat map? 
chal_heatmap <- chal %>%
  group_by(start_year, department) %>%
  summarize(count = n()) 
chal_heatmap <- chal_heatmap[order(chal_heatmap$count, decreasing = TRUE),]
chal_heatmap_mini <- chal_heatmap[c(1:200),]
ggplot(chal_heatmap_mini, aes(x = start_year, 
                              y = reorder(department, -count), 
                              fill = count)) + 
  geom_tile() + 
  theme_bw() + 
  labs(x = "Year", y = "") + 
  scale_fill_distiller(palette= "YlGnBu", name = "# Challenges") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# looking at average challenges/agencies through time: 
chal_dept <- chal %>%
  group_by(start_year, department, env_flag) %>%
  summarize(count = n()) %>%
  group_by(start_year, env_flag) %>%
  summarize(total_chal = sum(count), 
            uniq_agencies = length(unique(department)))
chal_dept$mean_chal_agencies <- chal_dept$total_chal / chal_dept$uniq_agencies

fig1 <- ggplot(chal_dept, aes(x = start_year, y = mean_chal_agencies, color = env_flag)) + 
  geom_point(size = 5) + 
  geom_smooth(method = "lm", se = F) + 
  scale_color_manual(values = c("#7181bf", "#76bf71"), 
                     name = "") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "Year", y = "# Challenges per Agency") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title.x = element_text(size = 23),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 23), 
        axis.text.y = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        strip.text.x = element_text(size = 20))
fig1
ggsave("./results/chal_dept_year.png", fig1, dpi = 144)


model_non_env <- lm(mean_chal_agencies ~ start_year, 
                    data = chal_dept %>% 
                      filter(env_flag != "Environmental & Natural Resource Agencies"))
model_env <- lm(mean_chal_agencies ~ start_year, 
                data = chal_dept %>% 
                  filter(env_flag == "Environmental & Natural Resource Agencies")) 
summary(model_non_env) # 0.53
summary(model_env) # 0.19


# what if we remove dod and dhhs
chal_dept_filt <- chal %>%
  filter(!(department %in% c("Department of Health and Human Services", 
                             "Department of Defense"))) %>%
  group_by(start_year, department, env_flag) %>%
  summarize(count = n()) %>%
  group_by(start_year, env_flag) %>%
  summarize(total_chal = sum(count), 
            uniq_agencies = length(unique(department)))
chal_dept_filt$mean_chal_agencies <- chal_dept_filt$total_chal / chal_dept_filt$uniq_agencies

ggplot(chal_dept_filt, aes(x = start_year, y = mean_chal_agencies, color = env_flag)) + 
  geom_point(size = 4) + 
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("#7181bf", "#76bf71"), 
                     name = "") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "Year", y = "# Challenges per Agency") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

model_non_env <- lm(mean_chal_agencies ~ start_year, 
                    data = chal_dept_filt %>% 
                      filter(env_flag != "Environmental / Natural Resource Agencies"))
model_env <- lm(mean_chal_agencies ~ start_year, 
                data = chal_dept_filt %>% 
                  filter(env_flag == "Environmental / Natural Resource Agencies"))
summary(model_non_env) # 0.15
summary(model_env) # 0.09
# so overall trend is still the same - just weaker 


# challenge structures: 
# fixing dollar amounts: 
chal_summs <- chal %>%
  group_by(start_year, env_flag) %>%
  summarize('Average Prize Duration (days)' = mean(duration_days), 
            'Average Prize Amount ($)' = mean(prize_num))
chal_summs <- pivot_longer(chal_summs, 
                          3:4)
chal_structure <- ggplot(chal_summs, aes(x = start_year, 
                       y = value, 
                       color = env_flag)) + 
  geom_point(size = 5) + 
  geom_line(size = 1) + 
  scale_color_manual(values = c("#7181bf", "#76bf71"), 
                     name = "") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "Year", y = "Value") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 23),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 23), 
        axis.text.y = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        strip.text.x = element_text(size = 20)) + 
  facet_wrap(~name, scale = "free_y", ncol = 1) + 
  ggh4x::facetted_pos_scales(y = list(
    name == "Average Prize Amount ($)" ~ 
      scale_y_continuous(labels = scales::dollar_format())
  ))
chal_structure
ggsave("./results/chal_structures.png", chal_structure, dpi = 144)


# how many challenges don't offer prizes?
chal %>%
  group_by(env_flag) %>%
  summarize(no_prize = sum(prize_num == 0), 
            total = n()) %>%
  mutate(percentage = no_prize/total*100)
# all = 24.8%; env = 40.3% 


# want to creat a figure of number of agencies participating through time: 
chal_agencies <- chal %>%
  group_by(start_year, env_flag) %>%
  summarize(Agencies = length(unique(department)), 
            Challenges = n())
chal_agencies_long <- pivot_longer(chal_agencies, Agencies:Challenges)

ggplot(chal_agencies_long, aes(x = start_year, 
                   y = value, color = env_flag)) + 
  geom_point(size = 5) + 
  geom_smooth(method = "lm") + 
  geom_line(size = 1) + 
  scale_color_manual(values = c("#7181bf", "#76bf71"), 
                     name = "") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "Year", y = "Total") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 23),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 23), 
        axis.text.y = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        strip.text.x = element_text(size = 20)) + 
 facet_wrap(~name, scales = "free_y")
