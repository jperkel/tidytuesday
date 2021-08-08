library(tidyverse)
library(tidytuesdayR)
library(here)

ttdata <- tt_load("2021-08-03")
archery <- ttdata$athletes %>% filter(type == "Archery")

archery$medal <- factor(archery$medal, levels = c("Bronze","Silver","Gold"), ordered = TRUE)

archery %>% group_by(country, medal) %>%
  filter(!is.na(country), country != "-") %>%
  summarize(count = n()) %>%
  mutate(total = sum(count),
         country = case_when(
           country == "United States of America" ~ "United States",
           country == "Czech Republic" ~ "Czechia",
           country == "Korea" ~ "South Korea",
           country == "Great Britain" ~ "United Kingdom",
           TRUE ~ country
         )) %>%
  ggplot(aes(x = reorder(country, total), y = count, fill = medal)) +
  geom_col(color = "black") + 
  scale_fill_manual(values = c("#E69F00","#999999","gold")) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank()) +
  labs(x = "country", y = "medals", 
       title = paste0("Paralympics archery medals, ", 
                      min(archery$year, na.rm = TRUE), " -- ", 
                      max(archery$year, na.rm = TRUE)),
       caption = "#tidytuesday | 2021-08-03 | @j_perkel")

  ggsave(here("2021-08-03-paralympics_medals.jpg"), bg = "white", width = 8, height = 8, dpi = 300)