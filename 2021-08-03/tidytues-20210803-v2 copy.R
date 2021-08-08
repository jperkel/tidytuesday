library(tidyverse)
library(tidytuesdayR)

ttdata <- tt_load("2021-08-03")

# iterate over each type of event...
for (mytype in unique(ttdata$athletes$type)) {
  if (mytype == "Triathlon") next # no data for this event
  t <- ttdata$athletes %>% filter(type == mytype)
  t$medal <- factor(t$medal, levels = c("Bronze","Silver","Gold"), ordered = TRUE)
  
  t %>% group_by(abb, medal) %>%
    filter(!is.na(abb)) %>%
    summarize(count = n()) %>%
    # this variation sorts by # of gold medals
    #  mutate(total = sum(count), n_gold = sum(count[medal == "Gold"])) %>%
    mutate(total = sum(count)) %>%
    filter(total > 5) %>%
    #  ggplot(aes(x = reorder(abb, n_gold), y = count, fill = medal)) +
    ggplot(aes(x = reorder(abb, total), y = count, fill = medal)) +
    geom_col(color = "black") + 
    scale_fill_manual(values = c("#E69F00","#999999","gold")) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "top", legend.title = element_blank()) +
    labs(x = "country", y = "medals", 
         title = paste0("Paralympics ", mytype, " medals, ", 
                        min(t$year, na.rm = TRUE), " -- ", 
                        max(t$year, na.rm = TRUE)),
         caption = "#tidytuesday | 2021-08-03 | @j_perkel")
  
  ggsave(paste0("2021-08-03-", gsub(' ', '_', mytype), "-medals.jpg"), bg = "white", width = 8, height = 8, dpi = 300)
}


archery <- ttdata$athletes %>% filter(type == "Archery")

archery$medal <- factor(archery$medal, levels = c("Bronze","Silver","Gold"), ordered = TRUE)

archery %>% group_by(abb, medal) %>%
  filter(!is.na(abb)) %>%
  summarize(count = n()) %>%
  # this variation sorts by # of gold medals
#  mutate(total = sum(count), n_gold = sum(count[medal == "Gold"])) %>%
  mutate(total = sum(count)) %>%
#  ggplot(aes(x = reorder(abb, n_gold), y = count, fill = medal)) +
  ggplot(aes(x = reorder(abb, total), y = count, fill = medal)) +
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

  ggsave("2021-08-03-paralympics_medals.jpg", bg = "white", width = 8, height = 8, dpi = 300)