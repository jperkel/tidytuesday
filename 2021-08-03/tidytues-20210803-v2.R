library(tidyverse)
library(tidytuesdayR)
library(here)

ttdata <- tt_load("2021-08-03")

# this version iterates over each class of event
# and uses country code (abb field) instead of name, as many country names are NA
for (mytype in unique(ttdata$athletes$type)) {
  if (mytype == "Triathlon") next # no data for this event
  t <- ttdata$athletes %>% filter(type == mytype)
  t$medal <- factor(t$medal, levels = c("Bronze","Silver","Gold"), ordered = TRUE)
  
  t %>% group_by(abb, medal) %>%
    filter(!is.na(abb)) %>%
    summarize(count = n()) %>%
    mutate(total = sum(count)) %>%
    filter(total > 5) %>%
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
  
  ggsave(here(paste0("2021-08-03-", gsub(' ', '_', mytype), "-medals.jpg")), bg = "white", width = 8, height = 8, dpi = 300)
}
