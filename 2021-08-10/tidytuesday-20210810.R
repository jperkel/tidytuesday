library(tidyverse)
library(tidytuesdayR)
library(here)

ttdata <- tt_load('2021-08-10')
inv <- ttdata$investment
inv$year <- as.Date(ISOdate(inv$year, 1, 1))

# use the 'presidential' dataset to overlay info on who's in the White House over time
# 'presidential' begins with Eisenhauer, but the tidytuesday dataset starts with Truman
presidential <- rbind(presidential, 
                      data.frame(name="Truman", start="1945-04-12", end="1953-01-20", 
                                 party="Democratic"))

# summarize data by totaling all spending in each meta_cat group
mydata <- inv %>% 
  filter(gross_inv > 0) %>%
  group_by(meta_cat, year) %>% 
  summarize(total = sum(gross_inv)) %>%
  ggplot() + 
  theme_minimal() +
  # gross_inv in millions of USD, so divide by 1000 for billions
  geom_point(mapping=aes(x = year, y = total/1000, color = meta_cat, group = meta_cat)) + 
  geom_line(mapping=aes(x = year, y = total/1000, color = meta_cat, group = meta_cat)) +
  geom_rect(data=presidential, 
            mapping=aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
            fill=ifelse(presidential$party == "Democratic", "blue","red"),
            alpha=0.2, color = "grey50") + 
  geom_text(data=presidential,
            mapping=aes(x=start+(end-start)/2, y = 750, label=name),
            size=3, hjust = 1, check_overlap = TRUE, angle = 90) +
  labs(x = NULL, y = "Investment ($USD billions)",
       title = "US infrastructure spending, 1947-2016",
       caption = "#tidytuesday | 2021-08-10 | @j_perkel") + 
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

ggsave(here("2021-08-10-infrastructure.jpg"), bg = "white", width = 8, height = 8, dpi = 300)
