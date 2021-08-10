library(tidyverse)
library(tidytuesdayR)
library(here)

ttdata <- tt_load('2021-08-10')
inv <- ttdata$investment
inv$year <- as.Date(ISOdate(inv$year, 1, 1))

# source: https://history.house.gov/Institution/Party-Divisions/Party-Divisions/
# congress <- read.csv(textConnection(
#   "year1,year2,party
#   1947,1949,R
#   1949,1951,D
#   1951,1953,D
#   1953,1955,R
#   1955,1957,D
#   1957,1959,D
#   1959,1961,D
#   1961,1963,D
#   1963,1965,D
#   1965,1967,D
#   1967,1969,D
#   1969,1971,D
#   1971,1973,D
#   1973,1975,D
#   1975,1977,D
#   1977,1979,D
#   1979,1981,D
#   1981,1983,D
#   1983,1985,D
#   1985,1987,D
#   1987,1989,D
#   1989,1991,D
#   1991,1993,D
#   1993,1995,D
#   1995,1997,R
#   1997,1999,R
#   1999,2001,R
#   2001,2003,R
#   2003,2005,R
#   2005,2007,R
#   2007,2009,D
#   2009,2011,D
#   2011,2013,R
#   2013,2015,R
#   2015,2017,R
#   2017,2019,R
#   2019,2021,D
#   2021,2023,D"
#   ))
# congress$party <- factor(congress$party)
# congress$year1 <- as.Date(ISOdate(congress$year1, 1, 1))
# congress$year2 <- as.Date(ISOdate(congress$year2, 1, 1))


presidents <- read.csv(textConnection(
  "year1,year2,party,name
  1945,1953,D,Truman
  1953,1961,R,Eisenhower
  1961,1963,D,Kennedy
  1963,1969,D,Johnson
  1969,1974,R,Nixon
  1974,1977,R,Ford
  1977,1981,D,Carter
  1981,1989,R,Reagan
  1989,1993,R,Bush
  1993,2001,D,Clinton
  2001,2009,R,Bush
  2009,2017,D,Obama"
))
presidents$party <- factor(presidents$party)
presidents$year1 <- as.Date(ISOdate(presidents$year1, 1, 1))
presidents$year2 <- as.Date(ISOdate(presidents$year2, 1, 1))

mydata <- inv %>% 
  filter(gross_inv > 0) %>%
  group_by(meta_cat, year) %>% 
  summarize(total = sum(gross_inv)) # %>%

# gross_inv in millions of USD, so divide by 1000 for billions
ggplot() + 
  theme_minimal() +
  geom_point(data=mydata, 
             mapping=aes(x = year, y = total/1000, color = meta_cat, group = meta_cat)) + 
  geom_line(data=mydata, 
            mapping=aes(x = year, y = total/1000, color = meta_cat, group = meta_cat)) +
  geom_rect(data=presidents, 
            mapping=aes(xmin=year1, xmax=year2, ymin=-Inf, ymax=Inf),
            fill=ifelse(presidents$party == "D", "blue","red"),
            alpha=0.3, color = "black") + 
  geom_text(data=presidents,
            mapping=aes(x=year1+(year2-year1)/2, y = 500, label=name),
            size=3, hjust = 1, check_overlap = TRUE, angle = 90) +
  labs(x = NULL, y = "Investment ($USD billions)",
       title = "US infrastructure spending, 1947-2016",
       caption = "#tidytuesday | 2021-08-10 | @j_perkel") + 
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(here("2021-08-10-infrastructure.jpg"), bg = "white", width = 8, height = 8, dpi = 300)
