library(tidyverse)
library(rvest)
library(here)
library(ggtext)
# library(ggExtra)
library(patchwork)

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

myurl <- 'https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-24/readme.md'

# download the taxon table by scraping 'myurl' 
# there are two tables on the page, so we select the first one ([[1]])
myhtml <- read_html(myurl)
taxon_table <- html_elements(myhtml, 'table')[[1]] %>% html_table()

# make column names lower-case, replace spaces with underscores
colnames(taxon_table) <- tolower(colnames(taxon_table)) %>% 
  gsub(' ', '_', .)

# taxon_table$taxon has CMEAD where lemurs$taxon has CMED, let's fix that
taxon_table$taxon[which(taxon_table$taxon == "CMEAD")] <- "CMED"
# add the common and Latin names to the lemurs table
joined <- left_join(lemurs, taxon_table, by="taxon")

joined$age_category <- factor(joined$age_category, 
                              levels = c("IJ","young_adult","adult"), ordered=TRUE)
# colorizing text: 
# ht https://stackoverflow.com/questions/17083362/colorize-parts-of-the-title-in-a-plot
p1 <- joined %>% 
  ggplot(mapping = aes(x = age_at_wt_y, y = weight_g, color=age_category)) + 
  geom_point(shape = '.', show.legend = FALSE, na.rm = TRUE) + 
  scale_color_manual(
    name = NULL,
    values = c(IJ = "#0072B2", young_adult = "#009E73", adult = "#D55E00")) +
  labs(x = "Age (yrs)", y = "Weight (g)") +
  theme_minimal() 
# ggExtra places histograms on the margins of the scatterplot... but the output 
# is incompatible with {patchwork}
# p1 <- ggExtra::ggMarginal(p1, groupFill = TRUE, groupColour = TRUE) 

# reordering boxblot by median:
# ht: https://rpubs.com/crazyhottommy/reorder-boxplot
p2 <- joined %>% 
  ggplot(mapping = aes(x = fct_reorder(common_name, weight_g, .fun=median), y = weight_g, 
                       color = age_category)) + 
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.5) +
  labs(x=NULL, y=NULL) +
  scale_color_manual(
    name = NULL,
    values = c(IJ = "#0072B2", young_adult = "#009E73", adult = "#D55E00")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

pfinal <- (p1 | p2) + 
  plot_annotation(
    title = "Weight distribution of <span style='color:#0072B2;'><strong>infant/juvenile</strong></span>, <span style='color:#009E73;'><strong>young adult</strong></span>, and <span style='color:#D55E00;'><strong>adult</strong></span> lemurs at the <strong>Duke Lemur Center</strong>",
    caption = "Source: DOI:10.1038/sdata.2014.19 (2014)\n#tidytuesday | @j_perkel | 2021-08-24",
    theme = theme(plot.title = element_markdown(lineheight = 1.1))
  )

ggsave(here("2021-08-24-lemurs.jpg"), plot = pfinal, bg = "white", width = 10, 
       height = 6, dpi = 300)
