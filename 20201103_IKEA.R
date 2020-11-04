### TIdy Tuesday: IKEA
##  Rebecca Vanderpool
##  vanderpoolrr@arizona.edu
##  2020-11-3

library(sysfonts)
library(tidyverse)
library(ggpubr)
library(scales)

font_add_google("Noto Sans", "noto")
font_add_google("Noto Bold")

ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

ikea = ikea %>% 
  mutate(usd = price*0.27)

### ordering the categories 
grouped_cat = ikea %>% 
  group_by(category) %>% 
  summarize(average_cost = mean(price), n()) %>% 
  arrange(desc(average_cost))


  ggbarplot(grouped_cat, x = "category", y = "average_cost", 
           color = "category", fill = "category", 
           sort.val = "desc",
           legend = "none", 
           x.text.angle = 90)
  
  
  ggerrorplot(ikea, x = "category", y = "usd", 
              desc_stat = "mean_sd", 
            #color = "category",  
            color = "#0051ba",
             
            sort.val = "desc",
            add = "violin", 
            add.params = list(color = "#ffda1a", fill = "#ffda1a"),
            orientation = "horizontal", 
            xlab = "", 
            legend = "none", 
            order = rev(grouped_cat$category), 
            
            
            #x.text.angle = 90
            ) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", size = 0.4) +
    labs(x = NULL, 
         y = "Mean Price (in USD)", 
         title = "IKEA Prices on Saudi Arabian Website", 
         subtitle = "Average price comparison based on furniture categories: ", 
         caption = "1 USD ~ 3.7 SAR, Data source: Kaggle, TidyTuesday | Graphic: @RRVdpool") + 
    theme(text = element_text( family = "Noto Sans"),
          axis.text.y = element_text(size = 10), 
          plot.caption = element_text(face = "italic"), 
          plot.title.position = "plot",
          plot.title = element_text(face = "bold", size = 18)) + 
    scale_y_continuous(labels = scales::dollar_format())
  
dev.copy(png, "20201103_TidyTuesday_IKEA.png", 7,5, units = "in", res = 300)
dev.off()
  

