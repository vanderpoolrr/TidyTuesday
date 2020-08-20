## Tidy Tuesday: Extinct Plants
#  Rebecca Vanderpool
#  vanderpoolrr@email.arizona.edu
#  Working with graph analysis and creating different figures 
#  for network graphs https://rviews.rstudio.com/2019/03/06/intro-to-graph-analysis/

#install.packages("tidygraph")

library(tidyverse)
library(cowplot)

# Get the Data
# Read in the data manually

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')


## There are 500 plants 
## There are actions and threats

unique(actions$action_type)
unique(threats$threat_type)
unique(threats$group)

### based on the different groups... 


summary(plants)

## practice figure
threats %>%
  group_by(group, continent) %>%
  count(threat_type, threatened, sort = TRUE) %>%
  filter(threatened == TRUE)  %>%

  ggplot(aes(x= threat_type, y=n, color = group, group = continent)) +
  geom_point() +
  facet_grid(~ continent)


## facet plot based on group and continent
P1 = threats %>%
  group_by(group, continent) %>%
  count(threat_type, threatened, sort = TRUE) %>%
  filter(threatened == TRUE)  %>%

  ggplot(aes(y = group, weight = n)) +
  #geom_bar(aes(fill = threat_type), position = position_stack(reverse = TRUE)) +
  geom_bar(aes(fill = threat_type), position = "fill") +
  theme(legend.position = "top")+ 
  #scale_fill_brewer(palette="Paired", name = c("Threat Type")) +
  scale_fill_manual(values = c("forestgreen", "darksalmon", "dodgerblue", "bisque4", "firebrick", "peachpuff", "plum", 
                                "gold", "royalblue4" , "lightsteelblue" , "darkseagreen" , "gray90"), name = c("Action Type")) +
  facet_grid(~ continent)+ 
  scale_x_continuous(name ="Threat Type (%)",
                     breaks = seq(0, 1, 0.25), 
                     labels = c("0", "", "50%", "", "100%"))+ 
  theme_classic()+
    theme(
      panel.grid.major = element_line(colour = "gray75", linetype = "dashed"), 
      strip.text.x = element_text( face = "bold"),  
      legend.text=element_text(size=rel(0.7)),
      axis.title.y=element_blank()
      )+
  labs(title = "Threat types based on continent and type of plant.")

P2 = actions %>%
  group_by(group, continent) %>%
  count(action_type, action_taken, sort = TRUE) %>%
  filter(action_taken == TRUE)  %>%
  
  ggplot(aes(y = group, weight = n)) +
  #geom_bar(aes(fill = threat_type), position = position_stack(reverse = TRUE)) +
  geom_bar(aes(fill = action_type), position = "fill") +
  theme(legend.position = "top")+ 
  #scale_fill_brewer(palette="RdGy", name = c("Action Type")) +
  scale_fill_manual(values = c("firebrick", "royalblue", "lightsalmon" , "lightsteelblue" , "darkseagreen" , "gray90"), name = c("Action Type")) +
  facet_grid(~ continent)+ 
  scale_x_continuous(name ="Action Type (%)",
                     breaks = seq(0, 1, 0.25), 
                     labels = c("0", "", "50%", "", "100%"))+
  theme_classic()+
  theme(
    panel.grid.major = element_line(colour = "gray75", linetype = "dashed"), 
    strip.text.x = element_text( face = "bold"), 
    axis.title.y=element_blank(), 
    legend.text=element_text(size=rel(0.7)), 
    plot.caption.position = "plot"
  )+
  labs(title = "Action types based on continent and type of plant.",
       caption = "TidyTuesday: Extinct Plants, Analysis @RRVdpool, Created: 8/18/2020")


## Lay out the figure
plot_row <- plot_grid(P1, P2,  labels = c('A)', 'B)' ), ncol = 1,  label_size = 12)


# now add the title
title <- ggdraw() + 
  draw_label(
    "Percentage of threats against and actions taken in extinct plants separated by continent and plant type",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

dev.copy(png, "20200818_ExtinctPlants_color_save.png", width = 1200, height = 700)
dev.off()




