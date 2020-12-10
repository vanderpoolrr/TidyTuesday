### TidyTuesday 8/24/2020 -- Chopped
## Rebecca R. Vanderpool
## vanderpoolrr@email.arizona.edu
## 2020-08-24

#install.packages("stringdist")
#install.packages("ggsci")
#install.packages("gridGraphics")
library(tidyverse)
library(stringdist)
library(ggpubr)
library(viridis)
library(ggsci)
library(cowplot)
library(gridGraphics)

# Get the Data
# Read in the data manually

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')
## change season to a factor

chopped = chopped %>%
  mutate(season_group = case_when(
    season <=15 ~ 1, 
    season >15 & season <=30 ~ 2,
    season > 30 ~ 3), 
    ) 
chopped$season = factor(chopped$season)


### exploring the judges and the number of judges
judges = c(unique(chopped$judge1), unique(chopped$judge2), unique(chopped$judge3))

judges[order(judges)]
unique(judges[order(judges)])


############### Identified a number of errors with judges names. 
### Correct Aarón Sánchez' name in the judges columns
chopped$judge1[chopped$judge1 == "Aarón Sanchez"] = "Aarón Sánchez"
chopped$judge2[chopped$judge2 == "Aarón Sanchez"] = "Aarón Sánchez"
chopped$judge3[chopped$judge3 == "Aarón Sanchez"] = "Aarón Sánchez"

### Correct Jody Williams name in the judges columns
chopped$judge1[chopped$judge1 == "Jody William"] = "Jody Williams"
chopped$judge2[chopped$judge2 == "Jody William"] = "Jody Williams"
chopped$judge3[chopped$judge3 == "Jody William"] = "Jody Williams"

### Correct Amanda Freitag name in the judges columns
chopped$judge1[chopped$judge1 == "Amanda Freita" | chopped$judge1 == "Amanda Frietag"] = "Amanda Freitag"
chopped$judge2[chopped$judge2 == "Amanda Freita" | chopped$judge2 == "Amanda Frietag"] = "Amanda Freitag"
chopped$judge3[chopped$judge3 == "Amanda Freita" | chopped$judge3 == "Amanda Frietag"] = "Amanda Freitag"

### Correct Geoffrey Zakarian name in the judges columns
chopped$judge1[chopped$judge1 == "Geoffrey Zacharian"] = "Geoffrey Zakarian"
chopped$judge2[chopped$judge2 == "Geoffrey Zacharian"] = "Geoffrey Zakarian"
chopped$judge3[chopped$judge3 == "Geoffrey Zacharian"] = "Geoffrey Zakarian"

### Correct Maneet Chauhan name in the judges columns
chopped$judge1[chopped$judge1 == "Maneet Chauhaun"] = "Maneet Chauhan"
chopped$judge2[chopped$judge2 == "Maneet Chauhaun"] = "Maneet Chauhan"
chopped$judge3[chopped$judge3 == "Maneet Chauhaun"] = "Maneet Chauhan"

### Correct Chris Santos name in the judges columns
chopped$judge1[chopped$judge1 == "Chris Santo"] = "Chris Santos"
chopped$judge2[chopped$judge2 == "Chris Santo"] = "Chris Santos"
chopped$judge3[chopped$judge3 == "Chris Santo"] = "Chris Santos"


## Exploring the highs and the low rated shows. 
hist(chopped$episode_rating)
sum(chopped$episode_rating >9, na.rm = TRUE)
sum(chopped$episode_rating <7.2, na.rm = TRUE)
summary(chopped$episode_rating)


top_rated = which(chopped$episode_rating >9)
low_rated = which(chopped$episode_rating < 7.2)

View(chopped[top_rated, ])
View(chopped[low_rated, ])


### Figure of the episode ratings based on season. 
#   Which season has the highest/lowest ratings. 

#col_pal = rep(viridis(15), 3)
col_pal = inferno(45)

p = ggdensity(chopped, x = "episode_rating", 
                    # add = "mean", 
                    legend = "right", 
                    palette = col_pal,
                    color = "season", 
                    fill = "season",  
              xlab = "Episode Rating"
                    #rug = TRUE
          )

P2 = facet(p, 
      facet.by = "season_group", 
      ncol = 1, 
      panel.labs = list(season_group = c("Seasons 1-15", "Seasons 16-30", "Seasons 31-43")), 
      panel.labs.background = list(color = "Black", fill = "white", linetype = 0, size = 0)) +
      theme(legend.title = element_text(size = 10), 
            legend.text  = element_text(size = 9),
            legend.key.size = unit(0.2, "lines"))+
  labs(caption = "")



#### create a figure of the average season ratings to identify the seasons with the highest and lowest ratings. 
average_season = chopped %>%
  select("season", "season_episode", "episode_rating" ) %>%
  group_by(season) %>%
  summarize(mean_rating = mean(episode_rating, na.rm = TRUE), n = n())

## Exploration figure -- looks like season 33 took a big downturn... 
plot(average_season$season, average_season$mean_rating)


### Plot the deviation fo the values from the reference value
##  will work with the z-score as in the examples. 

average_season = average_season %>%
                    mutate(rating_z = (mean_rating - mean(mean_rating, na.rm = TRUE))/sd(mean_rating, na.rm = TRUE), 
                           rating_grp = factor(ifelse(rating_z < 0, "low", "high"), levels = c("low", "high")))


col_journal = rev(pal_jama()(2))

P3 = ggdotchart(average_season, x = "season", y = "rating_z",
           color = "rating_grp",                                # Color by groups
           palette = col_journal,        # Custom color palette
           sorting = "ascending",                               # Sort value in descending order
           add = "segments",                                    # Add segments from y = 0 to dots
           add.params = list(color = "lightgray", size = 2),    # Change segment color and size
           group = "rating_grp",                                # Order by groups
           #rotate = TRUE, 
           dot.size = 7,                                        # Large dot size
           label = round(average_season$mean_rating,1),                        # Add mean ratings values as dot labels
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           legend = c(0.85, 0.3), 
           legend.title = "Ratings", 
           ylab = c("Z score"), 
           xlab = c("Season"),
           ggtheme = theme_pubr()                        # ggplot2 theme
) +
  #geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
  geom_vline(xintercept = 19.5, linetype = 2, color = "gray25") +
  geom_vline(xintercept = 43.5, linetype = 2, color = "gray25")

## looking at the highest rated and the lowest rated seasons. 
#chopped %>%
#  filter(season == 33 | season == 32 | season == 1 | season == 40) %>%
#  View()

### normalize the judges rating to the overall ratings
overall_mean = mean(average_season$mean_rating, na.rm = TRUE)
overall_sd = sd(average_season$mean_rating, na.rm = TRUE)


judges_df_long <- chopped %>%
  select( "season", "season_episode", "series_episode", "episode_rating", "judge1", 
          "judge2", "judge3", "appetizer") %>% 
  gather(judge_pos, judge_name, judge1:judge3)  %>% 
  arrange(series_episode) 

judge_fig = judges_df_long %>% 
  add_count(judge_name, sort = TRUE) %>%
  group_by(judge_name) %>%
  summarize(mean_rating = mean(episode_rating, na.rm = TRUE), 
            sd_rating = sd(episode_rating, na.rm = TRUE),
            n = n()) %>%
  filter(n >= 5) %>%
  mutate(appearance = ifelse(n > 20, "Regular", "Guest"), 
         judge_rating_z = (mean_rating - overall_mean)/overall_sd, 
         judge_rating_grp = factor(ifelse(judge_rating_z < 0, "low", "high"), levels = c("low", "high")))


col_journal = pal_lancet()(3)

P1 = ggdotchart(judge_fig, x = "judge_name", y = "judge_rating_z",
           color = "appearance",                                # Color by groups
           palette = col_journal,        # Custom color palette
           sorting = "ascending",                               # Sort value in descending order
           add = "segments",                                    # Add segments from y = 0 to dots
           add.params = list(color = "lightgray", size = 2),    # Change segment color and size
           group = "appearance",                                # Order by groups
           rotate = TRUE, 
           dot.size = 7,                                        # Large dot size
           label = round(judge_fig$mean_rating,1),                        # Add mean rating values as dot labels
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           legend = c(0.8, 0.9), 
           legend.title = "Appearances", 
           ylab = c("Z Score"), 
           xlab = c(""),
           ggtheme = theme_pubr()                        # ggplot2 theme
) +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
  #geom_vline(xintercept = 9.5, linetype = 2, color = "gray25") +
  geom_vline(xintercept = 7.5, linetype = 2, color = "gray25")+
  labs(caption = "TidyTuesday: Chopped, Analysis @RRVdpool, Created: 8/31/2020")
  #labs(caption = "Ordered based on their Z-score based on episode ratings.")

## Lay out the figure
#plot_row = plot_grid(P1, P2,  labels = c('A)', 'B)' ), ncol = 2,  label_size = 14)
#plot_combine = plot_grid(plot_row, P3, labels = c('', 'C)'),  label_size = 14, ncol = 1)


plot_row = plot_grid(P2, P1,  labels = c('B)', 'C)' ), ncol = 2, align = 'v', axis = '1', label_size = 14)
plot_combine = plot_grid(P3, plot_row,  labels = c('A)', ''),  label_size = 14, ncol = 1)



# now add the title
title <- ggdraw() + 
  draw_label(
    "Chopped: Episode Ratings for Frequent Judges and Seasons",
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
  title, plot_combine,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

dev.copy(png, "20200831_Chopped.png", width = 9, height = 8.5, units = "in", res = 300)
dev.off()






