# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

#tuesdata <- tidytuesdayR::tt_load('2020-08-04')
#tuesdata <- tidytuesdayR::tt_load(2020, week = 32)

#energy_types <- tuesdata$energy_types

# Or read in the data manually


library(tidyverse)
library(gridExtra)

energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')

## explore the energy_types
dput(unique(energy_types$country))
dput(unique(energy_types$country_name))
dput(unique(energy_types$type))
dput(unique(energy_types$level))


## explore the country_totals
dput(unique(country_totals$country))
dput(unique(country_totals$country_name))
dput(unique(country_totals$type))
dput(unique(country_totals$level))




### Creating some ring plots 
#   https://www.azandisresearch.com/2019/07/19/create-a-radial-mirrored-barplot-with-ggplot/


import_totals = country_totals %>%
                filter(type == "Imports") %>% 
                rename(itype = type, i2016 = `2016`, i2017 = `2017`, i2018 = `2018`)
export_totals = country_totals %>%
                filter(type == "Exports") %>% 
                rename(etype = type, e2016 = `2016`, e2017 = `2017`, e2018 = `2018`)

I_E_data = left_join(import_totals, export_totals)



I_E_data = mutate(I_E_data, e2016.neg = -e2016)
I_E_data = mutate(I_E_data, e2017.neg = -e2017)
I_E_data = mutate(I_E_data, e2018.neg = -e2018)


I_E_data <- I_E_data %>% 
            mutate(importRank = rank(-i2016), exportRank = rank(-e2016)) %>% 
            mutate(SqRank = (importRank^2)+(exportRank^2)/2) %>% 
            mutate(RankOrder = rank(SqRank))

### working with labels:

p1 = ggplot(I_E_data, aes(x = reorder(country, RankOrder))) +
  geom_col(aes(y = i2016), fill = "#5d8402") +
  geom_text(aes(y = ifelse(i2016 >= 32000, 35000, (i2016 + 7000)), 
                color = ifelse(i2016 >= 32000, 'white', '#5d8402'), 
                label = round(i2016, 0)), size = 3)+
  geom_col(aes(y = e2016.neg), fill = "#817d79") +
  geom_text(aes(y = ifelse(e2016.neg >= -15000, -17000, ifelse(e2016.neg >= -12000, -10000, ifelse(e2016.neg >= -4000, -4000, (e2016.neg - 9000)))), 
                color = ifelse(e2016.neg <= -80000, 'white', '#817d79'), 
                label = round(e2016, 0)), size = 3)+
  #geom_text(aes(y = ifelse(e2016.neg <= -22000, -10000, (e2016.neg - 9000)), 
  #              color = ifelse(e2016.neg <= -22000, 'white', '#817d79'), 
  #              label = round(e2016, 0)), size = 3)
  geom_text(aes(y = ifelse(i2016 <= 2500 , 25000, i2016 + 21000), label = country)) +
  coord_polar() +
  scale_y_continuous(limits = c(-120000, 80000)) +
  scale_color_identity() +
  theme_void() +
  theme(plot.margin=grid::unit(c(-20,-20,-20,-20), "mm")) + 
  annotate("text", x=0, y=-110000, label= "2016",  color="Black", 
           size=7, fontface="bold" ) + 
  annotate("text", x=35.5, y=80000, label= "Energy Import vs. Export",  
           color="gray25", size=7 ) +  
  annotate("text", x=0.15, y=12000, label= "Import",  color='#5d8402', 
           size=4 , angle=85, fontface="bold" ) + 
  annotate("text", x=0, y=-30000, label= "Export",  color='#817d79', 
           size=4 , angle=85, fontface="bold" )  + 
  annotate("text", x=1, y=60000, label= "Country",  color="gray25", 
           size=4 , fontface="bold" )   
  # annotate("text", x=16, y=60000, label= "Data source: TidyTuesday, Eurostat",  
  #          color="gray25", size=4 ) 
#  annotate("text", x=16, y=70000, label= "Figures created: 8/3/2020, by @RRVdpool",  
#           color="gray25", size=3 )
p1


### 2017

p2 = ggplot(I_E_data, aes(x = reorder(country, RankOrder))) +
  geom_col(aes(y = i2017), fill = "#5d8402") +
  geom_text(aes(y = ifelse(i2017 >= 32000, 35000, (i2017 + 7000)), 
                color = ifelse(i2017 >= 32000, 'white', '#5d8402'), 
                label = round(i2017, 0)), size = 3)+
  geom_col(aes(y = e2017.neg), fill = "#817d79") +
  geom_text(aes(y = ifelse(e2017.neg >= -15000, -17000, ifelse(e2017.neg >= -12000, -10000, ifelse(e2017.neg >= -4000, -4000, (e2017.neg - 9000)))), 
                color = ifelse(e2017.neg <= -80000, 'white', '#817d79'), 
                label = round(e2017, 0)), size = 3)+
  geom_text(aes(y = ifelse(i2017 <= 2500 , 25000, i2017 + 21000), label = country)) +
  coord_polar() +
  scale_y_continuous(limits = c(-120000, 80000)) +
  scale_color_identity() +
  theme_void() +
  theme(plot.margin=grid::unit(c(-20,-20,-20,-20), "mm")) + 
  annotate("text", x=0, y=-110000, label= "2017",  color="Black", 
           size=7, fontface="bold" ) + 
  annotate("text", x=0.15, y=12000, label= "Import",  color='#5d8402', 
           size=4 , angle=85, fontface="bold" ) + 
  annotate("text", x=0, y=-30000, label= "Export",  color='#817d79', 
           size=4 , angle=85, fontface="bold" )  + 
  annotate("text", x=1, y=60000, label= "Country",  color="gray25", 
           size=4 , fontface="bold" ) 
  # annotate("text", x=16, y=60000, label= "Data source: TidyTuesday, Eurostat",  
  #          color="gray25", size=4 )  
  #annotate("text", x=16, y=70000, label= "Figures created: 8/3/2020, by @RRVdpool",  
  #         color="gray25", size=3 )

p2



### 2018


p3 = ggplot(I_E_data, aes(x = reorder(country, RankOrder))) +
  geom_col(aes(y = i2018), fill = "#5d8402") +
  geom_text(aes(y = ifelse(i2018 >= 32000, 35000, (i2018 + 7000)), 
                color = ifelse(i2018 >= 32000, 'white', '#5d8402'), 
                label = round(i2018, 0)), size = 3)+
  geom_col(aes(y = e2018.neg), fill = "#817d79") +
  geom_text(aes(y = ifelse(e2018.neg >= -15000, -17000, ifelse(e2018.neg >= -12000, -10000, ifelse(e2018.neg >= -4000, -4000, (e2018.neg - 9000)))), 
                color = ifelse(e2018.neg <= -80000, 'white', '#817d79'), 
                label = round(e2018, 0)), size = 3)+
  geom_text(aes(y = ifelse(i2018 <= 2500 , 25000, i2018 + 21000), label = country)) +
  coord_polar() +
  scale_y_continuous(limits = c(-120000, 80000)) +
  scale_color_identity() +
  theme_void() +
  theme(plot.margin=grid::unit(c(-20,-20,-20,-20), "mm")) + 
  annotate("text", x=0, y=-110000, label= "2018",  color="Black", 
           size=6, fontface="bold" ) + 
  annotate("text", x=0.15, y=12000, label= "Import",  color='#5d8402', 
           size=4 , angle=85, fontface="bold" ) + 
  annotate("text", x=0, y=-30000, label= "Export",  color='#817d79', 
           size=4 , angle=85, fontface="bold" )  + 
  annotate("text", x=1, y=60000, label= "Country",  color="gray25", 
           size=4 , fontface="bold" )  + 
  annotate("text", x=16, y=60000, label= "Data source: TidyTuesday, Eurostat",  
           color="gray25", size=4 )  + 
  annotate("text", x=16, y=70000, label= "Figures created: 8/3/2020, by @RRVdpool",  
           color="gray25", size=3 )



grid.arrange(p1, p2, p3, nrow = 1)
dev.copy(pdf, "Energy_IE.pdf", width = 20, height = 8.5)
dev.off()


