# TidyTuesday

A repository for contributions to the Tidy Tuesday intiative: [https://github.com/rfordatascience/tidytuesday](https://github.com/rfordatascience/tidytuesday). 

## [2020-11-03 - Data: IKEA](https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-11-03/readme.md)

This week it was about exploring prices of items on IKEA Saudi Arabian website that was collected using web scraping and deposited on kaggle. Did the typical data exploration of the average price in each of the furniature categories. Changed the font to the IKEA font "Noto Sans" and the colors to the [IKEA colors](https://brandpalettes.com/ikea-logo-color-codes). 

 Some initial exploration using networks but then ended with some faceted plots. Color is always an interesting thing to play around with to assist with communication of the data. [R code to generate figure](/20201103_IKEA.R)

![Average price in each furniture category](/20201103_TidyTuesday_IKEA.png "Average prices")


## [2020-08-25 - Data: Chopped](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-08-25/readme.md)

This week* it was about exploring data from the TV show Chopped. [R code to generate figure](/20200825_Chopped_clean.R) 

![Ratings on the Chopped TV show](/20200831_Chopped.png "Ratings on the Chopped TV show")


* missed posting the analysis and figure at the time (uploaded 12/9/2020)


## [2020-08-19 - Data: Plants in Danger](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-08-18/readme.md)

This week it was about exploring the extinct plants dataset. Some initial exploration using networks but then ended with some faceted plots. Color is always an interesting thing to play around with to assist with communication of the data. [R code to generate figure](/2020818_ExtinctPlants_clean.R)

![Threats and Actions taken in the extinct plants dataset](/20200818_ExtinctPlants_color.png "Threats and Actions taken in the extinct plants dataset")

Same figure but using some of the standard color palettes. It is more difficult to ignore the part of the graph for the unknown threats/actions. The use of dark gray in Panel B gives too much attention to the unknown actions. 
![Threats and Actions taken in the extinct plants dataset - standard palettes](/20200818_ExtinctPlants.png "Threats and Actions taken in the extinct plants datasetr - standard palettes")


## [2020-08-11 - Data: Avatar: The last airbender](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-08-11/readme.md)

This week it was about exploring the ggpubr package and cowplot to make easier publication quality figures. [R code to generate figure](/20200810_AvatarLastAirbender_clean.R)

![Factors contributing to IMDB ratings for Avatar: The last airbender](/20200811_Avatar_airbender.png "Factors contributing to IMDB ratings for Avatar: The last airbender")



## [2020-08-04 - Data: European Electricity by EU_Eurostat](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-08-04/readme.md)

Worked with making some radial barcharts comparing the imports and exports of energy. [R code to generate figure](/20200803_Energy_clean.R)

Plot design modified from [AZA Arietta's tutorial](https://www.azandisresearch.com/2019/07/19/create-a-radial-mirrored-barplot-with-ggplot/)

![European Energy plots for imports and exports based on country for 2016, 2017 and 2018](/20200803_Energy_plot.png "European Import vs. Export Energy")



