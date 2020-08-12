## Week 33 of TidyTuesday: Avatar: The Last Airbender
## 
## Analysis theme this week is to use ggpubr to work with publication 
## quality figures: https://rpkgs.datanovia.com/ggpubr/index.html
## and Cowplot for arranging them: https://wilkelab.org/cowplot/articles/plot_grid.html
## 
## Rebecca R. Vanderpool
## vanderpoolrr@email.arizona.edu
## 2020-08-10

# Libraries: 
#install.packages("tvthemes")
#install.packages("ggpubr")
#install.packages("extrafont")
#install.packages("cowplot")
#font_import()


library(tidyverse)
library(tvthemes)
library(extrafont)
library(ggpubr)
library(dplyr)
library(cowplot)
library(stringr)

#> Registering fonts with R
#loadfonts(quiet = TRUE)


# Get the Data
# read in the data manually

avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
#scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')

## adding in the number of words per interaction: 
avatar$word_count = str_count(avatar$character_words, '\\w+')


avatar_data = avatar[, c("id", "book", "book_num", "chapter", "chapter_num", "character", 
                           "writer", "director", "imdb_rating", "word_count")]

#summary(avatar$imdb_rating[avatar$director == "Michael Dante DiMartino"])
# Michael Dante DiMartino only had one episode

dat_ratings <- avatar_data %>%
  distinct(book, chapter_num, chapter, director, imdb_rating)

dat_ratings$book = factor(dat_ratings$book, levels = c("Water",  "Earth", "Fire"))


### Boxplot of IMDB ratings as a function of book
p <- ggboxplot(dat_ratings, x = "book", y = "imdb_rating",
               color = "black", fill = "book", 
               palette = c("#0047AB","#4c7022", "#a10000"), alpha = 0.5,
               ylab = c("IMDB ratings"),
               add = "jitter", shape = "book", legend = "none") +
        labs(title = "Are IMDB ratings different by book?", 
             x = c("")) + 
        theme(plot.title = element_text(size = 12))
#+  theme_avatar()

### the order of the comparisons in the list are the order in which they are added
my_comparisons <- list( c("Water", "Earth"), 
                        c("Earth", "Fire"), 
                        c("Water", "Fire"))


p1_plot = p + stat_compare_means(comparisons = my_comparisons, aes(label = ..p.signif..))+
  stat_compare_means(label.y = 12) 


# Violin plots with box plots inside
# :::::::::::::::::::::::::::::::::::::::::::::::::::
# Change fill color by groups: director
# add boxplot with white fill color

p2 = ggviolin(dat_ratings, x = "director", y = "imdb_rating", fill = "director",
                # palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "boxplot", add.params = list(fill = "white")) +
  scale_fill_avatar(palette = "FireNation") +
  theme_avatar()
#  facet_wrap(~ book)

p2 = ggviolin(dat_ratings, x = "director", y = "imdb_rating", fill = "director",
              ylab = "IMDB ratings", alpha = 0.4, font.label = list(size = 8, color = "black"),
              legend = 'none', 
              add = "boxplot", add.params = list(fill = "white"), x.text.angle = 20) +
              scale_fill_avatar(palette = "FireNation") +
       labs(title = "Are IMDB ratings a function of directors?", 
            x = c("")) + 
       theme(plot.title = element_text(size = 12))



my_comparisons <- list( c("Dave Filoni", "Lauren MacMullan"),
                        #c("Lauren MacMullan", "Giancarlo Volpe"),
                        #c("Giancarlo Volpe", "Ethan Spaulding"),                        
                        #c("Lauren MacMullan", "Ethan Spaulding"),
                        #c("Ethan Spaulding", "Joaquim Dos Santos"), 
                        #c("Dave Filoni", "Giancarlo Volpe"),
                        #c("Giancarlo Volpe", "Joaquim Dos Santos"),                        
                        c("Dave Filoni", "Ethan Spaulding"), 
                        #c("Lauren MacMullan", "Joaquim Dos Santos"),
                        c("Dave Filoni", "Joaquim Dos Santos"))


p2_plot = p2 + stat_compare_means(comparisons = my_comparisons,  label.y = c(10.60, 11.00, 11.40))+ # Add significance levels
  stat_compare_means(label.y = 12)   
p2_plot

#label = "p.signif", - argument to add to change from p-value to symbols

table(dat_ratings$director)  
  

  

## Imdb ratings as a function of Chapter/Episode
p3 = ggscatter(dat_ratings, x = "chapter_num", y = "imdb_rating",
               size = 0.9,
               add = "reg.line",                         # Add regression line
               conf.int = TRUE,  
               color = "book",                           # Color by groups "book"
               palette = c("#0047AB","#4c7022", "#a10000"),           
               shape = "book",
               legend = "none", 
               xlab = "Chapter Number", 
               ylab = "IMDB ratings")+
    stat_cor(aes(color = book),method = "spearman", label.y = 10) +
    theme(strip.text.x = element_text(size = 12, color = "Black", face = "bold"), 
    strip.background = element_rect(color=c("white"), fill=c("white"), size=1.5, linetype="solid"), 
    plot.title = element_text(size = 12))

p3_plot = facet(p3, facet.by = c('book')) +
  labs(title = "Are IMDB ratings a function of Chapter and Book?",
       caption = "TidyTuesday: Avatar: The Last Airbender Books, Analysis @RRVdpool, Created: 8/10/2020")




  
## plot together:
#plot_grid(p1_plot, p2_plot, p3_plot, ncol = 2, labels =  c("A)", "B)", "C)"), align = "h")

## make a plot grid consisting of two panels: 
plot_row <- plot_grid(p1_plot, p2_plot, labels = c("A)", "B)"), align = "h")
plot_combine = plot_grid(plot_row, p3_plot, labels = c('', 'C)'), ncol = 1)

title <- ggdraw() + 
          draw_label("Factors contributing to IMDB ratings for Avatar: The last airbender",
                    fontface = 'bold',
                    x = 0,
                    hjust = 0) +
          theme(plot.margin = margin(0, 0, 0, 7))
          # add margin on the left of the drawing canvas,
          # so title is aligned with left edge of first plot



plot_grid(title, plot_combine, ncol = 1,
          # rel_heights values control vertical title margins
          rel_heights = c(0.1, 1))

dev.copy(pdf, "Avatar_airbender_20200811.pdf", 11, 8.5)
dev.off()






  