library(here)
library(tidyverse)
library(ggpubr)

# Boxplot theme

#' the boxplot is the data visualization technique which we use most often. 
#' We test it out here with our 4 main stratum being represented as the 
#' three species in the data set


strata_pal_test <- c("setosa" = "#4A5A28", "versicolor" = "#ADB1B9", "virginica" = "#CEB88E")

my_means <- compare_means(Sepal.Length ~ Species,  data = iris)
my_comparisons <- my_means %>% 
  nest(groups = c(group1, group2)) %>% 
  pull(groups) %>% 
  map(., as.character)

min_value <- 

sample_sizes <- iris %>% 
  group_by(Species) %>% 
  tally() %>% 
  mutate(n = paste0('n = ', n))


theme_bbox <- function(){ 
    font = "sans"   #assign font family up front
    base_size = 12
    legend.position = 'none'
    
    theme_classic() %+replace%    #replace elements we want to change
      
      theme(
        
        #grid elements
        axis.ticks = element_blank(),          #strip axis ticks
        
        #text elements
        plot.title = element_text(             #title
          family = font,            #set font family
          size = 16,                #set font size
          face = 'bold',            #bold typeface
          hjust = 0.5,                #left align     
          vjust = 2
        ),
        
        plot.subtitle = element_text(          #subtitle
          family = font,            #font family
          size = 14),               #font size
        
        plot.caption = element_text(  #caption
          family = font,            #font family
          size = 9,                 #font size
          hjust = 1),               #right align
        
        axis.title = element_text(  #axis titles
          family = font),   
        
        axis.text = element_text(              #axis text
          family = font,            #axis famuly
          size = 9),                #font size
        
        axis.text.x = element_text(            #margin for axis text
          margin=margin(5, b = 10)),
        
        axis.line = element_line(
          colour = "grey25", linetype=3)
        
        #since the legend often requires manual tweaking 
        #based on plot content, don't define it here
      )
}




iris[1:125,] %>% 
  ggplot(., aes(x=Species, y=Sepal.Length, colour = Species
  ),  alpha = 0.5) + 
  stat_boxplot(notch = T, notchwidth = 0.75, 
               varwidth = T, 
               outlier.shape = 1, outlier.alpha = 0.4, outlier.colour = 'black') +
  
  geom_text(data = sample_sizes,
            aes(Species, Inf, label = n), color = 'black', 
            vjust = "inward", size = 3, 
            y = floor(min(iris$Sepal.Length * 0.95))) +
  stat_compare_means(comparisons = my_comparisons, 
                     aes(label = ..p.signif..),
                     tip.length = 0, vjust = 0.25) +
  stat_compare_means(label.y = floor(min(iris$Sepal.Length * 0.9)))  +
  
  labs(title = 'Comparision of Sepal Length in Iris Species') +
  
  theme_bbox() +
  
  scale_colour_manual(values = strata_pal_test) +
  theme(legend.position = 'none')



bbox_drawer <- function(df, response, group){
  
  #' this function serves to draw basic boxplots for the UFO AIM 5 year analysis
  #' it inherits it's aestheics from a custom theme developed for this purpose
  #' 
  #' Inputs : 
  #' data - a data frame containing all variables for the plot
  #' response - a single column of the variable to map
  #' groups - relevant grouping variable - expects character categorical

  term <- as.formula(paste(enexpr(response), ' ~ ', enexpr(group)))
  
  response <- enquo(response)
  group <- enquo(group)
  
  my_means <- compare_means(term,  data = df)
  my_comparisons <- my_means %>% 
    nest(groups = c(group1, group2)) %>% 
    pull(groups) %>% 
    map(., as.character)
  
  min_v <- summarise(df, mean_mpg = floor(min(!!response))) %>% pull()
    
  sample_sizes <- df %>% 
    group_by(!!group) %>% 
    tally() %>% 
    mutate(n = paste0('n = ', n))
  
   ufo_boxplot <- ggplot(df, aes(x = !!group, y = !!response, colour = !!group),  
          alpha = 0.5) + 
     stat_boxplot(notch = T, notchwidth = 0.75, 
                  varwidth = T, 
                  outlier.shape = 1, outlier.alpha = 0.4, outlier.colour = 'black') +
    
    geom_text(data = sample_sizes,
              aes(!!group, Inf, label = n), color = 'black', 
              vjust = "inward", size = 3, 
              y = min_v * 0.95) +
    stat_compare_means(comparisons = my_comparisons, 
                       aes(label = ..p.signif..),
                       tip.length = 0, vjust = 0.25) +
    stat_compare_means(label.y = min_v * 0.85)  +
    
    labs(title = 'Comparision of Sepal Length in Iris Species') +
    
    theme_bbox() +
    
    scale_colour_manual(values = strata_pal_test) +
    theme(legend.position = 'none')
  
   return(ufo_boxplot)
}


resin <- bbox_drawer(df = iris, response = Sepal.Length, group = Species)
plot(resin)


example <- function(df, group, response){
  
  term <- as.formula(paste0(enexpr(response), ' ~ ', enexpr(group)))
#  term <- as.formula(term)
  my_means <- compare_means(term,  data = df)
  return(my_means)
}

example(df = iris, response = Petal.Length, group = Species)
