library(here)
library(tidyverse)
library(ggpubr)

# Boxplot theme

#' the boxplot is the data visualization technique which we use most often. 
#' We test it out here with our 4 main stratum being represented as the 
#' three species in the data set

#' @export
#' @rdname UFO_EoS
theme_bbox <- function(){ 
    font = "sans"   #assign font family up front
    base_size = 12
    legend.position = 'none'
    
    theme_classic() %+replace%    #replace elements we want to change
      
      theme(
        
        #grid elements
        axis.ticks = element_blank(),          #strip axis ticks
        
        #text elements
        plot.title = element_text(  #title
          family = font,            #set font family
          size = 16,                #set font size
          face = 'bold',            #bold typeface
          hjust = 0.5,              #center    
          vjust = 2),               #move title up
        
        plot.subtitle = element_text(#subtitle
          family = font,
          size = 14),              
        
        plot.caption = element_text(#caption
          family = font,            
          size = 9, 
          hjust = 1),               #right align
        
        axis.title = element_text(  #axis titles
          family = font),   
        
        axis.text = element_text(   #axis text
          family = font,
          size = 9),
        
        axis.text.x = element_text( #margin for axis text
          margin=margin(5, b = 10)),
        
        axis.line = element_line(
          colour = "grey25", linetype=3)
        
        #since the legend often requires manual tweaking 
        #based on plot content, don't define it here
      )
}


# We also use the related barchart quite frequently here we make a similar plot type for them


#' @export
#' @rdname UFO_EoS
theme_bar <- function(){
  font = "sans"   #assign font family up front
  base_size = 12
  legend.position = 'none'
  
  theme_classic() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      axis.ticks = element_blank(),          #strip axis ticks
      
      #text elements
      plot.title = element_text(  #title
        family = font,            #set font family
        size = 16,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0.5,              #center    
        vjust = 2),               #move title up
      
      plot.subtitle = element_text(#subtitle
        family = font,
        size = 14),              
      
      plot.caption = element_text(#caption
        family = font,            
        size = 9, 
        hjust = 1),               #right align
      
      axis.title = element_text(  #axis titles
        family = font),   
      
      axis.title.x = element_text(
        hjust = 0.5, vjust = 4), #center      
      
      axis.text = element_text(   #axis text
        family = font,
        size = 9),
      
      axis.text.x = element_text( #margin for axis text
        margin=margin(5, b = 10),
        vjust = 7.5),
      
      strip.text = element_text(
        size = 12, color = "black", face = "bold"
      ),
      
      axis.line = element_blank(),
      legend.position = 'bottom'

      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}



set.seed(72)

is <- InsectSprays %>% 
  mutate(
    ID = 1:n(), .before = count) %>% 
  mutate(
    year = rep(2021:2022, times = 6, each = 6),
    dead = floor(runif(n(), min=0, max=count)),
    live = count - dead) %>% 
  pivot_longer(cols = dead:live, values_to = 'tot_count', names_to = 'response')

ordered_responses <- unique(is$response)

# CI calculator 
alpha=0.2

CInterval <- is %>% 
  group_by(year, spray, response) %>% 
  mutate(grp_total = sum(tot_count)) %>% 
  add_count(name = 'no_obs') %>% 
  mutate(
    t=qt((1-alpha)/2 + .5, n()-1),
    se = sd(tot_count) / sqrt(no_obs),
    CI = t*se) %>% 
  distinct(spray, year, response, .keep_all = T) %>% 
  select(spray, year, response, CI, grp_total, tot_count)
  
# now make sure the bars plot to the correct location... 
CInterval_upper <- CInterval %>% 
  ungroup(response) %>% 
  mutate(grp_total = sum(grp_total),
         pl_bar = grp_total - CI,
         pu_bar = grp_total + CI) %>% 
  filter(response == ordered_responses[1])

CInterval_lower <- CInterval %>% 
  mutate(grp_total = sum(grp_total),
         ll_bar = grp_total - CI,
         lu_bar = grp_total + CI) %>% 
  filter(response == ordered_responses[2])

  
ggplot(is, aes(y = tot_count, x = spray, fill = response)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~year, nrow = 1) +
  geom_linerange(data = CInterval_upper, 
                 aes(x=spray, ymin= pl_bar, ymax= pu_bar), 
                 colour="black", alpha=0.9) +
  geom_linerange(data = CInterval_lower, 
                 aes(x=spray, ymin= ll_bar, ymax= lu_bar), 
                 colour="black", alpha=0.9) +
  theme_bar() +
  theme(strip.background = element_blank() ) +
  labs(title = ' this is a test') +
  scale_fill_manual(values = c('dead' = '#91A4C3', 
                               'live' = '#C3B091'))


