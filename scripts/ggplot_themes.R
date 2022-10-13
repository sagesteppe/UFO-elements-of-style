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


stacked_prop_drawer <- function(data, response_val, response_cat, grp1, grp2,
                                 alpha){
    
    #' this function serves to draw stacked proportion barcharts  for the UFO AIM
    #' year analysis, these serve to show the sample size, and binomial breakdown
    #' within the sample such as mortality. it inherits it's aesthetics from a
    #' custom theme developed for this purpose 'theme_prop_bar'
    #' 
    #' Inputs : 
    #' data - a data frame containing all variables for the plot
    #' response_val - a numerical response. 
    #' response_cat - a category for the response
    #' group1 - relevant grouping variable - e.g. 'treatment'
    #' group2 - variable to facet by, e.g. year
    #' alpha - alpha value for p-value, defauts to 0.2 for CI of 80%
    
  if(missing(alpha)) {
    alpha = 0.2
  }
  
  response_val <- enquo(response_val)
  response_cat <- enquo(response_cat)
  grp1 <- enquo(grp1)
  grp2 <- enquo(grp2)
  
  CInterval <- data %>% 
    group_by(!!grp1, !!grp2, !!response_cat) %>% 
    mutate(grp_total = sum(!!response_val)) %>% 
    add_count(name = 'no_obs') %>% 
    mutate(
      t = qt((1-alpha)/2 + .5, n()-1),
      se = sd(!!response_val) / sqrt(no_obs),
      CI = t*se) %>% 
    distinct(spray, year, !!response_cat, .keep_all = T) %>% 
    select(!!grp1, !!grp2, !!response_cat, CI, grp_total, !!response_val)
  
 # Confidence interval generation
  
  ordered_responses <- distinct(data, !!response_cat) %>% pull()
  
  CInterval_upper <- CInterval %>% 
    ungroup(!!response_cat) %>% 
    mutate(grp_total = sum(grp_total),
           pl_bar = grp_total - CI,
           pu_bar = grp_total + CI) %>% 
    filter(!!response_cat == ordered_responses[1])
  
  CInterval_lower <- CInterval %>% 
    mutate(grp_total = sum(grp_total),
           ll_bar = grp_total - CI,
           lu_bar = grp_total + CI) %>% 
    filter(!!response_cat == ordered_responses[2])
  
  myplot <- ggplot(data, aes(y = !!response_val, x = !!grp1, fill = !!response_cat)) +
    geom_bar(position="stack", stat="identity") +
    facet_wrap(vars(!!grp2), nrow = 1) +
    geom_linerange(data = CInterval_upper, 
                   aes(x=spray, ymin= pl_bar, ymax= pu_bar), 
                   colour="black", alpha=0.9) +
    geom_linerange(data = CInterval_lower, 
                   aes(x=spray, ymin= ll_bar, ymax= lu_bar), 
                   colour="black", alpha=0.9) +
    theme_prop_bar() +
    theme(strip.background = element_blank() ) +
    labs(title = ' this is a test') +
    scale_fill_manual(values = c('dead' = '#91A4C3', 
                                 'live' = '#C3B091'))
  
    return(myplot)
}