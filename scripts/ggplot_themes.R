library(here)
library(tidyverse)
library(ggpubr)

# Boxplot theme

#' the boxplot is the data visualization technique which we use most often. 
#' We test it out here with our 4 main stratum being represented as the 
#' three species in the data set

#' @export
#' @rdname UFO_EoS
theme_boxplot <- function(){ 
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
          size = 14,                #set font size
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



#' @export
#' @rdname UFO_EoS
theme_prop_bar <- function(){
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
        size = 14,                #set font size
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
