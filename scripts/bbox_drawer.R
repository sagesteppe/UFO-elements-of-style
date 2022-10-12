bbox_drawer <- function(df, response, group, col_pal){
  
  #' this function serves to draw basic boxplots for the UFO AIM 5 year analysis
  #' it inherits it's aestheics from a custom theme developed for this purpose
  #' 
  #' Inputs : 
  #' data - a data frame containing all variables for the plot
  #' response - a single column of the variable to map
  #' groups - relevant grouping variable - expects character categorical
  #' col_pal - color palette e.g. 'strata_pal', or 'lifeform_pal'
  
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
    
    theme_bbox() +
    
    scale_colour_manual(values = col_pal) +
    theme(legend.position = 'none')
  
  return(ufo_boxplot)
}