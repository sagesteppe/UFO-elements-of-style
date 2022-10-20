#' Draw boxplots in the style of Tukey with test statistics on them
#' 
#' This function serves to draw basic boxplots for the UFO AIM 5 year analysis
#' it inherits it's aestheics from a custom theme developed for this purpose
#' @param data - a data frame containing all variables for the plot
#' @param response - a single column of the variable to map
#' @param group - relevant grouping variable - expects character categorical
#' @param col_pal - color palette e.g. 'strata_pal', or 'lifeform_pal'
#' @return a ggplot with the option to refine contents. 
#' @example strata_pal_test <- c("setosa" = "#4A5A28", 
#' "versicolor" = "#ADB1B9", "virginica" = "#CEB88E")
#' iris_box <- boxplot_drawer(df = iris, response = Sepal.Length, 
#'                     group = Species, col_pal = strata_pal_test)
#' plot(iris_box) + labs(title = 'Comparision of Sepal Length in Iris Species') 
#' @export 
#' @seealso theme_boxplot
boxplot_drawer <- function(df, response, group, col_pal){
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
    
    theme_boxplot() +
    
    scale_colour_manual(values = col_pal) +
    theme(legend.position = 'none')
  
  return(ufo_boxplot)
}


#' Draws a stacked barchart to convey number of observations, a binomial response, 
#' and confidence estimates of the response. 
#'
#' this function serves to draw stacked proportion barcharts  for the UFO AIM
#' year analysis, these serve to show the sample size, and binomial breakdown
#' within the sample such as mortality. it inherits it's aesthetics from a
#' custom theme developed for this purpose 'theme_prop_bar'
#' 
#' @param data - a data frame containing all variables for the plot
#' @param response_val - a numerical response. 
#' @param response_cat - a category for the response
#' @param group1 - relevant grouping variable - e.g. 'treatment'
#' @param group2 - variable to facet by, e.g. year
#' @param alpha - alpha value for p-value, defauts to 0.2 for CI of 80%
#' @example is <- InsectSprays %>% 
#' mutate(
#'  ID = 1:n(), .before = count) %>% 
#'  mutate(
#'    year = rep(2021:2022, times = 6, each = 6),
#'    dead = floor(runif(n(), min=0, max=count)),
#'    live = count - dead) %>% 
#'  pivot_longer(cols = dead:live, values_to = 'tot_count', names_to = 'response')
#'  
#' head(is)
#' 
#' is_stacked_prop <-  stacked_prop_drawer(data = is, response_val = tot_count, 
#' response_cat = response, grp1 = spray, grp2 = year)
#' 
#' is_stacked_prop + labs(title = 'simulated dataset mortality in insects')
#' 
#' @export
#' @rdname UFO_EoS
stacked_prop_drawer <- function(data, response_val, response_cat, grp1, grp2,
                                alpha, ...){

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
    distinct(!!grp1, !!grp2, !!response_cat, .keep_all = T) %>% 
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
    labs(title = 'reed shot hardcoding shit') +
    scale_fill_manual(values = c('dead' = '#91A4C3', 
                                 'live' = '#C3B091'))
  
  return(myplot)
}


is <- InsectSprays %>% 
   mutate(
     ID = 1:n(), .before = count) %>% 
   mutate(
     year = rep(2021:2022, times = 6, each = 6),
     dead = floor(runif(n(), min=0, max=count)),
     live = count - dead) %>% 
   pivot_longer(cols = dead:live, values_to = 'tot_count', names_to = 'response')
 
 
 stacked_prop_drawer(data = is, response_val = tot_count, response_cat = response,
                     grp1 = spray, grp2 = year) +
   labs(title = 'simulated dataset mortality in insects')
 