#' Draw boxplots in the style of Tukey with test statistics on them
#' 
#' This function serves to draw basic boxplots for the UFO AIM 5 year analysis
#' it inherits it's aestheics from a custom theme developed for this purpose
#' @param data - a data frame containing all variables for the plot
#' @param response - a single column of the variable to map
#' @param col_pal - color palette e.g. 'strata_pal', or 'lifeform_pal'
#' @param group - relevant grouping variables - expects character categorical
#' @return a ggplot with the option to refine contents. 
#' @example strata_pal_test <- c("setosa" = "#4A5A28", 
#' "versicolor" = "#ADB1B9", "virginica" = "#CEB88E")
#' iris_box <- boxplot_drawer(df = iris, response = Sepal.Length, 
#'                     group = Species, col_pal = strata_pal_test)
#' plot(iris_box) + labs(title = 'Comparision of Sepal Length in Iris Species') 
#' @export 
#' @seealso theme_boxplot

boxplot_drawer <- function(df, response, col_pal, group){
  
  term <- as.formula(paste(rlang::enexpr(response), ' ~ ', dplyr::enexpr(group)))
  response <- rlang::enquo(response)
  group <- rlang::enquo(group)
  
  my_means <- ggpubr::compare_means(term,  data = df)
  my_comparisons <- my_means %>% 
    nest(groups = c(group1, group2)) %>% 
    pull(groups) %>% 
    map(., as.character)
  
  min_v <- dplyr::summarise(df, mean_mpg = floor(min(!!response))) |>
    pull() |> min()
  
  sample_sizes <- df %>% 
    dplyr::group_by(!!group) %>%  
    dplyr::tally() %>% 
    dplyr::mutate(n = paste0('n = ', n)) 
  
  ufo_boxplot <- ggplot(df, aes(x = !!group, y = !!response, colour = !!group),  
                        alpha = 0.5) + 
    stat_boxplot(notch = T, notchwidth = 0.75, 
                 varwidth = T, 
                 outlier.shape = 1, outlier.alpha = 0.8, outlier.colour = 'black') +
    
    geom_text(data = sample_sizes,
              aes(!!group, Inf, label = n), color = 'black', 
              vjust = "inward", size = 4, 
              y = min_v * 0.95) +
    ggpubr::stat_compare_means(comparisons = my_comparisons, 
                               aes(label = ..p.signif..),
                               tip.length = 0, vjust = 0.25, size = 4) +
    ggpubr::stat_compare_means(label.y = min_v * 2, size = 4)  +
    
    theme_boxplot() +
    expand_limits(y= min_v) +
    scale_colour_manual(values = col_pal) +
    theme(legend.position = 'none')
  
  return(ufo_boxplot)
}



#strata_pal_test <- c("setosa" = "#4A5A28", "versicolor" = "#ADB1B9", "virginica" = "#CEB88E")
#iris_box <- boxplot_drawer(df = iris, response = Sepal.Length, 
#                           group = Species, col_pal = strata_pal_test)
#iris_box <- plot(iris_box) + labs(title = 'Sepal Length in Iris')
#ggsave(iris_box, path = '../results/plots/', 
#       filename = 'boxplot_drawer.png', width = 720, height = 720,  units = 'px', device = 'png', bg = 'transparent')

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
#' @param fills_vals - vector of length two of the fill variable (response_cat)
#' @param fill_cols - a vector of length two of the color for the fill variable
#' @param rowN - number of rows to facet by
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
#'  stacked_prop_drawer(data = is, response_val = tot_count, 
#'     response_cat = response,
#'     grp1 = spray, grp2 = year,
#'     rowN = 2,
#'     fill_vals =  c('dead', 'live'), 
#'     fill_cols = c('#91A4C3', '#C3B091')) +
#'     labs(title = 'Effect of Insecticides on Insect Mortality') # note you
#'     # can just add elements onto the function in line. 
#' 
#' @export
#' @rdname UFO_EoS
stacked_prop_drawer <- function(data, response_val, response_cat, grp1, grp2,
                                alpha, fill_vals, fill_cols, rowN){

  if(missing(alpha)) {
    alpha = 0.2
  }
  
  names(fill_cols) <- fill_vals
  
  response_val <- enquo(response_val)
  response_cat <- enquo(response_cat)
  grp1 <- enquo(grp1)
  grp2 <- enquo(grp2)

  CInterval <- data %>% 
    dplyr::group_by(!!grp1, !!grp2, !!response_cat) %>% 
    dplyr::mutate(grp_total = sum(!!response_val)) %>% 
    dplyr::add_count(name = 'no_obs') %>% 
    dplyr::mutate(
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
  
  # style the labels
#  data <- data %>% 
#    dplyr::mutate(dplyr::across(.cols = c(!!response_cat,
#                                          !!grp1, !!grp2), ~ str_to_sentence(.x)))
  
  myplot <- ggplot(data, aes(y = !!response_val, x = !!grp1, fill = !!response_cat)) +
    geom_bar(position="stack", stat="identity") +
    facet_wrap(vars(!!grp2), nrow = rowN) +
    geom_linerange(data = CInterval_upper, 
                   aes(x = !!grp1, ymin = pl_bar, ymax = pu_bar), 
                   colour="black", alpha = 0.9) +
    geom_linerange(data = CInterval_lower, 
                   aes(x = !!grp1, ymin = ll_bar, ymax = lu_bar), 
                   colour="black", alpha = 0.9) +
    theme_prop_bar() +
    theme(strip.background = element_blank() ) +
    scale_fill_manual(values = fill_cols)
  
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



#' Draws a dodged set of bar charts to show proportional data
#'
#' this function serves to draw side by side barcharts (really with geom_col)  
#' for the UFO AIM year analysis, it can take on proportions with many values, not
#' just binomial outcomes. 
#'
#' @param data - a data frame containing all variables for the plot
#' @param response_val - a numerical response.
#' @param response_cat - a category for the response
#' @param group1 - relevant grouping variable - e.g. 'treatment'
#' @param group2 - variable to facet by, e.g. year
#' @param alpha - alpha value for p-value, defaults to 0.2 for CI of 80%
#' @param fills_vals - vector of length two of the fill variable (response_cat)
#' @param fill_cols - a vector of length two of the color for the fill variable
#' @param rowN - number of rows to facet by, defaults to 1
#' @param minCIv - a number of the minimum value a confidence interval can extent to
#' @param maxCIv - a number of the minimum value a confidence interval can extent to
#' @param dodgeV value to dodge the cols by
#' @param errorT transparency for the error bars, defaults to 0.5
#' @param errorW width for error bars, defaults to 0.3
#' @example is <- InsectSprays %>%
#' rowid_to_column(var = "ID") %>%
#'  mutate(
#'    year = rep(2021:2022, times = 6, each = 6),
#'    dead = floor(runif(nrow(.), min=0, max=count)),
#' live = count - dead,
#'    instar = floor(runif(nrow(.), min = 0, max = count))
#'  ) %>% 
#'  rowwise()  %>% 
#'  mutate(count = sum(dead, live,instar)) %>% 
#'  mutate(across(dead:instar,  ~ (.x/count) * 100)) %>% 
#'  pivot_longer(cols = dead:instar, values_to = 'percent', names_to = 'response')
#'  
#' head(is)
#'
#' dodged_drawer(data = is, response_val = percent,
#' response_cat = response,
#' grp1 = spray, grp2 = year, rowN = 1, 
#' fill_vals =  c('dead', 'live', 'instar'),
#' fill_cols = c('#91A4C3', '#C3B091', '#183A91')) +
#'  labs(title = 'Effect of Insecticides on Insect Mortality') +
#'  theme_bw()
#'
#' @export
#' @rdname UFO_EoS
#' 
dodged_drawer <- function(data, response_val, response_cat, grp1, grp2,
                          fill_vals, fill_cols, alpha, rowN,
                          minCIv, maxCIv, dodgeV, errorT, errorW){
  
  names(fill_cols) <- fill_vals
  response_val <- enquo(response_val)
  response_cat <- enquo(response_cat)
  grp1 <- enquo(grp1)
  grp2 <- enquo(grp2)
  
  if(missing(errorW)){errorW <- 0.3}
  if(missing(errorT)){errorT <- 0.5}
  if(missing(alpha)){alpha <- 1-0.2} else {alpha <- 1 - alpha}
  if(missing(rowN)){rowN <- 1}
  if(missing(minCIv)){minCIv <- -Inf}
  if(missing(maxCIv)){maxCIv <- Inf}
  if(missing(dodgeV)){dodgeV <- 1}
  
  sumSE <- Rmisc::summarySE(data, measurevar = quo_name(response_val),
                            groupvars = c(quo_name(grp1), quo_name(grp2),
                                          quo_name(response_cat)),
                            conf.interval = alpha) %>% 
    dplyr::mutate(ci_low = !!response_val - ci, 
                  ci_high = !!response_val + ci,
                  ci_low = if_else(ci_low < minCIv, 0, ci_low),
                  ci_high = if_else(ci_low > maxCIv, 100, ci_high))
  
  dodge <- position_dodge(width = dodgeV)
  myplot <- ggplot(sumSE, 
                   aes(y = !!response_val, x = !!grp1, fill = !!response_cat)) +
    geom_col(position = dodge, stat = "identity") +
    theme(strip.background = element_blank() ) +
    scale_fill_manual(values = fill_cols) +
    facet_wrap(vars(!!grp2), nrow = rowN) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                  width = errorW, position = dodge, alpha = errorT
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, .1)))
  
  
  return(myplot)
  
}
