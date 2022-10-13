library(tidyverse)
library(here)

crowngap <- read.csv(file.path(here(), 'data/processed/crownggap_clean.csv'))

rasta_kols <- c("dark green", "khaki1", "sienna1", "firebrick3", "firebrick4")
legend_names <- c("No Gap", "25-50", "51-100", 
                  "101-200", "200+")

crown_gap_plots <- lapply(sort(unique(crowngap$site)), function (i) {
  
   ggplot(crowngap[crowngap$site==i,], aes(x=year, y=percent, fill=gap.class)) +
    geom_bar(position = "fill", stat="identity") + 
    theme_classic() +
    facet_wrap(~site, scales = 'free_x', ncol = 4) + # remove me for only one site - filter me above!!!

    scale_y_continuous(labels = scales::percent) + 
    scale_fill_manual(values= rasta_kols,
                      labels= legend_names) +
    labs(title="Crown Gap Cover SWDO, CO 2022",
         x="Year", 
         y="Relative Cover") + 
    guides(fill=guide_legend(title="Gap Size (cm)")) + 
    geom_hline(yintercept=0.5, linetype=4, color = "black", lwd = 1) + 
    theme(legend.position =  c(0.9, 0.25), 
          plot.title = element_text(hjust = 0.5)) 

})

names(crown_gap_plots) <- sort(unique(crowngap$site))
p <- file.path(here(), 'results/figures/manyPlotOutput')

ifelse(!dir.exists(file.path(p)), dir.create(file.path(p)), FALSE) # this optional 
# feel free to shift to a figure table which is already there. 

for (i in 1:length(crown_gap_plots)){
  
  ggsave(plot = crown_gap_plots[[i]], 
         file = paste(names(crown_gap_plots)[i], 'Crown_gap_fig', Sys.Date(), '.png', sep = '_'), 
         path = p, device = 'png', dpi = 300, 
     #  width = 9, height = 16, units = 'cm'
     )
}

rm(crowngap, rasta_kols, legend_names, crown_gap_plots, i)
