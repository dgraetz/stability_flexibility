library(ggplot2)
design <- list()
design$curr_val$color$bi <- "orange"
design$curr_val$color$uni <- "navy"
design$prev_val$color$bi <- "orange"
design$prev_val$color$uni <- "navy"

design$curr_val$linetype$bi <- "solid"
design$curr_val$linetype$uni <- "41" #dashed
design$prev_val$linetype$bi <- "solid"
design$prev_val$linetype$uni <- "41" #dashed

design$lines$linewidth <- 1.1

design$errorbar$width <- 0
design$errorbar$linewidth <- 0.8

apa_theming <- function(base_size = 12,
                        base_family = "sans",
                        blue = "navy",
                        orange = "orange") {
  
  # based off how others strucutre their apa figure themes but adding the specifications we talked about
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      text = element_text(family = base_family, color = "black"),
      plot.title = element_text(size = base_size * 1.4, face = "bold"),
      plot.subtitle = element_text(size = base_size * 1.1),
      plot.caption = element_text(size = base_size * 0.9),
      
      axis.title = element_text(size = base_size * 1.1),
      axis.text = element_text(size = base_size),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black"),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      
      legend.position = "right",
      legend.title = element_text(size = base_size * 1.1),
      legend.text = element_text(size = base_size),
      legend.key.width = unit(0.4, "in"),
      
      strip.background = element_rect(fill = "white", color = "white"),
      strip.text = element_text(size = base_size * 1.1, face = "bold", margin = margin(t = 4, b = 4))
    )#,
  
  #assigning colors specified in specfifc order
  #scale_color_manual(values = c(blue, orange)) #this will not work, as it may assign different labels dependent on how factors are ordered between experiments
}


theme_set(apa_theming())
