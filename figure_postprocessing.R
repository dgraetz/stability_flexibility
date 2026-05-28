library(tidyverse)
library(egg)
source("theme.R")

figures <- list.files("figures/", pattern = "RDS|rds", full.names = TRUE)

size_output_betw <- c(3, 5)
size_output_ddm <- c(3, 2)
size_output_within <- c(3.7, 2.5)
size_output_exp <- c(3.7/2, 2.5)


# Reading in figures ----

for (f in figures) {
  obj_name <- gsub(".RDS|.rds", "", basename(f))
  assign(obj_name, readRDS(f))
}



get_range <- function(plt_a, plt_b, dv_a, dv_b, eb_a_lower = NULL, eb_a_upper = NULL, eb_b_lower = NULL, eb_b_upper = NULL){
  a_range <- range(plt_a@data[,dv_a])
  b_range <- range(plt_b@data[,dv_b])
  
  dv_range <- c(min(a_range[1], b_range[1]), max(a_range[2], b_range[2]))
  
  ranges <- list()
  
  ranges$dv <- dv_range
  ranges$error <- NA
  
  if (is.character(eb_a_lower) & is.character(eb_a_upper) & is.character(eb_b_lower) & is.character(eb_b_upper)){
    
    eb_upper_range<- range(c(plt_a@data[,eb_a_upper] %>% unlist(), plt_b@data[,eb_b_upper] %>% unlist(),
                             plt_a@data[,eb_a_lower] %>% unlist(), plt_b@data[,eb_b_lower] %>% unlist()))
    
    ranges$error <- eb_upper_range
  }
  
  return(ranges)
}


AR_plt_background <- rgb(1, 1, 1)
AC_plt_background <- rgb(0.96, 0.96, 0.96)


# Manipulating plots ----

## ddm ----

### a ----

ddm_a_range <- get_range(AC_ddm_a, AR_ddm_a, dv_a = "a_m", dv_b = "a_m", eb_a_lower = "a_ci_lower", eb_b_lower = "a_ci_lower", eb_a_upper = "a_ci_upper", eb_b_upper = "a_ci_upper")

ddm_a_range$error[1] <- ddm_a_range$error[1] - 0.01
ddm_a_range$error[2] <- ddm_a_range$error[2] + 0.01


AR_ddm_a <- AR_ddm_a +
  labs(y = "Boundary Separation")+
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.05)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = ddm_a_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))
AR_ddm_a

AC_ddm_a@layers$geom_errorbar$aes_params$linetype = "solid"

AC_ddm_a <- AC_ddm_a +
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.05)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = ddm_a_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  ) +
  labs(y = "Boundary Separation") +
  scale_color_manual(name = "Current Valence",
                     labels = c("Univalent" = "Univalent",
                                "Bivalent" = "Bivalent"),
                     values = c("Univalent" = design$curr_val$color$uni,
                                "Bivalent" = design$curr_val$color$bi))+
  scale_linetype_manual(name = "Previous Valence",
                        labels = c("Univalent" = "Univalent",
                                   "Bivalent" = "Bivalent"),
                        values = c("Univalent" = design$prev_val$linetype$uni,
                                   "Bivalent" = design$prev_val$linetype$bi))+
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = alpha("white", 0)),
        axis.text.y = element_text(color = alpha("white", 0)),
        axis.title.y = element_text(color = alpha("white", 0)),
        axis.line.y = element_line(color = alpha("white", 0)),
        legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))
AC_ddm_a
AR_ddm_a

AC_ddm_a@layers$geom_errorbar$geom_params$width <- design$errorbar$width
AR_ddm_a@layers$geom_errorbar$geom_params$width <- design$errorbar$width


ggsave(plot = AR_ddm_a, "figures/postprocessing/AR_ddm_a.svg",  width = size_output_ddm[1], height = size_output_ddm[2], units = "in")
ggsave(plot = AC_ddm_a, "figures/postprocessing/AC_ddm_a.svg",  width = size_output_ddm[1], height = size_output_ddm[2], units = "in")

ggsave(plot = AR_ddm_a + theme(legend.position = "bottom"), "figures/postprocessing/legend_bottom.svg", width = 10, height = 3, units = "in")
ggsave(plot = AR_ddm_a + theme(legend.position = "right"), "figures/postprocessing/legend_right.svg", width = 10, height = 3, units = "in")

### TER ----


ddm_Ter_range <- get_range(AC_ddm_TER, AR_ddm_TER, dv_a = "Ter_m", dv_b = "Ter_m", eb_a_lower = "Ter_ci_lower", eb_b_lower = "Ter_ci_lower", eb_a_upper = "Ter_ci_upper", eb_b_upper = "Ter_ci_upper")

ddm_Ter_range$error[1] <- ddm_Ter_range$error[1] - 0.01
ddm_Ter_range$error[2] <- ddm_Ter_range$error[2] + 0.01

AC_ddm_TER@layers$geom_errorbar$aes_params$linetype = "solid"

AC_ddm_TER <- AC_ddm_TER +
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.08)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = ddm_Ter_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  ) +
  labs(y = "Non-Decision Time") +
  scale_color_manual(name = "Current Valence",
                     labels = c("Univalent" = "Univalent",
                                "Bivalent" = "Bivalent"),
                     values = c("Univalent" = design$curr_val$color$uni,
                                "Bivalent" = design$curr_val$color$bi))+
  scale_linetype_manual(name = "Previous Valence",
                        labels = c("Univalent" = "Univalent",
                                   "Bivalent" = "Bivalent"),
                        values = c("Univalent" = design$prev_val$linetype$uni,
                                   "Bivalent" = design$prev_val$linetype$bi))+
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = alpha("white", 0)),
        axis.text.y = element_text(color = alpha("white", 0)),
        axis.title.y = element_text(color = alpha("white", 0)),
        axis.line.y = element_line(color = alpha("white", 0)),
        legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))

AR_ddm_TER <- AR_ddm_TER +
  labs(y = "Non-Decision Time")+
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.08)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = ddm_Ter_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))

AC_ddm_TER@layers$geom_errorbar$geom_params$width <- design$errorbar$width
AR_ddm_TER@layers$geom_errorbar$geom_params$width <- design$errorbar$width


AC_ddm_TER
AR_ddm_TER

ggsave(plot = AC_ddm_TER, "figures/postprocessing/AC_ddm_TER.svg",  width = size_output_ddm[1], height = size_output_ddm[2], units = "in")
ggsave(plot = AR_ddm_TER, "figures/postprocessing/AR_ddm_TER.svg",  width = size_output_ddm[1], height = size_output_ddm[2], units = "in")

### v ----

ddm_v_range <- get_range(AC_ddm_v, AR_ddm_v, dv_a = "v_m", dv_b = "v_m", eb_a_lower = "v_ci_lower", eb_b_lower = "v_ci_lower", eb_a_upper = "v_ci_upper", eb_b_upper = "v_ci_upper")


AC_ddm_v@layers$geom_errorbar$aes_params$linetype = "solid"

AC_ddm_v <- AC_ddm_v +
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.03)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = ddm_v_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  ) +
  labs(y = "Drift Rate") +
  scale_color_manual(name = "Current Valence",
                     labels = c("Univalent" = "Univalent",
                                "Bivalent" = "Bivalent"),
                     values = c("Univalent" = design$curr_val$color$uni,
                                "Bivalent" = design$curr_val$color$bi))+
  scale_linetype_manual(name = "Previous Valence",
                        labels = c("Univalent" = "Univalent",
                                   "Bivalent" = "Bivalent"),
                        values = c("Univalent" = design$prev_val$linetype$uni,
                                   "Bivalent" = design$prev_val$linetype$bi))+
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = alpha("white", 0)),
        axis.text.y = element_text(color = alpha("white", 0)),
        axis.title.y = element_text(color = alpha("white", 0)),
        axis.line.y = element_line(color = alpha("white", 0)),
        legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))

AR_ddm_v <- AR_ddm_v +
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.03)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  labs(y = "Drift Rate") +
  coord_cartesian(ylim = ddm_v_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))+
  theme(axis.title.x = element_blank(),
        legend.position = "none")

AC_ddm_v@layers$geom_errorbar$geom_params$width <- design$errorbar$width
AR_ddm_v@layers$geom_errorbar$geom_params$width <- design$errorbar$width


ggsave(plot = AC_ddm_v, "figures/postprocessing/AC_ddm_v.svg",  width = size_output_ddm[1], height = size_output_ddm[2], units = "in")
ggsave(plot = AR_ddm_v, "figures/postprocessing/AR_ddm_v.svg",  width = size_output_ddm[1], height = size_output_ddm[2], units = "in")


## Errors ----

Error_range <- get_range(AC_Errors, AR_Errors, dv_a = "Error_m", dv_b = "Error_m", eb_a_lower = "Error_ci_lower", eb_b_lower = "Error_ci_lower", eb_a_upper = "Error_ci_upper", eb_b_upper = "Error_ci_upper")

AC_Errors@layers$geom_errorbar$aes_params$linetype = "solid"

AC_Errors <- AC_Errors +
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.03)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = Error_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  ) +
  labs(y = "Error") +
  scale_color_manual(name = "Current Valence",
                     labels = c("Univalent" = "Univalent",
                                "Bivalent" = "Bivalent"),
                     values = c("Univalent" = design$curr_val$color$uni,
                                "Bivalent" = design$curr_val$color$bi))+
  scale_linetype_manual(name = "Previous Valence",
                        labels = c("Univalent" = "Univalent",
                                   "Bivalent" = "Bivalent"),
                        values = c("Univalent" = design$prev_val$linetype$uni,
                                   "Bivalent" = design$prev_val$linetype$bi))+
  scale_y_continuous(
    labels = function(x) sub("^(-?)0\\.", "\\1.", sprintf("%.2f", x))
  )+
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = alpha("white", 0)),
        axis.text.y = element_text(color = alpha("white", 0)),
        axis.title.y = element_text(color = alpha("white", 0)),
        axis.line.y = element_line(color = alpha("white", 0)),
        legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))
AC_Errors

AR_Errors <- AR_Errors +
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.03)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = Error_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  ) +
  scale_y_continuous(
    labels = function(x) sub("^(-?)0\\.", "\\1.", sprintf("%.2f", x))
  )+
  labs(y = "Error") +
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))
AR_Errors

AC_Errors@layers$geom_errorbar$geom_params$width <- design$errorbar$width
AR_Errors@layers$geom_errorbar$geom_params$width <- design$errorbar$width

ggsave(plot = AC_Errors, "figures/postprocessing/AC_Errors.svg", width = size_output_exp[1], height = size_output_exp[2], units = "in")
ggsave(plot = AR_Errors, "figures/postprocessing/AR_Errors.svg", width = size_output_exp[1], height = size_output_exp[2], units = "in")

AC_Errors_w_y <- AC_Errors + 
  theme(axis.ticks.y = element_line(color = alpha("black", 1)),
        axis.text.y = element_text(color = alpha("black", 1)),
        axis.title.y = element_text(color = alpha("black", 1)),
        axis.line.y = element_line(color = alpha("black", 1)))

ggsave(plot = AC_Errors_w_y, "figures/postprocessing/AC_Errors_w_y.svg", width = size_output_exp[1], height = size_output_exp[2], units = "in")


## log(RT) ----

logRT_range <- get_range(AC_RTlog, AR_RTlog, dv_a = "RT_m", dv_b = "RT_log_m", eb_a_lower = "RT_ci_lower", eb_b_lower = "RT_log_ci_lower", eb_a_upper = "RT_ci_upper", eb_b_upper = "RT_log_ci_upper")

AC_RTlog@layers$geom_errorbar$aes_params$linetype = "solid"

AC_RTlog <- AC_RTlog +
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.2)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = logRT_range$error)+
  labs(y = "log(RT)") +
  scale_color_manual(name = "Current Valence",
                     labels = c("Univalent" = "Univalent",
                                "Bivalent" = "Bivalent"),
                     values = c("Univalent" = design$curr_val$color$uni,
                                "Bivalent" = design$curr_val$color$bi))+
  scale_linetype_manual(name = "Previous Valence",
                        labels = c("Univalent" = "Univalent",
                                   "Bivalent" = "Bivalent"),
                        values = c("Univalent" = design$prev_val$linetype$uni,
                                   "Bivalent" = design$prev_val$linetype$bi))+
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = alpha("white", 0)),
        axis.text.y = element_text(color = alpha("white", 0)),
        axis.title.y = element_text(color = alpha("white", 0)),
        axis.line.y = element_line(color = alpha("white", 0)),
        legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))

AR_RTlog <- AR_RTlog +
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.2)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = logRT_range$error)+
  labs(y = "log(RT)") +
  
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))


AC_RTlog@layers$geom_errorbar$geom_params$width <- design$errorbar$width
AR_RTlog@layers$geom_errorbar$geom_params$width <- design$errorbar$width


ggsave(plot = AC_RTlog, "figures/postprocessing/AC_RTlog.svg", width = size_output_exp[1], height = size_output_exp[2], units = "in")
ggsave(plot = AR_RTlog, "figures/postprocessing/AR_RTlog.svg", width = size_output_exp[1], height = size_output_exp[2], units = "in")

AC_RTlog_w_y <- AC_RTlog + 
  theme(axis.ticks.y = element_line(color = alpha("black", 1)),
        axis.text.y = element_text(color = alpha("black", 1)),
        axis.title.y = element_text(color = alpha("black", 1)),
        axis.line.y = element_line(color = alpha("black", 1)))

ggsave(plot = AC_RTlog_w_y, "figures/postprocessing/AC_RTlog_w_y.svg", width = size_output_exp[1], height = size_output_exp[2], units = "in")

## lag RT ----

lagRT_range <- get_range(AC_lagRT, AR_lagRT, dv_a = "emmean", dv_b = "emmean", eb_a_lower = "asymp.LCL", eb_b_lower = "asymp.LCL", eb_a_upper = "asymp.UCL", eb_b_upper = "asymp.UCL")

AC_lagRT@layers$geom_errorbar$aes_params$linetype = "solid"

AC_lagRT <- AC_lagRT +
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  facet_wrap(~lag1_time_log_res_c, labeller = labeller(lag1_time_log_res_c = c("-1.5" = "Lag RT Fast",
                                                                               "1.5" = "Lag RT Slow")))+
  coord_cartesian(ylim = lagRT_range$error)+
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(-5, max(x), by = 0.2)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  labs(y = "log(RT) Prediction") +
  scale_color_manual(name = "Current Valence",
                     labels = c("Univalent" = "Univalent",
                                "Bivalent" = "Bivalent"),
                     values = c("Univalent" = design$curr_val$color$uni,
                                "Bivalent" = design$curr_val$color$bi))+
  scale_linetype_manual(name = "Previous Valence",
                        labels = c("Univalent" = "Univalent",
                                   "Bivalent" = "Bivalent"),
                        values = c("Univalent" = design$prev_val$linetype$uni,
                                   "Bivalent" = design$prev_val$linetype$bi))+
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = alpha("white", 0)),
        axis.text.y = element_text(color = alpha("white", 0)),
        axis.title.y = element_text(color = alpha("white", 0)),
        axis.line.y = element_line(color = alpha("white", 0)),
        legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))

AR_lagRT <- AR_lagRT +
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  facet_wrap(~lag1_time_log_res_w_s_int_c, labeller = labeller(lag1_time_log_res_w_s_int_c = c("-1.5" = "Lag RT Fast",
                                                                                               "1.5" = "Lag RT Slow")))+
  coord_cartesian(ylim = lagRT_range$error)+
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(-5, max(x), by = 0.2)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  labs(y = "log(RT) Prediction") +
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))

AC_lagRT@layers$geom_errorbar$geom_params$width <- design$errorbar$width
AR_lagRT@layers$geom_errorbar$geom_params$width <- design$errorbar$width


ggsave(plot = AC_lagRT, "figures/postprocessing/AC_lagRT.svg", width = size_output_within[1], height = size_output_within[2], units = "in")
ggsave(plot = AR_lagRT, "figures/postprocessing/AR_lagRT.svg", width = size_output_within[1], height = size_output_within[2], units = "in")




## betweeen plots ----
#text size:

betw_textsize <- 3.2
betw_asterisk_size <- 6
betw_asterisk_symbol <- "*"
betw_se_color <- "grey40"
betw_se_alpha <- 0.2

### RT ----

AR_betw_RT@layers$geom_label$aes_params$size <- betw_textsize
AC_betw_RT@layers$geom_label$aes_params$size <- betw_textsize

AR_betw_RT@layers$geom_point$aes_params$size <- betw_asterisk_size
AC_betw_RT@layers$geom_point$aes_params$size <- betw_asterisk_size
AR_betw_RT@layers$geom_point$aes_params$shape <- betw_asterisk_symbol
AC_betw_RT@layers$geom_point$aes_params$shape <- betw_asterisk_symbol


AR_betw_RT@layers$geom_ribbon$aes_params$colour <- NA
AR_betw_RT@layers$geom_ribbon$aes_params$fill <- betw_se_color
AR_betw_RT@layers$geom_ribbon$aes_params$alpha <- betw_se_alpha


AC_betw_RT@layers$geom_ribbon$aes_params$colour <- NA
AC_betw_RT@layers$geom_ribbon$aes_params$fill <- betw_se_color
AC_betw_RT@layers$geom_ribbon$aes_params$alpha <- betw_se_alpha

betw_RT_range <- c(-0.25, 0.9)
AC_betw_RT

AC_betw_RT <- AC_betw_RT + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.6)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = betw_RT_range, xlim = c(1, 4.5))+ 
  theme(
    strip.text = element_text(color = alpha("white", 0)),
    strip.background = element_blank(),
    #axis.line.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10, color = alpha("white", 0)),
    axis.ticks.y = element_line(color = alpha("white", 0))
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))
AC_betw_RT

AR_betw_RT <- AR_betw_RT + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.6)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = betw_RT_range, xlim = c(1, 4.5)) + 
  theme(
    strip.text = element_text(color = alpha("white", 0)),
    strip.background = element_blank(),
    #axis.line.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank()
  )+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))

AR_betw_RT


ggsave(plot = AC_betw_RT, "figures/postprocessing/AC_betw_RT.svg", 
       width = size_output_betw[1], 
       height = size_output_betw[2], units = "in")
ggsave(plot = AR_betw_RT, "figures/postprocessing/AR_betw_RT.svg", 
       width = size_output_betw[1], 
       height = size_output_betw[2], units = "in")




### Error ----

AR_betw_Error@layers$geom_label$aes_params$size <- betw_textsize
AC_betw_Error@layers$geom_label$aes_params$size <- betw_textsize

AR_betw_Error@layers$geom_point$aes_params$size <- betw_asterisk_size
AC_betw_Error@layers$geom_point$aes_params$size <- betw_asterisk_size

AR_betw_Error@layers$geom_point$aes_params$shape <- betw_asterisk_symbol
AC_betw_Error@layers$geom_point$aes_params$shape <- betw_asterisk_symbol

AR_betw_Error@layers$geom_ribbon$aes_params$colour <- NA
AR_betw_Error@layers$geom_ribbon$aes_params$fill <- betw_se_color
AR_betw_Error@layers$geom_ribbon$aes_params$alpha <- betw_se_alpha
AC_betw_Error@layers$geom_ribbon$aes_params$colour <- NA
AC_betw_Error@layers$geom_ribbon$aes_params$fill <- betw_se_color
AC_betw_Error@layers$geom_ribbon$aes_params$alpha <- betw_se_alpha

betw_Error_range <- c(-0.25, 0.9)

AC_betw_Error <- AC_betw_Error + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.6)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = betw_Error_range, xlim = c(1, 4.5)) + 
  theme(
    strip.text = element_text(color = alpha("white", 0)),
    strip.background = element_blank(),
    # axis.line.x = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_text(size = 10, color = alpha("white", 0)),
    axis.ticks.y = element_line(color = alpha("white", 0))
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))

AC_betw_Error

AR_betw_Error <- AR_betw_Error + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.6)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = betw_Error_range, xlim = c(1, 4.5)) + 
  theme(
    strip.text = element_text(color = alpha("white", 0)),
    strip.background = element_blank(),
    # axis.line.x = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank()
  )+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))
AR_betw_Error


ggsave(plot = AC_betw_Error, "figures/postprocessing/AC_betw_Error.svg", 
       width = size_output_betw[1], 
       height = size_output_betw[2], units = "in")
ggsave(plot = AR_betw_Error, "figures/postprocessing/AR_betw_Error.svg", 
       width = size_output_betw[1], 
       height = size_output_betw[2], units = "in")



### v ----


AR_betw_v@layers$geom_label$aes_params$size <- betw_textsize
AC_betw_v@layers$geom_label$aes_params$size <- betw_textsize

AR_betw_v@layers$geom_point$aes_params$size <- betw_asterisk_size
AC_betw_v@layers$geom_point$aes_params$size <- betw_asterisk_size

AR_betw_v@layers$geom_point$aes_params$shape <- betw_asterisk_symbol
AC_betw_v@layers$geom_point$aes_params$shape <- betw_asterisk_symbol

AR_betw_v@layers$geom_ribbon$aes_params$colour <- NA
AR_betw_v@layers$geom_ribbon$aes_params$fill <- betw_se_color
AR_betw_v@layers$geom_ribbon$aes_params$alpha <- betw_se_alpha
AC_betw_v@layers$geom_ribbon$aes_params$colour <- NA
AC_betw_v@layers$geom_ribbon$aes_params$fill <- betw_se_color
AC_betw_v@layers$geom_ribbon$aes_params$alpha <- betw_se_alpha

betw_v_range <- c(-0.25, 0.9)

AC_betw_v <- AC_betw_v + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.6)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = betw_v_range, xlim = c(1, 4.5)) + 
  theme(
    strip.text = element_text(color = alpha("white", 0)),
    strip.background = element_blank(),
    # axis.line.x = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.line.y = element_line(color = alpha("white", 0)),
    axis.text.y = element_text(size = 10, color = alpha("white", 0)),
    axis.ticks.y = element_line(color = alpha("white", 0))
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))

AC_betw_v

AR_betw_v <- AR_betw_v + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.6)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = betw_v_range, xlim = c(1, 4.5)) + 
  theme(
    strip.text = element_text(color = alpha("white", 0)),
    strip.background = element_blank(),
    # axis.line.x = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank()
  )+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))
AR_betw_v


ggsave(plot = AC_betw_v, "figures/postprocessing/AC_betw_v.svg", 
       width = size_output_betw[1], 
       height = size_output_betw[2], units = "in")
ggsave(plot = AR_betw_v, "figures/postprocessing/AR_betw_v.svg", 
       width = size_output_betw[1], 
       height = size_output_betw[2], units = "in")




# Attempting same panel sizes ----

# Reading in figures ----


for (f in figures) {
  obj_name <- gsub(".RDS|.rds", "", basename(f))
  assign(obj_name, readRDS(f))
}



get_range <- function(plt_a, plt_b, dv_a, dv_b, eb_a_lower = NULL, eb_a_upper = NULL, eb_b_lower = NULL, eb_b_upper = NULL){
  a_range <- range(plt_a@data[,dv_a])
  b_range <- range(plt_b@data[,dv_b])
  
  dv_range <- c(min(a_range[1], b_range[1]), max(a_range[2], b_range[2]))
  
  ranges <- list()
  
  ranges$dv <- dv_range
  ranges$error <- NA
  
  if (is.character(eb_a_lower) & is.character(eb_a_upper) & is.character(eb_b_lower) & is.character(eb_b_upper)){
    
    eb_upper_range<- range(c(plt_a@data[,eb_a_upper] %>% unlist(), plt_b@data[,eb_b_upper] %>% unlist(),
                             plt_a@data[,eb_a_lower] %>% unlist(), plt_b@data[,eb_b_lower] %>% unlist()))
    
    ranges$error <- eb_upper_range
  }
  
  return(ranges)
}





# Manipulating plots ----

## ddm ----

#figure out panel size:
size_panel_ddm <- c(size_output_ddm[1] - 0.7, size_output_ddm[2] - 0.5)

### a ----



ddm_a_range <- get_range(AC_ddm_a, AR_ddm_a, dv_a = "a_m", dv_b = "a_m", eb_a_lower = "a_ci_lower", eb_b_lower = "a_ci_lower", eb_a_upper = "a_ci_upper", eb_b_upper = "a_ci_upper")

ddm_a_range$error[1] <- ddm_a_range$error[1] - 0.01
ddm_a_range$error[2] <- ddm_a_range$error[2] + 0.01


AR_ddm_a <- AR_ddm_a +
  labs(y = "Boundary Separation")+
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.05)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = ddm_a_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))
AR_ddm_a

AC_ddm_a@layers$geom_errorbar$aes_params$linetype = "solid"

AC_ddm_a <- AC_ddm_a +
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.05)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = ddm_a_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  ) +
  labs(y = "Boundary Separation") +
  scale_color_manual(name = "Current Valence",
                     labels = c("Univalent" = "Univalent",
                                "Bivalent" = "Bivalent"),
                     values = c("Univalent" = design$curr_val$color$uni,
                                "Bivalent" = design$curr_val$color$bi))+
  scale_linetype_manual(name = "Previous Valence",
                        labels = c("Univalent" = "Univalent",
                                   "Bivalent" = "Bivalent"),
                        values = c("Univalent" = design$prev_val$linetype$uni,
                                   "Bivalent" = design$prev_val$linetype$bi))+
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))
AC_ddm_a
AR_ddm_a

AC_ddm_a@layers$geom_errorbar$geom_params$width <- design$errorbar$width
AR_ddm_a@layers$geom_errorbar$geom_params$width <- design$errorbar$width

# Force the panel to an exact size
AC_ddm_a <- set_panel_size(AC_ddm_a, width = unit(size_panel_ddm[1], "in"), height = unit(size_panel_ddm[2], "in"))
AR_ddm_a <- set_panel_size(AR_ddm_a, width = unit(size_panel_ddm[1], "in"), height = unit(size_panel_ddm[2], "in"))


ggsave(plot = AR_ddm_a, "figures/postprocessing/AR_ddm_a_2.svg", width = size_output_ddm[1], height = size_output_ddm[2], units = "in")
ggsave(plot = AC_ddm_a, "figures/postprocessing/AC_ddm_a_2.svg", width = size_output_ddm[1], height = size_output_ddm[2], units = "in")


### TER ----


ddm_Ter_range <- get_range(AC_ddm_TER, AR_ddm_TER, dv_a = "Ter_m", dv_b = "Ter_m", eb_a_lower = "Ter_ci_lower", eb_b_lower = "Ter_ci_lower", eb_a_upper = "Ter_ci_upper", eb_b_upper = "Ter_ci_upper")

ddm_Ter_range$error[1] <- ddm_Ter_range$error[1] - 0.01
ddm_Ter_range$error[2] <- ddm_Ter_range$error[2] + 0.01

AC_ddm_TER@layers$geom_errorbar$aes_params$linetype = "solid"

AC_ddm_TER <- AC_ddm_TER +
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.08)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = ddm_Ter_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  ) +
  labs(y = "Non-Decision Time") +
  scale_color_manual(name = "Current Valence",
                     labels = c("Univalent" = "Univalent",
                                "Bivalent" = "Bivalent"),
                     values = c("Univalent" = design$curr_val$color$uni,
                                "Bivalent" = design$curr_val$color$bi))+
  scale_linetype_manual(name = "Previous Valence",
                        labels = c("Univalent" = "Univalent",
                                   "Bivalent" = "Bivalent"),
                        values = c("Univalent" = design$prev_val$linetype$uni,
                                   "Bivalent" = design$prev_val$linetype$bi))+
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))

AR_ddm_TER <- AR_ddm_TER +
  labs(y = "Non-Decision Time")+
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.08)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = ddm_Ter_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))

AC_ddm_TER
AR_ddm_TER

AC_ddm_TER@layers$geom_errorbar$geom_params$width <- design$errorbar$width
AR_ddm_TER@layers$geom_errorbar$geom_params$width <- design$errorbar$width

#Force the panel to an exact size
AC_ddm_TER <- set_panel_size(AC_ddm_TER, width = unit(size_panel_ddm[1], "in"), height = unit(size_panel_ddm[2], "in"))
AR_ddm_TER <- set_panel_size(AR_ddm_TER, width = unit(size_panel_ddm[1], "in"), height = unit(size_panel_ddm[2], "in"))


ggsave(plot = AR_ddm_TER, "figures/postprocessing/AR_ddm_TER_2.svg", width = size_output_ddm[1], height = size_output_ddm[2], units = "in")
ggsave(plot = AC_ddm_TER, "figures/postprocessing/AC_ddm_TER_2.svg", width = size_output_ddm[1], height = size_output_ddm[2], units = "in")

### v ----

ddm_v_range <- get_range(AC_ddm_v, AR_ddm_v, dv_a = "v_m", dv_b = "v_m", eb_a_lower = "v_ci_lower", eb_b_lower = "v_ci_lower", eb_a_upper = "v_ci_upper", eb_b_upper = "v_ci_upper")


AC_ddm_v@layers$geom_errorbar$aes_params$linetype = "solid"

AC_ddm_v <- AC_ddm_v +
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.03)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = ddm_v_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  ) +
  labs(y = "Drift Rate") +
  scale_color_manual(name = "Current Valence",
                     labels = c("Univalent" = "Univalent",
                                "Bivalent" = "Bivalent"),
                     values = c("Univalent" = design$curr_val$color$uni,
                                "Bivalent" = design$curr_val$color$bi))+
  scale_linetype_manual(name = "Previous Valence",
                        labels = c("Univalent" = "Univalent",
                                   "Bivalent" = "Bivalent"),
                        values = c("Univalent" = design$prev_val$linetype$uni,
                                   "Bivalent" = design$prev_val$linetype$bi))+
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))

AR_ddm_v <- AR_ddm_v +
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.03)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  labs(y = "Drift Rate") +
  coord_cartesian(ylim = ddm_v_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))+
  theme(axis.title.x = element_blank(),
        legend.position = "none")


AC_ddm_v@layers$geom_errorbar$geom_params$width <- design$errorbar$width
AR_ddm_v@layers$geom_errorbar$geom_params$width <- design$errorbar$width

#Force the panel to an exact size
AC_ddm_v <- set_panel_size(AC_ddm_v, width = unit(size_panel_ddm[1], "in"), height = unit(size_panel_ddm[2], "in"))
AR_ddm_v <- set_panel_size(AR_ddm_v, width = unit(size_panel_ddm[1], "in"), height = unit(size_panel_ddm[2], "in"))


ggsave(plot = AR_ddm_v, "figures/postprocessing/AR_ddm_v_2.svg", width = size_output_ddm[1], height = size_output_ddm[2], units = "in")
ggsave(plot = AC_ddm_v, "figures/postprocessing/AC_ddm_v_2.svg", width = size_output_ddm[1], height = size_output_ddm[2], units = "in")


## Errors ----

#figure out panel size:
size_panel_exp <- c(size_output_exp[1] - 0.7, size_output_exp[2] - 0.5)


Error_range <- get_range(AC_Errors, AR_Errors, dv_a = "Error_m", dv_b = "Error_m", eb_a_lower = "Error_ci_lower", eb_b_lower = "Error_ci_lower", eb_a_upper = "Error_ci_upper", eb_b_upper = "Error_ci_upper")

AC_Errors@layers$geom_errorbar$aes_params$linetype = "solid"

AC_Errors <- AC_Errors +
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.03)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  coord_cartesian(ylim = Error_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  ) +
  scale_y_continuous(
    labels = function(x) sub("^(-?)0\\.", "\\1.", sprintf("%.2f", x))
  )+
  labs(y = "Error") +
  scale_color_manual(name = "Current Valence",
                     labels = c("Univalent" = "Univalent",
                                "Bivalent" = "Bivalent"),
                     values = c("Univalent" = design$curr_val$color$uni,
                                "Bivalent" = design$curr_val$color$bi))+
  scale_linetype_manual(name = "Previous Valence",
                        labels = c("Univalent" = "Univalent",
                                   "Bivalent" = "Bivalent"),
                        values = c("Univalent" = design$prev_val$linetype$uni,
                                   "Bivalent" = design$prev_val$linetype$bi))+
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))
AC_Errors


AR_Errors <- AR_Errors +
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.03)
      b
    },
    labels = function(x) sprintf("%.2f", x)
  ) + 
  scale_y_continuous(
    labels = function(x) sub("^(-?)0\\.", "\\1.", sprintf("%.2f", x))
  )+
  coord_cartesian(ylim = Error_range$error)+
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  ) +
  labs(y = "Error") +
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))
AR_Errors


AC_Errors@layers$geom_errorbar$geom_params$width <- design$errorbar$width
AR_Errors@layers$geom_errorbar$geom_params$width <- design$errorbar$width


AC_Errors_w_y <- AC_Errors + 
  theme(axis.ticks.y = element_line(color = alpha("black", 1)),
        axis.text.y = element_text(color = alpha("black", 1)),
        axis.title.y = element_text(color = alpha("black", 1)),
        axis.line.y = element_line(color = alpha("black", 1)))


#Force the panel to an exact size
AC_Errors <- set_panel_size(AC_Errors, width = unit(size_panel_exp[1], "in"), height = unit(size_panel_exp[2], "in"))
AR_Errors <- set_panel_size(AR_Errors, width = unit(size_panel_exp[1], "in"), height = unit(size_panel_exp[2], "in"))





ggsave(plot = AR_Errors, "figures/postprocessing/AR_Errors_2.svg", width = size_output_exp[1], height = size_output_exp[2], units = "in")
ggsave(plot = AC_Errors, "figures/postprocessing/AC_Errors_2.svg", width = size_output_exp[1], height = size_output_exp[2], units = "in")
ggsave(plot = AC_Errors_w_y, "figures/postprocessing/AC_Errors_w_y_2.svg", width = size_output_exp[1], height = size_output_exp[2], units = "in")


## log(RT) ----

logRT_range <- get_range(AC_RTlog, AR_RTlog, dv_a = "RT_m", dv_b = "RT_log_m", eb_a_lower = "RT_ci_lower", eb_b_lower = "RT_log_ci_lower", eb_a_upper = "RT_ci_upper", eb_b_upper = "RT_log_ci_upper")

AC_RTlog@layers$geom_errorbar$aes_params$linetype = "solid"

AC_RTlog <- AC_RTlog +
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.2)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = logRT_range$error)+
  labs(y = "log(RT)") +
  scale_color_manual(name = "Current Valence",
                     labels = c("Univalent" = "Univalent",
                                "Bivalent" = "Bivalent"),
                     values = c("Univalent" = design$curr_val$color$uni,
                                "Bivalent" = design$curr_val$color$bi))+
  scale_linetype_manual(name = "Previous Valence",
                        labels = c("Univalent" = "Univalent",
                                   "Bivalent" = "Bivalent"),
                        values = c("Univalent" = design$prev_val$linetype$uni,
                                   "Bivalent" = design$prev_val$linetype$bi))+
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))

AR_RTlog <- AR_RTlog +
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, max(x), by = 0.2)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = logRT_range$error)+
  labs(y = "log(RT)") +
  
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))



AC_RTlog@layers$geom_errorbar$geom_params$width <- design$errorbar$width
AR_RTlog@layers$geom_errorbar$geom_params$width <- design$errorbar$width


AC_RTlog_w_y <- AC_RTlog + 
  theme(axis.ticks.y = element_line(color = alpha("black", 1)),
        axis.text.y = element_text(color = alpha("black", 1)),
        axis.title.y = element_text(color = alpha("black", 1)),
        axis.line.y = element_line(color = alpha("black", 1)))


#Force the panel to an exact size
AC_RTlog <- set_panel_size(AC_RTlog, width = unit(size_panel_exp[1], "in"), height = unit(size_panel_exp[2], "in"))
AC_RTlog_w_y <- set_panel_size(AC_RTlog_w_y, width = unit(size_panel_exp[1], "in"), height = unit(size_panel_exp[2], "in"))
AR_RTlog <- set_panel_size(AR_RTlog, width = unit(size_panel_exp[1], "in"), height = unit(size_panel_exp[2], "in"))


ggsave(plot = AR_RTlog, "figures/postprocessing/AR_RTlog_2.svg", width = size_output_exp[1], height = size_output_exp[2], units = "in")
ggsave(plot = AC_RTlog, "figures/postprocessing/AC_RTlog_2.svg", width = size_output_exp[1], height = size_output_exp[2], units = "in")



ggsave(plot = AC_RTlog_w_y, "figures/postprocessing/AC_RTlog_w_y_2.svg", width = size_output_exp[1], height = size_output_exp[2], units = "in")


## lag RT ----
#figure out panel size:
size_panel_within <- c(size_output_within[1]/2 - 0.7, size_output_within[2] - 0.5)


lagRT_range <- get_range(AC_lagRT, AR_lagRT, dv_a = "emmean", dv_b = "emmean", eb_a_lower = "asymp.LCL", eb_b_lower = "asymp.LCL", eb_a_upper = "asymp.UCL", eb_b_upper = "asymp.UCL")

AC_lagRT@layers$geom_errorbar$aes_params$linetype = "solid"

AC_lagRT <- AC_lagRT +
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  facet_wrap(~lag1_time_log_res_c, labeller = labeller(lag1_time_log_res_c = c("-1.5" = "Lag RT Fast",
                                                                               "1.5" = "Lag RT Slow")))+
  coord_cartesian(ylim = lagRT_range$error)+
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(-5, max(x), by = 0.2)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  labs(y = "log(RT) Prediction") +
  scale_color_manual(name = "Current Valence",
                     labels = c("Univalent" = "Univalent",
                                "Bivalent" = "Bivalent"),
                     values = c("Univalent" = design$curr_val$color$uni,
                                "Bivalent" = design$curr_val$color$bi))+
  scale_linetype_manual(name = "Previous Valence",
                        labels = c("Univalent" = "Univalent",
                                   "Bivalent" = "Bivalent"),
                        values = c("Univalent" = design$prev_val$linetype$uni,
                                   "Bivalent" = design$prev_val$linetype$bi))+
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))

AR_lagRT <- AR_lagRT +
  guides(
    linetype = guide_legend(order = 1),  # appears first
    color    = guide_legend(order = 2)   # appears second
  )+
  facet_wrap(~lag1_time_log_res_w_s_int_c, labeller = labeller(lag1_time_log_res_w_s_int_c = c("-1.5" = "Lag RT Fast",
                                                                                               "1.5" = "Lag RT Slow")))+
  coord_cartesian(ylim = lagRT_range$error)+
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(-5, max(x), by = 0.2)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  labs(y = "log(RT) Prediction") +
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))


AC_lagRT@layers$geom_errorbar$geom_params$width <- design$errorbar$width
AR_lagRT@layers$geom_errorbar$geom_params$width <- design$errorbar$width


#Force the panel to an exact size
AC_lagRT <- set_panel_size(AC_lagRT, width = unit(size_panel_within[1], "in"), height = unit(size_panel_within[2], "in"))
AR_lagRT <- set_panel_size(AR_lagRT, width = unit(size_panel_within[1], "in"), height = unit(size_panel_within[2], "in"))


ggsave(plot = AC_lagRT, "figures/postprocessing/AC_lagRT_2.svg", width = size_output_within[1], height = size_output_within[2], units = "in")
ggsave(plot = AR_lagRT, "figures/postprocessing/AR_lagRT_2.svg", width = size_output_within[1], height = size_output_within[2], units = "in")



## betweeen plots ----
#text size:

#figure out panel size:
size_panel_within <- c(size_output_betw[1] - 0.7, size_output_betw[2]/4 * 0.7) #factor 0.9 for margins between facets

betw_textsize <- 3.2
betw_asterisk_size <- 6
betw_asterisk_symbol <- "*"
size_output_betw <- c(3, 5)
betw_se_color <- "grey40"
betw_se_alpha <- 0.2

### RT ----

AR_betw_RT@layers$geom_label$aes_params$size <- betw_textsize
AC_betw_RT@layers$geom_label$aes_params$size <- betw_textsize

AR_betw_RT@layers$geom_point$aes_params$size <- betw_asterisk_size
AC_betw_RT@layers$geom_point$aes_params$size <- betw_asterisk_size
AR_betw_RT@layers$geom_point$aes_params$shape <- betw_asterisk_symbol
AC_betw_RT@layers$geom_point$aes_params$shape <- betw_asterisk_symbol


AR_betw_RT@layers$geom_ribbon$aes_params$colour <- NA
AR_betw_RT@layers$geom_ribbon$aes_params$fill <- betw_se_color
AR_betw_RT@layers$geom_ribbon$aes_params$alpha <- betw_se_alpha


AC_betw_RT@layers$geom_ribbon$aes_params$colour <- NA
AC_betw_RT@layers$geom_ribbon$aes_params$fill <- betw_se_color
AC_betw_RT@layers$geom_ribbon$aes_params$alpha <- betw_se_alpha

betw_RT_range <- c(-0.25, 0.9)
AC_betw_RT

AC_betw_RT <- AC_betw_RT + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.6)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = betw_RT_range, xlim = c(1, 4.5))+ 
  theme(
    strip.text = element_text(color = alpha("white", 0)),
    strip.background = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))
AC_betw_RT

AR_betw_RT <- AR_betw_RT + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.6)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = betw_RT_range, xlim = c(1, 4.5)) + 
  theme(
    strip.text = element_text(color = alpha("white", 0)),
    strip.background = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank()
  )+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))

AR_betw_RT



#Force the panel to an exact size
AC_betw_RT <- set_panel_size(AC_betw_RT, width = unit(size_panel_within[1], "in"), height = unit(size_panel_within[2], "in"))
AR_betw_RT <- set_panel_size(AR_betw_RT, width = unit(size_panel_within[1], "in"), height = unit(size_panel_within[2], "in"))


ggsave(plot = AR_betw_RT, "figures/postprocessing/AR_betw_RT_2.svg", width = size_output_betw[1], height = size_output_betw[2], units = "in")
ggsave(plot = AC_betw_RT, "figures/postprocessing/AC_betw_RT_2.svg", width = size_output_betw[1], height = size_output_betw[2], units = "in")




### Error ----

AR_betw_Error@layers$geom_label$aes_params$size <- betw_textsize
AC_betw_Error@layers$geom_label$aes_params$size <- betw_textsize

AR_betw_Error@layers$geom_point$aes_params$size <- betw_asterisk_size
AC_betw_Error@layers$geom_point$aes_params$size <- betw_asterisk_size

AR_betw_Error@layers$geom_point$aes_params$shape <- betw_asterisk_symbol
AC_betw_Error@layers$geom_point$aes_params$shape <- betw_asterisk_symbol

AR_betw_Error@layers$geom_ribbon$aes_params$colour <- NA
AR_betw_Error@layers$geom_ribbon$aes_params$fill <- betw_se_color
AR_betw_Error@layers$geom_ribbon$aes_params$alpha <- betw_se_alpha
AC_betw_Error@layers$geom_ribbon$aes_params$colour <- NA
AC_betw_Error@layers$geom_ribbon$aes_params$fill <- betw_se_color
AC_betw_Error@layers$geom_ribbon$aes_params$alpha <- betw_se_alpha

betw_Error_range <- c(-0.25, 0.9)

AC_betw_Error <- AC_betw_Error + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.6)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = betw_Error_range, xlim = c(1, 4.5)) + 
  theme(
    strip.text = element_text(color = alpha("white", 0)),
    strip.background = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))

AC_betw_Error

AR_betw_Error <- AR_betw_Error + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.6)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = betw_Error_range, xlim = c(1, 4.5)) + 
  theme(
    strip.text = element_text(color = alpha("white", 0)),
    strip.background = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank()
  )+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))
AR_betw_Error


#Force the panel to an exact size
AC_betw_Error <- set_panel_size(AC_betw_Error, width = unit(size_panel_within[1], "in"), height = unit(size_panel_within[2], "in"))
AR_betw_Error <- set_panel_size(AR_betw_Error, width = unit(size_panel_within[1], "in"), height = unit(size_panel_within[2], "in"))


ggsave(plot = AR_betw_Error, "figures/postprocessing/AR_betw_Error_2.svg", width = size_output_betw[1], height = size_output_betw[2], units = "in")
ggsave(plot = AC_betw_Error, "figures/postprocessing/AC_betw_Error_2.svg", width = size_output_betw[1], height = size_output_betw[2], units = "in")


### v ----


AR_betw_v@layers$geom_label$aes_params$size <- betw_textsize
AC_betw_v@layers$geom_label$aes_params$size <- betw_textsize

AR_betw_v@layers$geom_point$aes_params$size <- betw_asterisk_size
AC_betw_v@layers$geom_point$aes_params$size <- betw_asterisk_size

AR_betw_v@layers$geom_point$aes_params$shape <- betw_asterisk_symbol
AC_betw_v@layers$geom_point$aes_params$shape <- betw_asterisk_symbol

AR_betw_v@layers$geom_ribbon$aes_params$colour <- NA
AR_betw_v@layers$geom_ribbon$aes_params$fill <- betw_se_color
AR_betw_v@layers$geom_ribbon$aes_params$alpha <- betw_se_alpha
AC_betw_v@layers$geom_ribbon$aes_params$colour <- NA
AC_betw_v@layers$geom_ribbon$aes_params$fill <- betw_se_color
AC_betw_v@layers$geom_ribbon$aes_params$alpha <- betw_se_alpha

betw_v_range <- c(-0.25, 0.9)

AC_betw_v <- AC_betw_v + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.6)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = betw_v_range, xlim = c(1, 4.5)) + 
  theme(
    strip.text = element_text(color = alpha("white", 0)),
    strip.background = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "none"
  )+
  theme(plot.background = element_rect(fill = AC_plt_background, color = AC_plt_background), 
        panel.background = element_rect(fill = AC_plt_background, color = AC_plt_background))

AC_betw_v

AR_betw_v <- AR_betw_v + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.6)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = betw_v_range, xlim = c(1, 4.5)) + 
  theme(
    strip.text = element_text(color = alpha("white", 0)),
    strip.background = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank()
  )+
  theme(plot.background = element_rect(fill = AR_plt_background, color = AR_plt_background), 
        panel.background = element_rect(fill = AR_plt_background, color = AR_plt_background))
AR_betw_v


#Force the panel to an exact size
AC_betw_v <- set_panel_size(AC_betw_v, width = unit(size_panel_within[1], "in"), height = unit(size_panel_within[2], "in"))
AR_betw_v <- set_panel_size(AR_betw_v, width = unit(size_panel_within[1], "in"), height = unit(size_panel_within[2], "in"))


ggsave(plot = AR_betw_v, "figures/postprocessing/AR_betw_v_2.svg", width = size_output_betw[1], height = size_output_betw[2], units = "in")
ggsave(plot = AC_betw_v, "figures/postprocessing/AC_betw_v_2.svg", width = size_output_betw[1], height = size_output_betw[2], units = "in")







