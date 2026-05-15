library(tidyverse)
source("theme.R")

figures <- list.files("figures/", pattern = "RDS|rds", full.names = TRUE)


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
        legend.position = "none")


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
        axis.ticks.y = element_line(color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.line.y = element_line(color = "white"),
        legend.position = "none"
        )
AC_ddm_a
AR_ddm_a

ggsave(plot = AR_ddm_a, "figures/postprocessing/AR_ddm_a.svg", width = 3, height = 3, units = "in")
ggsave(plot = AC_ddm_a, "figures/postprocessing/AC_ddm_a.svg", width = 3, height = 3, units = "in")

ggsave(plot = AC_ddm_a + theme(legend.position = "bottom"), "figures/postprocessing/legend_bottom.svg", width = 10, height = 3, units = "in")
ggsave(plot = AC_ddm_a + theme(legend.position = "right"), "figures/postprocessing/legend_right.svg", width = 10, height = 3, units = "in")

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
        axis.ticks.y = element_line(color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.line.y = element_line(color = "white"),
        legend.position = "none"
  )

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
        legend.position = "none")


ggsave(plot = AC_ddm_TER, "figures/postprocessing/AC_ddm_TER.svg", width = 3, height = 3, units = "in")
ggsave(plot = AR_ddm_TER, "figures/postprocessing/AR_ddm_TER.svg", width = 3, height = 3, units = "in")

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
        legend.position = "none")

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
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.line.y = element_line(color = "white"),
        legend.position = "none"
  )



ggsave(plot = AC_ddm_v, "figures/postprocessing/AC_ddm_v.svg", width = 3, height = 3, units = "in")
ggsave(plot = AR_ddm_v, "figures/postprocessing/AR_ddm_v.svg", width = 3, height = 3, units = "in")


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
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.line.y = element_line(color = "white"),
        legend.position = "none"
  )
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
  labs(y = "Error") +
  theme(axis.title.x = element_blank(),
        legend.position = "none")
AR_Errors

ggsave(plot = AC_Errors, "figures/postprocessing/AC_Errors.svg", width = 3, height = 3, units = "in")
ggsave(plot = AR_Errors, "figures/postprocessing/AR_Errors.svg", width = 3, height = 3, units = "in")

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
        axis.ticks.y = element_line(color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.line.y = element_line(color = "white"),
        legend.position = "none"
  )

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
        legend.position = "none")

ggsave(plot = AC_RTlog, "figures/postprocessing/AC_RTlog.svg", width = 3, height = 3, units = "in")
ggsave(plot = AR_RTlog, "figures/postprocessing/AR_RTlog.svg", width = 3, height = 3, units = "in")


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
        axis.ticks.y = element_line(color = "white"),
        axis.text.y = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.line.y = element_line(color = "white"),
        legend.position = "none"
  )

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
        legend.position = "none")

ggsave(plot = AC_lagRT, "figures/postprocessing/AC_lagRT.svg", width = 4, height = 3.5, units = "in")
ggsave(plot = AR_lagRT, "figures/postprocessing/AR_lagRT.svg", width = 4, height = 3.5, units = "in")




## betweeen plots ----
#text size:

betw_textsize <- 3.2
betw_asterisk_size <- 6
betw_asterisk_symbol <- "*"
betw_output_size <- c(3, 5)


### RT ----

AR_betw_RT@layers$geom_label$aes_params$size <- betw_textsize
AC_betw_RT@layers$geom_label$aes_params$size <- betw_textsize

AR_betw_RT@layers$geom_point$aes_params$size <- betw_asterisk_size
AC_betw_RT@layers$geom_point$aes_params$size <- betw_asterisk_size
AR_betw_RT@layers$geom_point$aes_params$shape <- betw_asterisk_symbol
AC_betw_RT@layers$geom_point$aes_params$shape <- betw_asterisk_symbol


betw_RT_range <- c(-0.1, 0.8)


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
    strip.text = element_text(color = "white"),
    strip.background = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_text(color = "white"),
    axis.ticks.y = element_line(color = "white")
  )
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
    strip.text = element_text(color = "white"),
    strip.background = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_blank()
  )

AR_betw_RT


ggsave(plot = AC_betw_RT, "figures/postprocessing/AC_betw_RT.svg", 
       width = betw_output_size[1], 
       height = betw_output_size[2], units = "in")
ggsave(plot = AR_betw_RT, "figures/postprocessing/AR_betw_RT.svg", 
       width = betw_output_size[1], 
       height = betw_output_size[2], units = "in")




### Error ----

AR_betw_Error@layers$geom_label$aes_params$size <- betw_textsize
AC_betw_Error@layers$geom_label$aes_params$size <- betw_textsize

AR_betw_Error@layers$geom_point$aes_params$size <- betw_asterisk_size
AC_betw_Error@layers$geom_point$aes_params$size <- betw_asterisk_size

AR_betw_Error@layers$geom_point$aes_params$shape <- betw_asterisk_symbol
AC_betw_Error@layers$geom_point$aes_params$shape <- betw_asterisk_symbol

betw_Error_range <- c(-0.1, 0.8)

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
    strip.text = element_text(color = "white"),
    strip.background = element_blank(),
    # axis.line.x = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_text(color = "white"),
    axis.ticks.y = element_line(color = "white")
  )

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
    strip.text = element_text(color = "white"),
    strip.background = element_blank(),
    # axis.line.x = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank()
  )
AR_betw_Error


ggsave(plot = AC_betw_Error, "figures/postprocessing/AC_betw_Error.svg", 
       width = betw_output_size[1], 
       height = betw_output_size[2], units = "in")
ggsave(plot = AR_betw_Error, "figures/postprocessing/AR_betw_Error.svg", 
       width = betw_output_size[1], 
       height = betw_output_size[2], units = "in")



### v ----


AR_betw_v@layers$geom_label$aes_params$size <- betw_textsize
AC_betw_v@layers$geom_label$aes_params$size <- betw_textsize

AR_betw_v@layers$geom_point$aes_params$size <- betw_asterisk_size
AC_betw_v@layers$geom_point$aes_params$size <- betw_asterisk_size

AR_betw_v@layers$geom_point$aes_params$shape <- betw_asterisk_symbol
AC_betw_v@layers$geom_point$aes_params$shape <- betw_asterisk_symbol

betw_v_range <- c(-0.1, 0.8)

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
    strip.text = element_text(color = "white"),
    strip.background = element_blank(),
    # axis.line.x = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_text(color = "white"),
    axis.ticks.y = element_line(color = "white")
  )

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
    strip.text = element_text(color = "white"),
    strip.background = element_blank(),
    # axis.line.x = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank()
  )
AR_betw_v


ggsave(plot = AC_betw_Error, "figures/postprocessing/AC_betw_Error.svg", 
       width = betw_output_size[1], 
       height = betw_output_size[2], units = "in")
ggsave(plot = AR_betw_Error, "figures/postprocessing/AR_betw_Error.svg", 
       width = betw_output_size[1], 
       height = betw_output_size[2], units = "in")




ggsave(plot = AC_betw_v, "figures/postprocessing/AC_betw_v.svg", width = 3.5, height = 4.3, units = "in")
ggsave(plot = AR_betw_v, "figures/postprocessing/AR_betw_v.svg", width = 3.5, height = 4.3, units = "in")




