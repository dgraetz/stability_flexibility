figures <- list.files("figures/", pattern = "RDS|rds", full.names = TRUE)

source("theme.R")

# Reading in figures ----

for (f in figures) {
  obj_name <- gsub(".RDS|.rds", "", basename(f))
  assign(obj_name, readRDS(f))
}






# Manipulating plots ----

AR_ddm_a +
  labs(y = "Boundary Separation")

ggsave(plot = AC_ddm_a, "figures/postprocessing/AC_ddm_a.svg", width = 5, height = 5, units = "in")
ggsave(plot = AR_ddm_a, "figures/postprocessing/AR_ddm_a.svg", width = 5, height = 5, units = "in")

AR_ddm_TER +
  labs(y = "Non-Decision Time")


ggsave(plot = AC_ddm_TER, "figures/postprocessing/AC_ddm_TER.svg", width = 5, height = 5, units = "in")
ggsave(plot = AR_ddm_TER, "figures/postprocessing/AR_ddm_TER.svg", width = 5, height = 5, units = "in")

AR_ddm_v +
  labs(y = "Drift Rate")

ggsave(plot = AC_ddm_v, "figures/postprocessing/AC_ddm_v.svg", width = 5, height = 5, units = "in")
ggsave(plot = AR_ddm_v, "figures/postprocessing/AR_ddm_v.svg", width = 5, height = 5, units = "in")

ggsave(plot = AC_Errors, "figures/postprocessing/AC_Errors.svg", width = 5, height = 5, units = "in")
ggsave(plot = AR_Errors, "figures/postprocessing/AR_Errors.svg", width = 5, height = 5, units = "in")

ggsave(plot = AC_lagRT, "figures/postprocessing/AC_lagRT.svg", width = 5, height = 5, units = "in")
ggsave(plot = AR_lagRT, "figures/postprocessing/AR_lagRT.svg", width = 5, height = 5, units = "in")

ggsave(plot = AC_RTlog, "figures/postprocessing/AC_RTlog.svg", width = 5, height = 5, units = "in")
ggsave(plot = AR_RTlog, "figures/postprocessing/AR_RTlog.svg", width = 5, height = 5, units = "in")


### betweeen plots
#text size:

betw_textsize <- 3.2
asterisk_size <- 6

AR_betw_Error@layers$geom_label$aes_params$size <- betw_textsize
AC_betw_Error@layers$geom_label$aes_params$size <- betw_textsize

AR_betw_Error@layers$geom_point$aes_params$size <- asterisk_size
AC_betw_Error@layers$geom_point$aes_params$size <- asterisk_size

AR_betw_Error_range <- AR_betw_Error@data$estimate %>% range()
AC_betw_Error_range <- AC_betw_Error@data$estimate %>% range()

betw_Error_range <- c(min(c(AR_betw_Error_range[1], AC_betw_Error_range[1])), max(c(AR_betw_Error_range[2], AC_betw_Error_range[2])))
betw_Error_range[1] <- -0.2
betw_Error_range[2] <- betw_Error_range[2] + 0.2

AC_betw_Error <- AC_betw_Error + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.5)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = c(-0.2, betw_Error_range[2]), xlim = c(1, 4.5))+ 
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

AC_betw_Error

AR_betw_Error <- AR_betw_Error + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.5)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) + 
  coord_cartesian(ylim = c(-0.2, betw_Error_range[2]), xlim = c(1, 4.5)) + 
  theme(
    strip.text = element_text(color = "white"),
    strip.background = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank()
  )
AR_betw_Error


ggsave(plot = AC_betw_Error, "figures/postprocessing/AC_betw_Error.svg", width = 3, height = 5, units = "in")
ggsave(plot = AR_betw_Error, "figures/postprocessing/AR_betw_Error.svg", width = 3, height = 5, units = "in")

AC_betw_RT <- AC_betw_RT + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.5)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  )

AR_betw_RT <- AR_betw_RT + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.5)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  )

AR_betw_RT@layers$geom_label$aes_params$size <- betw_textsize
AC_betw_RT@layers$geom_label$aes_params$size <- betw_textsize

ggsave(plot = AC_betw_RT, "figures/postprocessing/AC_betw_RT.svg", width = 3.5, height = 4.3, units = "in")
ggsave(plot = AR_betw_RT, "figures/postprocessing/AR_betw_RT.svg", width = 3.5, height = 4.3, units = "in")

AC_betw_v <- AC_betw_v + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.5)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  ) 


AR_betw_v@layers$geom_label$aes_params$size <- betw_textsize
AC_betw_v@layers$geom_label$aes_params$size <- betw_textsize


AR_betw_v <- AR_betw_v + 
  scale_y_continuous(
    breaks = function(x) {
      b <- seq(0, ceiling(max(x) * 5) / 5, by = 0.5)
      b
    },
    labels = function(x) sprintf("%.1f", x)
  )


ggsave(plot = AC_betw_v, "figures/postprocessing/AC_betw_v.svg", width = 3.5, height = 4.3, units = "in")
ggsave(plot = AR_betw_v, "figures/postprocessing/AR_betw_v.svg", width = 3.5, height = 4.3, units = "in")




