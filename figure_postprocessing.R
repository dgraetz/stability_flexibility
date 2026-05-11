figures <- list.files("figures/", pattern = "RDS|rds", full.names = TRUE)

source("theme.R")

# Reading in data ----

for (f in figures) {
  obj_name <- gsub(".RDS|.rds", "", basename(f))
  assign(obj_name, readRDS(f))
}



# Manipulating plots ----


ggsave(AC_ddm_a, "figures/postprocessing/AC_ddm_a.svg", width = 5, height = 5, units = "in")
ggsave(AR_ddm_a, "figures/postprocessing/AR_ddm_a.svg", width = 5, height = 5, units = "in")

ggsave(AC_ddm_TER, "figures/postprocessing/AC_ddm_TER.svg", width = 5, height = 5, units = "in")
ggsave(AR_ddm_TER, "figures/postprocessing/AR_ddm_TER.svg", width = 5, height = 5, units = "in")

ggsave(AC_ddm_v, "figures/postprocessing/AC_ddm_v.svg", width = 5, height = 5, units = "in")
ggsave(AR_ddm_v, "figures/postprocessing/AR_ddm_v.svg", width = 5, height = 5, units = "in")

ggsave(AC_Errors, "figures/postprocessing/AC_Errors.svg", width = 5, height = 5, units = "in")
ggsave(AR_Errors, "figures/postprocessing/AR_Errors.svg", width = 5, height = 5, units = "in")

ggsave(AC_lagRT, "figures/postprocessing/AC_lagRT.svg", width = 5, height = 5, units = "in")
ggsave(AR_lagRT, "figures/postprocessing/AR_lagRT.svg", width = 5, height = 5, units = "in")


ggsave(AC_RTlog, "figures/postprocessing/AC_RTlog.svg", width = 5, height = 5, units = "in")
ggsave(AR_RTlog, "figures/postprocessing/AR_RTlog.svg", width = 5, height = 5, units = "in")
