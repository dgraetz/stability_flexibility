library(tidyverse)
library(rio)
library(lmerTest)

normWS <- function(d ,s, dv, between = NULL) {
  eval(substitute(d %>%
                    dplyr::group_by(s) %>%
                    dplyr::mutate(sAV = mean(dv, na.rm = TRUE)) %>%
                    dplyr::group_by_at(between) %>% #this is grouping by between subjects variables
                    dplyr::mutate(gAV = mean(dv, na.rm = TRUE)) %>%
                    dplyr::mutate(DV_n = dv - sAV + gAV), list(s = as.name(s), dv = as.name(dv))))
}

#Alternate Cues
alt_run <- import("../data/AlternateRuns.csv")

alt_run %>% group_by(id, block) %>% summarize(dur = sum(time)/1000,
                                              max = n())

#this is code with trial sub groups, keeping the cycle variable from 1-8
# alt_run2 <- alt_run %>%
#   group_by(id, block) %>%
#   mutate(trialgroup = NA,
#          trialgroup = ifelse(cycle == 1, 1, 0),
#          trialgroup = cumsum(trialgroup),
#          trialgroup_sub = NA,
#          trialgroup_sub = ifelse(cycle == 1, 1, NA), #first half
#          trialgroup_sub = ifelse(cycle == 5, 2, trialgroup_sub), #second half
#          is_neutral = ifelse(dim1 == 4 | dim2 == 4, 1, 0),
#          was_prev_neutral = lag(is_neutral, 1),
#          was_prev_error = lag(error, 1),
#          prev_task = lag(task, 1),
#          switch = ifelse(cycle %in% c(1, 5), 1, 0)) %>%
#   fill(trialgroup_sub) %>%
#   group_by(id, block, trialgroup, trialgroup_sub) %>%
#   mutate(was_prev_neutral = was_prev_neutral[1]) %>%
#   mutate(id = factor(id),
#          was_prev_neutral = factor(was_prev_neutral),
#          is_neutral = factor(is_neutral),
#          cycle = factor(cycle),
#          switch = factor(switch)) %>%
#   filter(trialgroup != 1, block != 0, error == 0, was_prev_error == 0)

alt_run <- alt_run %>%
  group_by(id, block) %>%
  mutate(cycle = ifelse(cycle > 4, cycle - 4, cycle), #transforming cycle variable to the range of 1-4
         trialgroup = ifelse(cycle == 1, 1, 0),
         trialgroup = cumsum(trialgroup),
         is_neutral = ifelse(dim1 == 4 | dim2 == 4, 1, 0),
         was_prev_neutral = lag(is_neutral, 1),
         was_prev_error = lag(error, 1),
         prev_task = lag(task, 1)) %>%
  group_by(id, block, trialgroup) %>%
  mutate(was_prev_neutral = was_prev_neutral[1]) %>%
  mutate(id = factor(id),
         was_prev_neutral = factor(was_prev_neutral, levels = c("0", "1")),
         is_neutral = factor(is_neutral, levels = c("0", "1")),
         cycle = factor(cycle)) %>%
  filter(trialgroup != 1, block != 0, error == 0, was_prev_error == 0)

alt_run_agg <- alt_run %>%
  group_by(id, is_neutral, was_prev_neutral, cycle) %>%
  summarize(RT = mean(time, na.rm = TRUE)) %>%
  normWS(s = "id", dv = "RT") %>%
  mutate(RT = DV_n) %>%
  group_by(is_neutral, was_prev_neutral, cycle) %>%
  summarize(RT_m = mean(RT, na.rm = TRUE),
            RT_sd = sd(RT, na.rm = TRUE),
            N = n(),
            SE = RT_sd/sqrt(N),
            CI = abs(qt(0.025, df = N-1))*SE)
  

pos <- position_dodge(width = 0.1)

ggplot(alt_run_agg, aes(x = cycle, y = RT_m, color = is_neutral, linetype = was_prev_neutral, group = interaction(is_neutral, was_prev_neutral)))+
  geom_line(position = pos)+
  geom_errorbar(aes(ymin = RT_m-CI, ymax = RT_m+CI), width = 0.1, position = pos)+
  theme_classic()
