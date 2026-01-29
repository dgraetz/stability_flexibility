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
alt_cue <- import("data/AlternateCues.csv")

exclude_subjects <- alt_cue %>%
  group_by(id) %>% 
  summarize(acc = mean(1-error),
            N = n())

exclude_subjects <- exclude_subjects %>%
  filter(acc < 0.8)

exclude_subjects <- c(exclude_subjects$id, 201, 901, 902, 959, 975, 4)

alt_cue <- alt_cue %>%
  filter(!id %in% exclude_subjects) %>%
  group_by(id, block) %>%
  mutate(trialgroup = ifelse(cycle == 1, 1, 0),
         trialgroup = cumsum(trialgroup),
         is_bivalent = ifelse(dim1 == 4 | dim2 == 4, 0, 1),
         was_prev_bivalent = lag(is_bivalent, 1),
         was_prev_error = lag(error, 1),
         prev_task = lag(task, 1),
         switch = ifelse(prev_task == task, 0, 1)) %>%
  group_by(id, block, trialgroup) %>%
  mutate(was_prev_bivalent = was_prev_bivalent[1]) %>%
  ungroup() %>%
  mutate(id = factor(id),
         was_prev_bivalent = factor(was_prev_bivalent),
         is_bivalent = factor(is_bivalent),
         cycle = factor(cycle),
         switch = factor(switch),
         was_prev_bivalent_contr = case_when(was_prev_bivalent == 1 ~ 0.5,
                                              was_prev_bivalent == 0 ~ -0.5),
         is_bivalent_contr = case_when(is_bivalent == 1 ~ 0.5,
                                        is_bivalent == 0 ~ -0.5),
         switch_contr = case_when(switch == 1 ~ 0,
                                  switch == 0 ~ 1)) %>%
  filter(trialgroup != 1, block != 0) %>%
  group_by(id, was_prev_bivalent, is_bivalent, cycle) %>%
  mutate(time_z = scale(time - mean(time[error == 0 & was_prev_error == 0])) %>% as.vector(),
         time_outlier = ifelse(abs(time_z) > 3, 1, 0),
         filter_correct = ifelse(error == 1 & was_prev_error == 1, 1, 0),
         time_log = log(time)) %>%
  filter(time_outlier == 0)

alt_cue_agg <- alt_cue %>%
  group_by(id, is_bivalent, was_prev_bivalent, switch, cycle) %>%
  summarize(RT = mean(time[filter_correct == 0], na.rm = TRUE)) %>%
  normWS(s = "id", dv = "RT") %>%
  mutate(RT = DV_n) %>%
  group_by(is_bivalent, was_prev_bivalent, switch, cycle) %>%
  summarize(RT_m = mean(RT, na.rm = TRUE),
            RT_sd = sd(RT, na.rm = TRUE),
            N = n(),
            SE = RT_sd/sqrt(N),
            CI = abs(qt(0.025, df = N-1))*SE)

pos <- position_dodge(width = 0.1)

ggplot(alt_cue_agg, aes(x = cycle, y = RT_m, color = is_bivalent, linetype = was_prev_bivalent, group = interaction(is_bivalent, was_prev_bivalent)))+
  geom_line(position = pos)+
  geom_errorbar(aes(ymin = RT_m-CI, ymax = RT_m+CI), width = 0.1, position = pos)+
  theme_classic()



m1 <- lmer(time ~ is_bivalent_contr * was_prev_bivalent_contr * switch_contr + (is_bivalent_contr + was_prev_bivalent_contr + switch_contr||id), 
     alt_cue %>% 
       filter(filter_correct == 0))

summary(m1)


