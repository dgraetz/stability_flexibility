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
alt_cue <- import("../data/AlternateCues.csv")

exclude_subjects <- alt_cue %>%
  group_by(id) %>% 
  summarize(acc = mean(1-error),
            N = n())

alt_cue <- alt_cue %>%
  group_by(id, block) %>%
  mutate(trialgroup = ifelse(cycle == 1, 1, 0),
         trialgroup = cumsum(trialgroup),
         is_neutral = ifelse(dim1 == 4 | dim2 == 4, 1, 0),
         was_prev_neutral = lag(is_neutral, 1),
         was_prev_error = lag(error, 1),
         prev_task = lag(task, 1),
         switch = ifelse(prev_task == task, 0, 1)) %>%
  group_by(id, block, trialgroup) %>%
  mutate(was_prev_neutral = was_prev_neutral[1],
         switch = switch[1]) %>%
  ungroup() %>%
  mutate(id = factor(id),
         was_prev_neutral = factor(was_prev_neutral),
         is_neutral = factor(is_neutral),
         cycle = factor(cycle),
         switch = factor(switch)) %>%
  filter(trialgroup != 1, block != 0) %>%
  group_by(id, was_prev_neutral, is_neutral, cycle) %>%
  mutate(RT_z = (time - mean(time[error == 0 & was_prev_error == 0]))/ sd(time[error == 0 & was_prev_error == 0]),
         RT_outlier = ifelse(abs(RT_z) > 3, 1, 0))

alt_cue_agg <- alt_cue %>%
  group_by(id, is_neutral, was_prev_neutral, switch, cycle) %>%
  summarize(RT = mean(time, na.rm = TRUE)) %>%
  normWS(s = "id", dv = "RT") %>%
  mutate(RT = DV_n) %>%
  group_by(is_neutral, was_prev_neutral, switch, cycle) %>%
  summarize(RT_m = mean(RT, na.rm = TRUE),
            RT_sd = sd(RT, na.rm = TRUE),
            N = n(),
            SE = RT_sd/sqrt(N),
            CI = abs(qt(0.025, df = N-1))*SE)

pos <- position_dodge(width = 0.1)

ggplot(alt_cue_agg, aes(x = cycle, y = RT_m, color = is_neutral, linetype = was_prev_neutral, group = interaction(is_neutral, was_prev_neutral)))+
  geom_line(position = pos)+
  geom_errorbar(aes(ymin = RT_m-CI, ymax = RT_m+CI), width = 0.1, position = pos)+
  theme_classic()


lmer(time ~ was_prev_neutral*is_neutral+(was_prev_neutral*is_neutral|id), data = alt_cue %>% filter(cycle == 1)) %>% summary()
lmer(time ~ was_prev_neutral*is_neutral+(was_prev_neutral*is_neutral|id), data = alt_cue %>% filter(cycle == 2)) %>% summary()
lmer(time ~ was_prev_neutral*is_neutral+(was_prev_neutral*is_neutral|id), data = alt_cue %>% filter(cycle == 3)) %>% summary()

lmer(time ~ was_prev_neutral*is_neutral*cycle+(was_prev_neutral*is_neutral*cycle|id), data = alt_cue) %>% summary()

intercept <- lmer(time ~ was_prev_neutral*is_neutral+(1|id), data = alt_cue %>% filter(cycle == 1))
add <- lmer(time ~ was_prev_neutral*is_neutral+(was_prev_neutral+is_neutral|id), data = alt_cue %>% filter(cycle == 1))
multi <- lmer(time ~ was_prev_neutral*is_neutral+(was_prev_neutral*is_neutral|id), data = alt_cue %>% filter(cycle == 1))

anova(intercept, add, multi)




