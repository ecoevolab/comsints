setwd("~/micropopgen/exp/2025/today3/")
library(tidyverse)
library(lme4)
library(brms)


load("2025-03-11.data_for_aplicaciones_lcg/models.rdat")
Dat <- read_tsv("2025-03-11.data_for_aplicaciones_lcg/pilot_dat_perfect_design.tsv") %>%
  mutate(community = factor(community, levels = paste0("R", 1:12)))
dat <- Dat %>%
  filter(hrs == 24) 

summary(m5.t1)

#' Get the effect of community
b_com <- ranef(m5.t1, condVar = TRUE, whichel = "b_com", postVar = TRUE)
res <- tibble(effect = row.names(b_com$b_com),
              est = b_com$b_com[,1],
              postVar = attr(b_com$b_com, "postVar")[,,]) %>%
  mutate(lower = qnorm(p = 0.025, mean = est, sd = sqrt(postVar)),
         upper = qnorm(p = 0.975, mean = est, sd = sqrt(postVar)),
         pval = 2 * pnorm(q = -abs(est), mean = 0, sd = sqrt(postVar))) %>%
  mutate(qval = p.adjust(pval)) %>%
  arrange(est) %>%
  mutate(effect = factor(effect, levels = effect)) %>%
  print(n = 100)
res

res %>%
  ggplot(aes(x = est, y = effect)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  theme_classic()


dat %>%
  filter(strain == "ST00046") %>%
  filter(added == 1) %>%
  # filter(community %in% c("R1", "R2", "R3", "R4", "R5", "R12")) %>%
  mutate(effect = as.character(community)) %>%
  mutate(effect = replace(effect, effect %in% c("R1", "R2", "R3", "R4"), "pos")) %>%
  mutate(effect = replace(effect, effect %in% c("R5", "R12"), "neg")) %>%
  ggplot(aes(col = factor(temp))) +
  facet_wrap(~effect + temp) +
  geom_segment(aes(x = 0, y = i_freq, xend = hrs, yend = count / depth, linetype = exp )) +
  ggtitle(label = "ST00046") +
  theme_classic() 




dat %>%
  filter(strain == "ST00046") %>%
  filter(added == 1) %>%
  filter(community == "R5") %>%
  # filter(community %in% c("R1", "R2", "R3", "R4", "R5", "R12")) %>%
  mutate(effect = as.character(community)) %>%
  mutate(effect = replace(effect, effect %in% c("R1", "R2", "R3", "R4"), "pos")) %>%
  mutate(effect = replace(effect, effect %in% c("R5", "R12"), "neg")) %>%
  ggplot(aes(col = factor(temp))) +
  facet_wrap(~temp) +
  geom_segment(aes(x = 0, y = i_freq, xend = hrs, yend = count / depth, linetype = exp )) +
  ggtitle(label = "ST00046") +
  theme_classic() 

#' Get the effect of temp
b_temp <- ranef(m5.t1, condVar = TRUE, whichel = "b_temp", postVar = TRUE)
res <- tibble(effect = row.names(b_temp$b_temp),
              est = b_temp$b_temp[,1],
              postVar = attr(b_temp$b_temp, "postVar")[,,]) %>%
  mutate(lower = qnorm(p = 0.025, mean = est, sd = sqrt(postVar)),
         upper = qnorm(p = 0.975, mean = est, sd = sqrt(postVar)),
         pval = 2 * pnorm(q = -abs(est), mean = 0, sd = sqrt(postVar))) %>%
  mutate(qval = p.adjust(pval)) %>%
  arrange(est) %>%
  mutate(effect = factor(effect, levels = effect))
res

res %>%
  ggplot(aes(x = est, y = effect)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  theme_classic()


#' Example of ST00046 showing that it decreases more at 28 than 32 (not significant)
dat %>%
  filter(strain == "ST00110") %>%
  filter(added == 1) %>%
  filter(hrs %in% c(24)) %>%
  ggplot(aes(col = factor(temp))) +
  facet_wrap(~temp) +
  geom_segment(aes(x = 0, y = i_freq, xend = hrs, yend = count / depth, linetype = exp )) +
  ggtitle(label = "ST00110") +
  theme_classic() 



b_temp_com <- ranef(m5.t1, condVar = TRUE, whichel = "b_temp_com", postVar = TRUE)
res <- tibble(effect = row.names(b_temp_com$b_temp_com),
              est = b_temp_com$b_temp_com[,1],
              postVar = attr(b_temp_com$b_temp_com, "postVar")[,,]) %>%
  mutate(lower = qnorm(p = 0.025, mean = est, sd = sqrt(postVar)),
         upper = qnorm(p = 0.975, mean = est, sd = sqrt(postVar)),
         pval = 2 * pnorm(q = -abs(est), mean = 0, sd = sqrt(postVar))) %>%
  mutate(qval = p.adjust(pval)) %>%
  arrange(est) %>%
  mutate(effect = factor(effect, levels = effect))
res %>%
  print(n = 1000)


res %>%
  ggplot(aes(x = est, y = effect)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  theme_classic()




dat %>%
  filter(strain == "ST00143") %>%
  filter(added == 1) %>%
  filter(community == "R10") %>%
  ggplot(aes(col = factor(temp))) +
  geom_segment(aes(x = 0, y = i_freq, xend = hrs, yend = count / depth, linetype = exp )) +
  ggtitle(label = "ST00143") +
  theme_classic() 
