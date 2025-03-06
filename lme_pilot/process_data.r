setwd("/Users/sur/lab/exp/2025/today")

library(tidyverse)

Tab <- read_csv("2025-03-06.ancom_data/ancom_data/freq.csv") %>%
  rename(strain = 1)
Tab

Meta <- read_csv("2025-03-06.ancom_data/ancom_data/meta.csv") %>%
  rename(id = 1)
Meta



syncoms <- read_tsv("../../../data/2024_rhizo_pilot_syncom_NS/NS1/syncoms.tsv")

#' Select two communities to test

# %>% select(community, ST00046, ST00154, ST00101, ST00109, ST00042, ST00060

meta <- Meta %>%
  filter(community %in% c("R1", "R2")) %>%
  left_join(syncoms %>%
              select(strain, R1,R2) %>%
              pivot_longer(-strain, names_to = "community", values_to = "presence") %>%
              pivot_wider(names_from = "strain", values_from = "presence"),
            by = "community")

meta %>% 
  print(n = 100)

meta <- meta %>%
  pivot_longer(-c("id", "community", "hrs", "techrep", "exp", "temp", "color_comsint", "community_temp"),
               names_to = "strain", values_to = "added") %>%
  mutate(added = replace_na(added, 0))

meta %>% 
  print(n = 100)


strains <- Tab$strain
strains <- strains %>%
  str_replace("NS_042g_27F", "ST00042") %>%
  str_replace("NS_164C_27F", "ST00164") %>%
  str_replace("NS_110C_1_27F", "ST00110") 
strains
Tab$strain <- strains


tab <- Tab %>%
  pivot_longer(-strain, names_to = "id", values_to = "count") 
tab



Dat <- meta %>%
  left_join(tab %>%
              group_by(id) %>%
              summarize(depth = sum(count)),
            by = "id") %>%
  left_join(tab, by = c("id", "strain"))
Dat

#' Calculate relative abundances of each strain on the inoculum
#' of each experiment

Dat <- Dat %>%
  left_join(Dat %>%
              filter(hrs == 0) %>%
              group_by(strain, community, exp) %>%
              summarise(i_freq = sum(count) / sum(depth),
                        .groups = "drop"),
            by = c("strain", "community", "exp"))
Dat


#' Confirm that count match expected species
#' In general it does. ST00060 seems to be the only semi problematic
Dat %>% 
  ggplot(aes(x = added == 1, y = count)) +
  facet_wrap(~ strain, scales = "free_y") +
  geom_point(position = position_jitter(width = 0.1, height = 0)) +
  theme_classic()
  


#' # Model with lme4
library(lme4)
Dat


dat <- Dat %>%
  mutate(b_com = paste0(community, "_", strain)) %>%
  mutate(b_com = replace(b_com, added == 0, NA)) %>%
  
  mutate(b_rep = paste0(exp, "_", strain)) %>%
  mutate(b_rep = replace(b_rep, added == 0, NA)) %>%
  
  mutate(b_temp = paste0(temp, "_", strain)) %>%
  mutate(b_temp = replace(b_temp, added == 0, NA)) %>%
  
  mutate(b_obs = as.character(1:n()))
dat

m1 <- glmer(count ~ log(depth) + i_freq + (1|b_com) + 
              (1|b_rep) + (1|b_temp) + (1|b_obs), 
            data = dat %>%
              filter(hrs == 24), 
            family = poisson(link = log) )
summary(m1)
AIC(m1)

#' Get the effect of community
b_com <- ranef(m1, condVar = TRUE, whichel = "b_com", postVar = TRUE)
res <- tibble(effect = row.names(b_com$b_com),
              est = b_com$b_com[,1],
              postVar = attr(b_com$b_com, "postVar")[,,]) %>%
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


#' Get the effect of temp
b_temp <- ranef(m1, condVar = TRUE, whichel = "b_temp", postVar = TRUE)
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



dat %>%
  filter(strain == "ST00046") 


