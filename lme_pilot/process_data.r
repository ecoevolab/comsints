#' Script to prepare data from pilot for modelling. We require to put every
#' count observation in a single line in a master data table. We are also
#' Calculating the frequency of inoculum. *Should we also calculate frequency
#' at time t-1*

#' The working directory may be set different depending on where are the
#' relevant files. I'm using NSM's ancom_data files downloaded on 2025-03-06.

setwd("/Users/sur/lab/exp/2025/today")
library(tidyverse)

#' # Read data

#' We add column names to the sample ids (`id`) and strain ids (`strain`)
Tab <- read_csv("2025-03-06.ancom_data/ancom_data/freq.csv") %>%
  rename(strain = 1)
Tab

Meta <- read_csv("2025-03-06.ancom_data/ancom_data/meta.csv") %>%
  rename(id = 1)
Meta

#' **NOTE**: The following are locations and files in Sur's lab desktop. Format
#' might be different in someone else
syncoms <- read_tsv("../../../data/2024_rhizo_pilot_syncom_NS/NS1/syncoms.tsv") %>%
  full_join(read_tsv("../../../data/2024_rhizo_pilot_syncom_NS/NS2/syncoms.tsv"), 
            by = "strain")
syncoms

#' # Format data
#' We add a column per strain to the metadata table to indicate which species
#' were added to that community, 1 means added, NA means not added (though
#' it might be detected still). We create a new object.
meta <- Meta %>%
  # filter(community %in% c("R1", "R2")) %>%
  left_join(syncoms %>%
              # select(strain, R1,R2) %>%
              pivot_longer(-strain, names_to = "community", values_to = "presence") %>%
              pivot_wider(names_from = "strain", values_from = "presence"),
            by = "community")
meta 

#' Then we pivot the table so there is one row per *strain x sample* 
#' combination. The new column added indicates if the relevant strain
#' was added (1) or not (0)
meta <- meta %>%
  pivot_longer(-c("id", "community", "hrs", "techrep", "exp", "temp", "color_comsint", "community_temp"),
               names_to = "strain", values_to = "added") %>%
  mutate(added = replace_na(added, 0))
meta

#' Before adding count information we need to homogenize the strain names
strains <- Tab$strain
strains <- strains %>%
  str_replace("NS_042g_27F", "ST00042") %>%
  str_replace("NS_164C_27F", "ST00164") %>%
  str_replace("NS_110C_1_27F", "ST00110") 
strains
Tab$strain <- strains

#' Now we pivot the count table to have 1 row per *sample x strain* combination
#' and we join it with the metadata table. We also calculate the sequencing
#' depth per sample and add that information to the metadata. We create a new
#' object
tab <- Tab %>%
  pivot_longer(-strain, names_to = "id", values_to = "count") 
Dat <- meta %>%
  left_join(tab %>%
              group_by(id) %>%
              summarize(depth = sum(count)),
            by = "id") %>%
  left_join(tab, by = c("id", "strain"))
Dat

#' Calculate relative abundances of each strain in the inoculum
#' of each experiment, and add that information to the data
Dat <- Dat %>%
  left_join(Dat %>%
              filter(hrs == 0) %>%
              group_by(strain, community, exp) %>%
              summarise(i_freq = sum(count) / sum(depth),
                        .groups = "drop"),
            by = c("strain", "community", "exp"))
Dat

#' # Add modelling variables

#' We add variables for things we want to model. We are **ignoring time** for 
#' the time (;)) being

Dat <- Dat %>%
  # The effect of community on strain abundance
  mutate(b_com = paste0(community, "_", strain)) %>%
  mutate(b_com = replace(b_com, added == 0, NA)) %>%
  
  # The effect of biological replicat on strain abundance
  mutate(b_rep = paste0(exp, "_", strain)) %>%
  mutate(b_rep = replace(b_rep, added == 0, NA)) %>%
  
  # The effect of temperature on strain abundance
  mutate(b_temp = paste0(temp, "_", strain)) %>%
  mutate(b_temp = replace(b_temp, added == 0, NA)) %>%
  
  # The effect of *temperature x community* on strain abundance
  
  # The effect of community *color* on strain abundance
  
  # Individual observation-level effect (for overdispersion)
  mutate(b_obs = as.character(1:n()))
Dat



#' Confirm that count match expected species
#' In general it does. ST00060 seems to be the only semi problematic
Dat %>% 
  # filter(community %in% c("R3", "R4")) %>%
  ggplot(aes(x = added == 1, y = count)) +
  facet_wrap(~ strain, scales = "free_y") +
  geom_point(position = position_jitter(width = 0.1, height = 0)) +
  theme_classic()
  


Dat %>% filter(strain == "ST00046")  %>% arrange(added) %>% print(n = 200)

Dat %>% filter(community %in% c("R3","R4")) %>%
  filter(added == 1)

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
BIC(m1)

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


#' Example of ST00046 showing that it decreases more at 28 than 32 (not significant)
dat %>%
  filter(strain == "ST00046") %>%
  filter(hrs %in% c(24)) %>%
  ggplot(aes(col = factor(temp))) +
  facet_wrap(~community) +
  geom_segment(aes(x = 0, y = i_freq, xend = temp, yend = count / depth, linetype = exp )) +
  theme_classic() 


