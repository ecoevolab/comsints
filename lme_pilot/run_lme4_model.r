# rmarkdown::render("/Users/sur/lab/src/NSM_comsints/lme_pilot/run_lme4_model.r", output_format = "html_document", output_dir = "/Users/sur/lab/exp/2025/today")
# setwd("/Users/sur/lab/exp/2025/today")
knitr::opts_knit$set(root.dir = "/Users/sur/lab/exp/2025/today")

library(tidyverse)
library(lme4)

Dat <- read_tsv("pilot_dat_perfect_design.tsv")
Dat


#' Try simple model


#' 24 hours only
dat <- Dat %>%
  filter(hrs == 24)

#' Four basic poisson models to check the effect of the overdispersion
#' variable (b_obs) and the interaction (temp * community) effects
m1.t1 <-  glmer(count ~ log(depth) + i_freq + 
                  (1|b_com) + (1|b_temp) + (1|b_temp_com) + (1|b_rep) + (1|b_obs),
                data = dat, 
                family = poisson(link = "log"),
                control=glmerControl(optCtrl=list(maxfun=4*1e4)))

m2.t1 <-  glmer(count ~ log(depth) + i_freq + 
                  (1|b_com) + (1|b_temp) + (1|b_rep) + (1|b_obs),
                data = dat, 
                family = poisson(link = "log"),
                control=glmerControl(optCtrl=list(maxfun=4*1e4)))

m3.t1 <-  glmer(count ~ log(depth) + i_freq + 
                  (1|b_com) + (1|b_temp) + (1|b_temp_com) + (1|b_rep),
                data = dat, 
                family = poisson(link = "log"),
                control=glmerControl(optCtrl=list(maxfun=4*1e4)))

m4.t1 <-  glmer(count ~ log(depth) + i_freq + 
                  (1|b_com) + (1|b_temp) + (1|b_rep),
                data = dat, 
                family = poisson(link = "log"),
                control=glmerControl(optCtrl=list(maxfun=4*1e4)))
                
AIC(m1.t1, m2.t1, m3.t1, m4.t1)
BIC(m1.t1, m2.t1, m3.t1, m4.t1)

#' Basically we need to include the overdispersion parameter.
#' The model without interactions provides a better fit.
#' Now fit the negative binomial

m5.t1 <- glmer.nb(count ~ log(depth) + i_freq + 
                    (1|b_com) + (1|b_temp) + (1|b_temp_com) + (1|b_rep),
                  data = dat)

m6.t1 <- glmer.nb(count ~ log(depth) + i_freq + 
                    (1|b_com) + (1|b_temp) + (1|b_rep),
                  data = dat)



AIC(m1.t1, m2.t1, m3.t1, m4.t1, m5.t1, m6.t1)
# df        AIC
# m1.t1 126   4288.263
# m2.t1   7   4570.509
# m3.t1 125  91657.828
# m4.t1   6 240681.080
# m5.t1 126   4277.931
# m6.t1   7   4563.837
BIC(m1.t1, m2.t1, m3.t1, m4.t1, m5.t1, m6.t1)
# df        BIC
# m1.t1 126   4724.171
# m2.t1   7   4594.726
# m3.t1 125  92090.277
# m4.t1   6 240701.837
# m5.t1 126   4713.839
# m6.t1   7   4588.054

save(m1.t1, m2.t1, m3.t1, m4.t1, m5.t1, m6.t1, file = "models.rdat")

#' There were convergence issues but negative binomial performs well
#' There is disagreement between AIC and BIC. BIC gives the models without
#' temp by community interactions better scores while AIC gives the models with
#' temperature x community interaction as better
#' 
#' From the winbugs manual:
#' 
#' 
#' In hierarchical models, these three techniques are
#' essentially answering different prediction problems.
#' Suppose the three levels of our model concerned
#' classes within schools within a country. Then
#' 1. if we were interested in predicting results of
#' future classes in those actual schools, then
#' DIC is appropriate (ie the random effects
#'                     themselves are of interest);
#' 2. if we were interested in predicting results of
#' future schools in that country, then marginal-
#'   likelihood methods such as AIC are
#' appropriate (ie the population parameters are
#'              of interest);
#' 3. if we were interested in predicting results for
#' a new country, then BIC/ Bayes factors are
#' appropriate (ie the 'true' underlying model is
#'              of interest).

#' We are interested in future run of these communities, thus
#' DIC may be more relevant?


#' Will run brms with the negbinomial model
library(brms)
load("brms_models.rdat")

# No convergence issues, takes 2 min with compile
m5.br.t1 <- brm(count ~ log(depth) + i_freq + 
                  (1|b_com) + (1|b_temp) + (1|b_temp_com) + (1|b_rep),
                data = dat,
                chains = 4,
                iter = 4000,
                warmup = 1000,
                cores = 4,
                thin = 2,
                family = "negbinomial")

summary(m5.br.t1)
summary(m5.t1)
pairs(m5.br.t1)

# Takes about 2 minutes to run with compilation
m6.br.t1 <- brm(count ~ log(depth) + i_freq + 
                  (1|b_com) + (1|b_temp) + (1|b_rep),
                data = dat,
                chains = 4,
                iter = 4000,
                warmup = 1000,
                cores = 4,
                thin = 2,
                family = "negbinomial")


summary(m6.br.t1)
summary(m6.t1)

save(m5.br.t1, m6.br.t1, file = "brms_models.rdat")

#' We compare with waic and loo
compare_ic(brms::waic(m5.br.t1), brms::waic(m6.br.t1))

m5.br.t1.loo <- brms::loo(m5.br.t1)
m6.br.t1.loo <- brms::loo(m6.br.t1)
m5.br.t1.loo
m6.br.t1.loo

# loo_compare(brms::loo(m5.br.t1, moment_match = TRUE), brms::loo(m6.br.t1, moment_match = TRUE))

