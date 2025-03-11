
summary(m1.t1)


lattice::qqmath(ranef(m1.t1))


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


