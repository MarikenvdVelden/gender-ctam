#H1a Women scholars are more likely to play it safe and indicate more validation strategies than men scholars.

h1a <- lm(n_val_strat ~ gender + factor(discipline) + factor(career) + 
           stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
           txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = d)
h1a <- tidy(h1a) %>%
  mutate(lower = estimate - (1.65 * std.error),
         upper = estimate + (1.65 * std.error),
         id = "DV: Number of Validation Strategies (H1a)") %>%
  select(term, estimate, lower, upper, id)

#H1b Women scholars are more likely to play it safe and indicate more challenges as reasons not to use CTAM than men scholars.

h1b <- lm(n_challenges ~ gender + factor(discipline) + factor(career) +
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = d)
h1b <- tidy(h1b) %>%
  mutate(lower = estimate - (1.65 * std.error),
         upper = estimate + (1.65 * std.error),
         id = "DV: Number of Reported Challenges (H1b)") %>%
  select(term, estimate, lower, upper, id)

#H2a Women scholars are more likely to indicate a higher number of important training needs than men scholars.
h2a <- lm(n_training_needs_gen ~ gender + factor(discipline) + factor(career) +
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = d)
h2a <- tidy(h2a) %>%
  mutate(lower = estimate - (1.65 * std.error),
         upper = estimate + (1.65 * std.error),
         id = "DV: Amount of Training Needs in General (H2a)") %>%
  select(term, estimate, lower, upper, id)

#H2b Women scholars are more likely to indicate that they themselves require (further advanced) training than men scholars
h2b <- lm(n_training_needs_ind ~ gender + factor(discipline) + factor(career) +
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = d)
h2b <- tidy(h2b) %>%
  mutate(lower = estimate - (1.65 * std.error),
         upper = estimate + (1.65 * std.error),
         id = "DV: Individual Training Need (H2b)") %>%
  select(term, estimate, lower, upper, id)

p <- h1a %>% 
  add_case(h1b) %>% 
  add_case(h2a) %>% 
  add_case(h2b) %>% 
  filter(term == "gender") %>% 
  mutate(id = factor(id,
                     levels = c("DV: Individual Training Need (H2b)",
                                "DV: Amount of Training Needs in General (H2a)",
                                "DV: Number of Reported Challenges (H1b)",
                                "DV: Number of Validation Strategies (H1a)"))) %>% 
  ggplot(aes(x = id, y = estimate,
             ymin = lower, ymax = upper)) +
  geom_point(position = position_dodge(.5), size = 3, color = fig_cols[6]) + 
  geom_errorbar(position = position_dodge(.5), width = 0, alpha = .6, color = fig_cols[6]) +
  labs(x = "", 
       y = "Effect of Identifying as Female on Dependent Variable") +
  theme_ipsum() +
  geom_hline(yintercept = 0, size = .5, linetype = "dashed", color = "gray75") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) 

df <- h1a %>% 
  add_case(h1b) %>% 
  add_case(h2a) %>% 
  add_case(h2b) %>% 
  filter(term == "gender") %>% 
  select(`Dependent Variable` = id, 
         `Coefficient` = estimate,
         `CI Lower Bound` = lower, 
         `CI Upper Bound` = upper)
