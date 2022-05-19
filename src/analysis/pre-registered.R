
#H1a Women scholars are more likely to play it safe and indicate more validation strategies than men scholars.
#H1b Women scholars are more likely to play it safe and indicate more challenges as reasons not to use CTAM than men scholars.
#H2a Women scholars are more likely to indicate a higher number of important training needs than men scholars.
#H2b Women scholars are more likely to indicate that they themselves require (further advanced) training than men scholars

m1 <- regression(d)
df <- m1 %>% 
  select(`Dependent Variable` = id, 
         `Coefficient` = estimate,
         `CI Lower Bound` = lower, 
         `CI Upper Bound` = upper)

## Split Sample on Using CTAM
d <- d %>% 
  mutate(comp_txt2 = if_else(comp_txt < 4, 0 ,1))
m_split <- regression2(d)

m1 <- m1 %>% 
  select(estimate, lower, upper, y = id, term) %>%
  mutate(term = recode(term, `gender` = "Fulll Sample <br/> (N=433)"))
m_split <- m_split %>% 
  select(estimate = AME, lower, upper, y, term = method)

p <- m1 %>% 
  add_case(m_split) %>% 
  mutate(y = factor(y,
                     levels = c("DV: Individual Training Need (H2b)",
                                "DV: Amount of Training Needs in General (H2a)",
                                "DV: Number of Reported Challenges (H1b)",
                                "DV: Number of Validation Strategies (H1a)"))) %>% 
  ggplot(aes(x = y, y = estimate,
             ymin = lower, ymax = upper, color = term)) +
  geom_point(position = position_dodge(.5), size = 3) + 
  geom_errorbar(position = position_dodge(.5), width = 0, alpha = .6) +
  mdthemes::as_md_theme(theme_ipsum()) +
  labs(x = "", 
       y = "Effect of Identifying as Female on Dependent Variable",
       caption = "*Estimates with 95% CI (one-sided)*") +
  geom_hline(yintercept = 0, size = .5, linetype = "dashed", color = "gray75") +
  coord_flip() +
  scale_color_manual(values = fig_cols) +
  theme(legend.position="bottom",
        legend.title = element_blank()) 
