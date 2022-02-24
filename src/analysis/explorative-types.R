## Seperate indicators: Validation
m <- regression5(d)
df <- m %>% 
  select(`Dependent Variable` = id, 
         `Coefficient` = estimate,
         `CI Lower Bound` = lower, 
         `CI Upper Bound` = upper)

d <- d %>% 
  mutate(comp_txt2 = if_else(comp_txt < 4, 0 ,1))
m_split <- regression6(d)

m <- m %>% 
  select(estimate, lower, upper, y = id, term) %>%
  mutate(term = recode(term, `gender` = "Fulll Sample"))
m_split <- m_split %>% 
  select(estimate = AME, lower, upper, y, term = method)

pv <- m %>% 
  add_case(m_split) %>% 
  mutate(y = factor(y,
                    levels = c("DV: Check documentation of software developer",
                               "DV: Check finding for plausibility and interpretability",
                               "DV: Check against gold standard",
                               "DV: Check validity of classification rules",
                               "DV: Check algorithmic procedures"))) %>% 
  ggplot(aes(x = y, y = estimate,
             ymin = lower, ymax = upper, color = term)) +
  geom_point(position = position_dodge(.5), size = 3) + 
  geom_errorbar(position = position_dodge(.5), width = 0, alpha = .6) +
  labs(x = "", 
       y = "Effect of Identifying as Female on Dependent Variable",
       title = "Validation") +
  theme_ipsum() +
  geom_hline(yintercept = 0, size = .5, linetype = "dashed", color = "gray75") +
  coord_flip() +
  scale_color_manual(values = fig_cols) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) 

## Seperate indicators: Challengess
m <- regression7(d)
df <- m %>% 
  select(`Dependent Variable` = id, 
         `Coefficient` = estimate,
         `CI Lower Bound` = lower, 
         `CI Upper Bound` = upper)

d <- d %>% 
  mutate(comp_txt2 = if_else(comp_txt < 4, 0 ,1))
m_split <- regression8(d)

m <- m %>% 
  select(estimate, lower, upper, y = id, term) %>%
  mutate(term = recode(term, `gender` = "Fulll Sample"))
m_split <- m_split %>% 
  select(estimate = AME, lower, upper, y, term = method)

pc <- m %>% 
  add_case(m_split) %>% 
  mutate(y = factor(y,
                    levels = c("DV: Time/effort required",
                               "DV: Funding required",
                               "DV: Training required",
                               "DV: Level of available instruction/materials higher than needed",
                               "DV: Level of available instruction/materials lower than needed",
                               "DV: Limited methodological guidance/documentation of tools",
                               "DV: Availability of suitable computational tools for specific measurement purposes",
                               "DV: Availability of suitable computational tools for the language(s) that I study",
                               "DV: Comparability of computational tools in different languages",
                               "DV: Issues concerning measurement validity/limited nuance",
                               "DV: Loss of manual contact with the material",
                               "DV: Reviewers’/editors’ skepticism toward computational methods",
                               "DV: Peers’ skepticism toward computational methods",
                               "DV: Own skepticism toward computational methods",
                               "DV: Computational methods are unnecessary/less relevant for my research"))) %>% 
  ggplot(aes(x = y, y = estimate,
             ymin = lower, ymax = upper, color = term)) +
  geom_point(position = position_dodge(.5), size = 3) + 
  geom_errorbar(position = position_dodge(.5), width = 0, alpha = .6) +
  labs(x = "", 
       y = "Effect of Identifying as Female on Dependent Variable",
       title = "Challenges") +
  theme_ipsum() +
  geom_hline(yintercept = 0, size = .5, linetype = "dashed", color = "gray75") +
  coord_flip() +
  scale_color_manual(values = fig_cols) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) 

## Seperate indicators: Training
m <- regression9(d)
df <- m %>% 
  select(`Dependent Variable` = id, 
         `Coefficient` = estimate,
         `CI Lower Bound` = lower, 
         `CI Upper Bound` = upper)

d <- d %>% 
  mutate(comp_txt2 = if_else(comp_txt < 4, 0 ,1))
m_split <- regression10(d)

m <- m %>% 
  select(estimate, lower, upper, y = id, term) %>%
  mutate(term = recode(term, `gender` = "Fulll Sample"))
m_split <- m_split %>% 
  select(estimate = AME, lower, upper, y, term = method)

pt <- m %>% 
  add_case(m_split) %>% 
  mutate(y = factor(y,
                    levels = c("DV: Data & Open Access Tools",
                                "DV: Programming & Software skills",
                                "DV: Theory & Concepts",
                                "DV: Research Inteegrity & Ethics"))) %>% 
  ggplot(aes(x = y, y = estimate,
             ymin = lower, ymax = upper, color = term)) +
  geom_point(position = position_dodge(.5), size = 3) + 
  geom_errorbar(position = position_dodge(.5), width = 0, alpha = .6) +
  labs(x = "", 
       y = "Effect of Identifying as Female on Dependent Variable",
       title = "Training") +
  theme_ipsum() +
  geom_hline(yintercept = 0, size = .5, linetype = "dashed", color = "gray75") +
  coord_flip() +
  scale_color_manual(values = fig_cols) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) 
