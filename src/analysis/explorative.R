#Interaction with discipline
df <- d %>% 
  mutate(discipline = recode(discipline,
                             `Discipline: Economics` = "Discipline: Other",
                             `Discipline: Sociology` = "Discipline: Other",
                             `Discipline: Political Science` = "Discipline: Political Science",
                             `Discipline: Communication Science` = "Discipline: Communication Science",
                             `Discipline: Psychology` = "Discipline: Other",
                             `Multi-Disciplinary` = "Multi-Disciplinary"),
         discipline = factor(discipline, 
                             levels = c("Discipline: Other","Multi-Disciplinary",
                                        "Discipline: Political Science",
                                        "Discipline: Communication Science")))

# Validation Stategies  
e1a <- lm(n_val_strat ~ gender*factor(discipline) + factor(career) + 
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)

m1a <- summary(margins(e1a, variables = "discipline", at = list(gender = 1))) %>% 
  mutate(y = "DV: Number of Validation Strategies",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         discipline = c("Discipline: Communication Science",
                        "Discipline: Political Science",
                        "Multi-Disciplinary")) %>%
  select(AME, upper, lower, y, discipline)


#challenges
e1b <- lm(n_challenges ~ gender*factor(discipline) + factor(career) +
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)
m1b <- summary(margins(e1b, variables = "discipline", at = list(gender = 1))) %>% 
  mutate(y = "DV: Number of Reported Challenges",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         discipline = c("Discipline: Communication Science",
                        "Discipline: Political Science",
                        "Multi-Disciplinary")) %>%
  select(AME, upper, lower, y, discipline)

#training needs general
e2a <- lm(n_training_needs_gen ~ gender*factor(discipline) + factor(career) +
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)
m2a <- summary(margins(e2a, variables = "discipline", at = list(gender = 1))) %>% 
  mutate(y = "DV: Amount of Training Needs in General",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         discipline = c("Discipline: Communication Science",
                        "Discipline: Political Science",
                        "Multi-Disciplinary")) %>%
  select(AME, upper, lower, y, discipline)

#H2b Women scholars are more likely to indicate that they themselves require (further advanced) training than men scholars
e2b <- lm(n_training_needs_ind ~ gender*factor(discipline) + factor(career) +
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)
m2b <- summary(margins(e2b, variables = "discipline", at = list(gender = 1))) %>% 
  mutate(y = "DV: Individual Training Need",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         discipline = c("Discipline: Communication Science",
                        "Discipline: Political Science",
                        "Multi-Disciplinary")) %>%
  select(AME, upper, lower, y, discipline)

p_disc <- m1a %>% 
  add_case(m1b) %>% 
  add_case(m2a) %>% 
  add_case(m2b) %>% 
   ggplot(aes(x = y, y = AME,
             ymin = lower, ymax = upper, color = discipline)) +
  geom_point(position = position_dodge(.5), size = 3) + 
  geom_errorbar(position = position_dodge(.5), width = 0, alpha = .6) +
  labs(x = "", 
       y = "Marginal Effect of Identifying as Female") +
  theme_ipsum() +
  geom_hline(yintercept = 0, size = .5, linetype = "dashed", color = "gray75") +
  coord_flip() +
  scale_color_manual(values = fig_cols) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

#Interaction with Academic Caareer Path

# Validation Stategies  
e3a <- lm(n_val_strat ~ gender*factor(career) + factor(discipline) +
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)

m3a <- summary(margins(e3a, variables = "career", at = list(gender = 1))) %>% 
  mutate(y = "DV: Number of Validation Strategies",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         career = c("Early-Career Researcher (<5 years since PhD)",
                        "Mid-Career Researcher (5-15 years since PhD)",
                        "Senior Researcher (>15 years since PhD)")) %>%
  select(AME, upper, lower, y, career)


#challenges
e3b <- lm(n_challenges ~ gender*factor(career) + factor(discipline) +
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)
m3b <- summary(margins(e3b, variables = "career", at = list(gender = 1))) %>% 
  mutate(y = "DV: Number of Reported Challenges",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         career = c("Early-Career Researcher (<5 years since PhD)",
                    "Mid-Career Researcher (5-15 years since PhD)",
                    "Senior Researcher (>15 years since PhD)")) %>%
  select(AME, upper, lower, y, career)


#training needs general
e4a <- lm(n_training_needs_gen ~ gender*factor(career) + factor(discipline) +
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)
m4a <- summary(margins(e4a, variables = "career", at = list(gender = 1))) %>% 
  mutate(y = "DV: Amount of Training Needs in General",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         career = c("Early-Career Researcher (<5 years since PhD)",
                    "Mid-Career Researcher (5-15 years since PhD)",
                    "Senior Researcher (>15 years since PhD)")) %>%
  select(AME, upper, lower, y, career)

#H2b Women scholars are more likely to indicate that they themselves require (further advanced) training than men scholars
e4b <- lm(n_training_needs_ind ~ gender*factor(career) + factor(discipline) +
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)
m4b <- summary(margins(e4b, variables = "career", at = list(gender = 1))) %>% 
  mutate(y = "DV: Individual Training Need",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         career = c("Early-Career Researcher (<5 years since PhD)",
                    "Mid-Career Researcher (5-15 years since PhD)",
                    "Senior Researcher (>15 years since PhD)")) %>%
  select(AME, upper, lower, y, career)

p_career <- m3a %>% 
  add_case(m3b) %>% 
  add_case(m4a) %>% 
  add_case(m4b) %>% 
  ggplot(aes(x = y, y = AME,
             ymin = lower, ymax = upper, color = career)) +
  geom_point(position = position_dodge(.5), size = 3) + 
  geom_errorbar(position = position_dodge(.5), width = 0, alpha = .6) +
  labs(x = "", 
       y = "Marginal Effect of Identifying as Female") +
  theme_ipsum() +
  geom_hline(yintercept = 0, size = .5, linetype = "dashed", color = "gray75") +
  coord_flip() +
  scale_color_manual(values = fig_cols) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=3,byrow=TRUE))