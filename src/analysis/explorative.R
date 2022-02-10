
# Gender & type of text analysis/ software use
descr <- d %>%
  select(n_val_strat, n_challenges, n_training_needs_gen, 
         n_training_needs_ind, gender) %>%
  pivot_longer(cols = n_val_strat:n_training_needs_ind,
               names_to = "dvs") %>%
  mutate(dvs = recode(dvs,
                      n_val_strat = "Number of Validation Strategies (H1a) \n Scale: 0 - 6",
                      n_challenges = "Number of Reported Challenges (H1b) \n Scale: 0 - 15",
                      n_training_needs_gen = "Amount of Training Needs in General (H2a) \n Scale: 0 - 4",
                      n_training_needs_ind = "Individual Training Need (H2b) \n Scale: 0 - 1"),
         value = as.numeric(value),
         gender = recode(gender, `1` = "Female \n (N=142)", `0` = "Male \n (N=291)", 
                         .missing = "Male \n (N=291)", .default = "Male \n (N=291)")) %>%
  group_by(dvs, gender) %>%
  summarise(means = mean(value, na.rm=T),
            stdev = sd(value, na.rm=T),
            n = n()) %>%
  ungroup() %>% 
  mutate(lower = means - (1.56 * stdev),
         upper = means + (1.56 * stdev)) %>% 
  ggplot(aes(x = dvs, y = means,
             ymin = lower, ymax = upper, color = gender)) +
  geom_point(position = position_dodge(.5), size = 3) + 
  geom_errorbar(position = position_dodge(.5), width = 0, alpha = .6) +
  labs(y = "", 
       x = "",
       title = "Dependent Variables under Study") +
  theme_ipsum() +
  scale_color_manual(values = fig_cols) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) 

descr_softw <- d %>% 
  select(gender, stats_softw:opensource_platf) %>% 
  pivot_longer(cols = stats_softw:opensource_platf,
               names_to = "Software") %>% 
  mutate(Software = recode(Software,
                           `stats_softw` = "Statistical Software",
                           `math_softw` = "Mathematical Software",
                           `netw_softw` = "Network Analysis Software",
                           `quali_softw` = "Qualitative Data Analysis Software",
                           `txt_softw` = "Specialized Text Mining Software",
                           `txt_platf` = "Generalized Text Analysis Platforms",
                           `opensource_platf` = "Open Source Coding Platforms"),
         gender = recode(gender, `1` = "Female \n (N=142)", `0` = "Male \n (N=291)", 
                         .missing = "Male \n (N=291)", .default = "Male \n (N=291)")) %>% 
  group_by(Software, gender) %>%
  summarise(means = mean(value, na.rm=T),
            stdev = sd(value, na.rm=T),
            n = n()) %>%
  ungroup() %>% 
  mutate(lower = means - (1.56 * stdev),
         upper = means + (1.56 * stdev)) %>% 
  ggplot(aes(x = Software, y = means,
             ymin = lower, ymax = upper, color = gender)) +
  geom_point(position = position_dodge(.5), size = 3) + 
  geom_errorbar(position = position_dodge(.5), width = 0, alpha = .6) +
  labs(y = "No Knowledge At All (1) - Primary Method of Research (5)", 
       x = "",
       title = "Software Knowledge") +
  theme_ipsum() +
  scale_color_manual(values = fig_cols) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) 

descr_method <- d %>% 
  select(gender, quali_txt:comp_txt) %>% 
  pivot_longer(cols = quali_txt:comp_txt,
               names_to = "method") %>% 
  mutate(method = recode(method,
                           `quali_txt` = "Qualitative Text Analysis",
                           `man_txt` = "Quantitative Manual Text Analysis",
                           `comp_txt` = "Computational Text Analysis"),
         gender = recode(gender, `1` = "Female \n (N=142)", `0` = "Male \n (N=291)", 
                         .missing = "Male \n (N=291)", .default = "Male \n (N=291)")) %>% 
  group_by(method, gender) %>%
  summarise(means = mean(value, na.rm=T),
            stdev = sd(value, na.rm=T)) %>%
  ungroup() %>% 
  mutate(lower = means - (1.56 * stdev),
         upper = means + (1.56 * stdev)) %>% 
  ggplot(aes(x = method, y = means,
             ymin = lower, ymax = upper, color = gender)) +
  geom_point(position = position_dodge(.5), size = 3) + 
  geom_errorbar(position = position_dodge(.5), width = 0, alpha = .6) +
  labs(y = "Never Use It (1) - Primary Method of Research (5)", 
       x = "",
       title = "Type of Content Analysis") +
  theme_ipsum() +
  scale_color_manual(values = fig_cols) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) 

#Interaction with discipline
df <- d %>% 
  mutate(discipline = recode(discipline,
                             `Discipline: Economics` = 0,
                             `Discipline: Sociology` = 0,
                             `Discipline: Political Science` = 2,
                             `Discipline: Communication Science` = 1,
                             `Discipline: Psychology` = 0,
                             `Multi-Disciplinary` = 3, .missing = 0,
                             .default = 0),
         career = recode(career,
                         `PhD Student` = 0,
                         `Early-Career Researcher (<5 years since PhD)` = 1,
                         `Mid-Career Researcher (5-15 years since PhD)` = 2,
                         `Senior Researcher (>15 years since PhD)` = 3))

# Validation Stategies  
e1a <- lm(n_val_strat ~ gender*factor(discipline) + factor(career) + 
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)

m1a <- summary(margins(e1a, variables = "gender", at = list(discipline = c(0:3)))) %>% 
  mutate(y = "DV: Number of Validation Strategies",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         discipline = c("Discipline: Other",
                        "Discipline: Communication Science",
                        "Discipline: Political Science",
                         "Multi-Disciplinary")) %>%
  select(AME, upper, lower, y, discipline)


#challenges
e1b <- lm(n_challenges ~ gender*factor(discipline) + factor(career) +
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)
m1b <- summary(margins(e1b, variables = "gender", at = list(discipline = c(0:3)))) %>% 
  mutate(y = "DV: Number of Reported Challenges",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         discipline = c("Discipline: Other",
                        "Discipline: Communication Science",
                        "Discipline: Political Science",
                        "Multi-Disciplinary")) %>%
  select(AME, upper, lower, y, discipline)

#training needs general
e2a <- lm(n_training_needs_gen ~ gender*factor(discipline) + factor(career) +
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)
m2a <- summary(margins(e2a, variables = "gender", at = list(discipline = c(0:3)))) %>% 
  mutate(y = "DV: Amount of Training Needs in General",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         discipline = c("Discipline: Other",
                        "Discipline: Communication Science",
                        "Discipline: Political Science",
                        "Multi-Disciplinary")) %>%
  select(AME, upper, lower, y, discipline)

#H2b Women scholars are more likely to indicate that they themselves require (further advanced) training than men scholars
e2b <- lm(n_training_needs_ind ~ gender*factor(discipline) + factor(career) +
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)
m2b <- summary(margins(e2b, variables = "gender", at = list(discipline = c(0:3)))) %>% 
  mutate(y = "DV: Individual Training Need",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         discipline = c("Discipline: Other",
                        "Discipline: Communication Science",
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
  guides(color=guide_legend(nrow=4,byrow=TRUE))

#Interaction with Academic Caareer Path

# Validation Stategies  
e3a <- lm(n_val_strat ~ gender*factor(career) + factor(discipline) +
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)

m3a <- summary(margins(e3a, variables = "gender", at = list(career = c(0:3)))) %>% 
  mutate(y = "DV: Number of Validation Strategies",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         career = c("PhD Student",
                    "Early-Career Researcher (<5 years since PhD)",
                    "Mid-Career Researcher (5-15 years since PhD)",
                    "Senior Researcher (>15 years since PhD)")) %>%
  select(AME, upper, lower, y, career)


#challenges
e3b <- lm(n_challenges ~ gender*factor(career) + factor(discipline) +
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)
m3b <- summary(margins(e3b, variables = "gender", at = list(career = c(0:3)))) %>% 
  mutate(y = "DV: Number of Reported Challenges",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         career = c("PhD Student",
                    "Early-Career Researcher (<5 years since PhD)",
                    "Mid-Career Researcher (5-15 years since PhD)",
                    "Senior Researcher (>15 years since PhD)")) %>%
  select(AME, upper, lower, y, career)

#training needs general
e4a <- lm(n_training_needs_gen ~ gender*factor(career) + factor(discipline) +
            stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)
m4a <- summary(margins(e4a, variables = "gender", at = list(career = c(0:3)))) %>% 
  mutate(y = "DV: Amount of Training Needs in General",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         career = c("PhD Student",
                    "Early-Career Researcher (<5 years since PhD)",
                    "Mid-Career Researcher (5-15 years since PhD)",
                    "Senior Researcher (>15 years since PhD)")) %>%
  select(AME, upper, lower, y, career)

#H2b Women scholars are more likely to indicate that they themselves require (further advanced) training than men scholars
e4b <- lm(n_training_needs_ind ~ gender*factor(career) + factor(discipline) +
            txt_platf + opensource_platf + quali_txt + man_txt + comp_txt, data = df)
m4b <- summary(margins(e4b, variables = "gender", at = list(career = c(0:3)))) %>% 
  mutate(y = "DV: Individual Training Need",
         lower = AME - (1.65 * SE),
         upper = AME + (1.65 * SE),
         career = c("PhD Student",
                    "Early-Career Researcher (<5 years since PhD)",
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
  guides(color=guide_legend(nrow=4,byrow=TRUE))
