# Gender & CTAM
descr_1 <- d %>% 
  select(gender, comp_txt2) %>%
  group_by(gender, comp_txt2) %>% 
  summarise(n = n()) %>% 
  mutate(gender = recode(gender, .missing = 0),
         percent = if_else(comp_txt2 == 0, n/203, n/230),
         gender = recode(gender, `0` = "Male <br/> (N= 291)", 
                         `1` = "Female <br/> (N = 142)", .default = "Male <br/> (N= 291)", .missing = 'Male \n (N= 291)'),
         comp_txt2 = recode(comp_txt2, `1` = "Using CTAM", 
                            `0` = "Not Using CTAM")) %>% 
  ggplot(aes(x = comp_txt2, y = percent, fill = gender)) +
  geom_col(width = .3) +
  mdthemes::as_md_theme(theme_ipsum()) +
  labs(x = "", y = "% of sample (not) using CTAM",
       title = "**Using CTAM**") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position="none",
        legend.title = element_blank()) 

# Gender & type of text analysis/ software use
descr <- d %>%
  select(n_val_strat, n_challenges, n_training_needs_gen, 
         n_training_needs_ind, gender) %>%
  pivot_longer(cols = n_val_strat:n_training_needs_ind,
               names_to = "dvs") %>%
  mutate(dvs = recode(dvs,
                      n_val_strat = "Number of Validation Strategies (H1a) <br/> Scale: 0 - 6",
                      n_challenges = "Number of Reported Challenges (H1b) <br/> Scale: 0 - 15",
                      n_training_needs_gen = "Amount of Training Needs in General (H2a) <br/> Scale: 0 - 4",
                      n_training_needs_ind = "Individual Training Need (H2b) <br/> Scale: 0 - 1"),
         value = as.numeric(value),
         gender = recode(gender, `1` = "Female <br/> (N=142)", `0` = "Male <br/> (N=291)", 
                         .missing = "Male <br/> (N=291)", .default = "Male <br/> (N=291)")) %>%
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
  mdthemes::as_md_theme(theme_ipsum()) +
  labs(y = "", 
       x = "",
       title = "**Dependent Variables under Study**") +
  scale_color_manual(values = fig_cols) +
  coord_flip() +
  theme(legend.position="none",
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
         gender = recode(gender, `1` = "Female <br/> (N=142)", 
                         `0` = "Male <br/> (N=291)", 
                         .missing = "Male <br/> (N=291)", 
                         .default = "Male <br/> (N=291)")) %>% 
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
  mdthemes::as_md_theme(theme_ipsum()) +
  labs(y = "No Knowledge At All (1) - Primary Method of Research (5)", 
       x = "",
       title = "**Software Knowledge**") +
  scale_color_manual(values = fig_cols) +
  coord_flip() +
  theme(legend.position="none",
        legend.title = element_blank()) 

descr_method <- d %>% 
  select(gender, quali_txt:comp_txt) %>% 
  pivot_longer(cols = quali_txt:comp_txt,
               names_to = "method") %>% 
  mutate(method = recode(method,
                           `quali_txt` = "Qualitative Text Analysis",
                           `man_txt` = "Quantitative Manual Text Analysis",
                           `comp_txt` = "Computational Text Analysis"),
         gender = recode(gender, `1` = "Female <br/> (N=142)", 
                         `0` = "Male <br/> (N=291)", 
                         .missing = "Male <br/> (N=291)", 
                         .default = "Male <br/> (N=291)")) %>% 
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
  mdthemes::as_md_theme(theme_ipsum()) +
  labs(y = "Never Use It (1) - Primary Method of Research (5)", 
       x = "",
       title = "**Type of Content Analysis**",
       caption = "*Dots are means, wiskers represent 95% CI*") +
  scale_color_manual(values = fig_cols) +
  coord_flip() +
  theme(legend.position="none",
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

exp1 <- regression3(df)
exp2 <- regression4(df)

p_exp1 <- exp1 %>%
  mutate(interact = factor(interact,
                           levels = c("Discipline: Communication Science <br/> (N=127)",
                                      "Discipline: Political Science <br/> (N=83)",
                                      "Multi-Disciplinary <br/> (N=143)",
                                      "Discipline: Other <br/> (N=80)"))) %>% 
  ggplot(aes(x = y, y = AME,
                     ymin = lower, ymax = upper, color = interact)) +
  geom_point(position = position_dodge(.5), size = 3) + 
  geom_errorbar(position = position_dodge(.5), width = 0, alpha = .6) +
  mdthemes::as_md_theme(theme_ipsum()) +
  labs(x = "", 
       y = "",
       title = "**Discipline**") +
  geom_hline(yintercept = 0, size = .5, linetype = "dashed", color = "gray75") +
  coord_flip() +
  scale_color_manual(values = fig_cols) +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=4,byrow=TRUE))

p_exp2 <- exp2 %>% 
  mutate(interact = factor(interact,
                           levels = c("PhD Student <br/> (N=38)",
                                      "Early-Career Researcher (<5 years since PhD) <br/> (N=110)",
                                      "Mid-Career Researcher (5-15 years since PhD) <br/> (N=205)",
                                      "Senior Researcher (>15 years since PhD) <br/> (N=80)"))) %>% 
   ggplot(aes(x = y, y = AME,
             ymin = lower, ymax = upper, color = interact)) +
  geom_point(position = position_dodge(.5), size = 3) + 
  geom_errorbar(position = position_dodge(.5), width = 0, alpha = .6) +
  mdthemes::as_md_theme(theme_ipsum()) +
  labs(x = "", 
       y = "",
       title = "**Career Stage**",
       caption = "Marginal Effect of Identifying as Female <br/> *Estimates with 95% CI (one-sided)*") +
  geom_hline(yintercept = 0, size = .5, linetype = "dashed", color = "gray75") +
  coord_flip() +
  scale_color_manual(values = fig_cols) +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.text.y = element_blank()) +
  guides(color=guide_legend(nrow=4,byrow=TRUE))
