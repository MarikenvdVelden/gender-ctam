descr <- d %>%
  select(gender:comp_txt, -career, -discipline) %>%
  pivot_longer(cols = everything(),
               names_to = "Variables") %>%
  group_by(Variables) %>%
  summarise(`Mean Value` = mean(value, na.rm = T),
            `St. Dev` = sd(value, na.rm = T),
            `Min. Value` = min(value, na.rm = T),
            `Max. Value` = max(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Variables = recode(Variables,
                            `gender` = "Gender",
                            `n_val_strat` = "Number of Validation Strategies",
                            `n_challenges`  = "Number of Reported Challenges",
                            `n_training_needs_gen`  = "Amount of Training Needs in General",
                            `n_training_needs_ind`  = "Individual Training Need",
                            `stats_softw` = "Software Knowledge: Statistical Software",
                            `math_softw` = "Software Knowledge: Mathematical Software",
                            `netw_softw`  = "Software Knowledge: Network Analysis Software",
                            `quali_softw` = "Software Knowledge: Qualitative Data Analysis Software",
                            `txt_softw` = "Software Knowledge: Specialized Text Mining Software",
                            `txt_platf` = "Software Knowledge: Generalized Text Analysis Software",
                            `opensource_platf` = "Software Knowledge: Open Source Coding Platforms Software",
                            `quali_txt`  = "Type of Content Analysis: Qualitative Text Analysis",
                            `man_txt` = "Type of Content Analysis: Quantitative Manual Text Analysis",
                            `comp_txt` = "Type of Content Analysis: Computational Text Analysis"),
         Variables = factor(Variables,
                            levels = c("Gender", "Number of Validation Strategies","Number of Reported Challenges",
                                       "Amount of Training Needs in General", "Individual Training Need",
                                       "Discipline: Communication Science", "Discipline: Economics",
                                       "Discipline: Political Science", "Discipline: Psychology",
                                       "Discipline: Sociology", "Discipline: Other",
                                       "Software Knowledge: Statistical Software",
                                       "Software Knowledge: Mathematical Software",
                                       "Software Knowledge: Network Analysis Software",
                                       "Software Knowledge: Qualitative Data Analysis Software",
                                       "Software Knowledge: Specialized Text Mining Software",
                                       "Software Knowledge: Generalized Text Analysis Software",
                                       "Software Knowledge: Open Source Coding Platforms Software",
                                       "Type of Content Analysis: Qualitative Text Analysis",
                                       "Type of Content Analysis: Quantitative Manual Text Analysis",
                                       "Type of Content Analysis: Computational Text Analysis")))

