c1 <- d %>% 
  dplyr::select(Q10_1l:Q10_15l, gender) %>% 
  pivot_longer(cols = Q10_1l:Q10_15l) %>% 
  group_by(gender, name, value) %>% 
  filter(value > 0) %>% 
  count() %>% 
  mutate(perc = ifelse(gender == 0, n/142, n/290),
         gender = recode(gender,
                         `1` = "Female",
                         `0` = "Male"),
         name = recode(name,
                       `Q10_9l` = "Comparability of computational tools in different languages",
                       `Q10_6l` = "Limited methodological guidance/documentation of tools",
                       `Q10_12l` = "Reviewers'/editors' skepticism toward computational methods",
                       `Q10_1l` = "Time/effort required",
                       `Q10_2l` = "Funding required",
                       `Q10_4l` = "Level of available instruction/materials higher than needed",
                       `Q10_5l` = "Level of available instruction/materials lower than needed",
                       `Q10_8l` = "Availability of suitable computational tools for the language(s) that I study",
                       `Q10_11l` = "Loss of manual contact with the material",
                       `Q10_10l` = "Issues concerning measurement validity/limited nuance",
                       `Q10_7l` = "Availability of suitable computational tools for specific measurement purposes",
                       `Q10_14l` = "I am skeptical toward computational methods myself",
                       `Q10_15l` = "Computational methods are unnecessary/less relevant for my research",
                       `Q10_13l` = "Peers' skepticism toward computational methods",
                       `Q10_3l` = "Availability of required training"),
         value = recode(value,
                        `1` = "No Challenge",
                        `2` = "Minor Challenge",
                        `3` = "Major Challenge"),
         value = factor(value,
                        levels = c("No Challenge", 
                                   "Minor Challenge",
                                   "Major Challenge"))) %>% 
  ggplot(aes(x = perc, y = name, fill = value)) +
  geom_col(position = position_dodge(1)) +
  theme_ipsum(axis_text_size = 14) +
  facet_grid(.~gender) +
  labs(x = "", y = "") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

# C1-X higher by male
# CX-X higger by female

t1 <- t.test(Q10_1l ~ gender, d)
t2 <- t.test(Q10_2l ~ gender, d)
t3 <- t.test(Q10_3l ~ gender, d)
t4 <- t.test(Q10_4l ~ gender, d)
t5 <- t.test(Q10_5l ~ gender, d)
t6 <- t.test(Q10_6l ~ gender, d)
t7 <- t.test(Q10_7l ~ gender, d)
t8 <- t.test(Q10_8l ~ gender, d)
t9 <- t.test(Q10_9l ~ gender, d)
t10 <- t.test(Q10_10l ~ gender, d)
t11 <- t.test(Q10_11l ~ gender, d)
t12 <- t.test(Q10_12l ~ gender, d)
t13 <- t.test(Q10_13l ~ gender, d)
t14 <- t.test(Q10_14l ~ gender, d)
t15 <- t.test(Q10_15l ~ gender, d)

viz <- tibble(
  mv = c(unlist(t1[5])[1], unlist(t1[5])[2],
         unlist(t2[5])[1], unlist(t2[5])[2],
         unlist(t3[5])[1], unlist(t3[5])[2],
         unlist(t4[5])[1], unlist(t4[5])[2],
         unlist(t5[5])[1], unlist(t5[5])[2],
         unlist(t6[5])[1], unlist(t6[5])[2],
         unlist(t7[5])[1], unlist(t7[5])[2],
         unlist(t8[5])[1], unlist(t8[5])[2],
         unlist(t9[5])[1], unlist(t9[5])[2],
         unlist(t10[5])[1], unlist(t10[5])[2],
         unlist(t11[5])[1], unlist(t11[5])[2],
         unlist(t12[5])[1], unlist(t12[5])[2],
         unlist(t13[5])[1], unlist(t13[5])[2],
         unlist(t14[5])[1], unlist(t14[5])[2],
         unlist(t15[5])[1], unlist(t15[5])[2]),
  stdev = c(rep(unlist(t1[7]),2), 
            rep(unlist(t2[7]),2),  
            rep(unlist(t3[7]),2), 
            rep(unlist(t4[7]),2),  
            rep(unlist(t5[7]),2),  
            rep(unlist(t6[7]),2), 
            rep(unlist(t7[7]),2),  
            rep(unlist(t8[7]),2),  
            rep(unlist(t9[7]),2), 
            rep(unlist(t10[7]),2),  
            rep(unlist(t11[7]),2),  
            rep(unlist(t12[7]),2), 
            rep(unlist(t13[7]),2),  
            rep(unlist(t14[7]),2),  
            rep(unlist(t15[7]),2)),
  value = c(rep("Time/effort required", 2),
            rep("Funding required", 2),
            rep("Availability of required training", 2),
            rep("Level of available instruction/materials higher than needed", 2),
            rep("Level of available instruction/materials lower than needed", 2),
            rep("Limited methodological guidance/documentation of tools", 2),
            rep("Availability of suitable computational tools for specific measurement purposes", 2),
            rep("Availability of suitable computational tools for the language(s) that I study", 2),
            rep("Comparability of computational tools in different languages", 2),
            rep("Issues concerning measurement validity/limited nuance", 2),
            rep("Loss of manual contact with the material", 2),
            rep("Reviewers'/editors' skepticism toward computational methods", 2),
            rep("Peers' skepticism toward computational methods", 2),
            rep("I am skeptical toward computational methods myself", 2),
            rep("Computational methods are unnecessary/less relevant for my research", 2)),
  gender = c(rep(c("Male", "Female"), 15))) %>% 
  mutate(lower = mv - 1.96*stdev,
         upper = mv + 1.96*stdev)

c2 <- ggplot(viz, aes(x = mv, y = value, 
                xmin = lower, xmax = upper,
                color = gender)) +
  geom_point(position = position_dodge(.3)) +
  geom_errorbar(width=0,
                position = position_dodge(.3)) +
  labs(x = "Average Score on Challenges per Gender", y = "") +
  scale_color_manual(values = fig_cols) +
  theme_ipsum(axis_text_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) 


