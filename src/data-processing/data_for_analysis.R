#select variables needed & mutate them
d <- df %>% 
  select(id, matches("Q11_"), matches("Q10"), matches("Q15"),
         matches("Q16"), matches("Q18"), career, gender, 
         matches("Q3"), matches("Q2")) %>%
  mutate_at(vars(Q10_1:Q10_15),
            funs(recode(.,`0` = 0,`1` = 0, .default = 1, .missing = 0))) %>%
  rowwise(id) %>% 
  mutate(n_val_strat = sum(c(Q11_1, Q11_2, Q11_3, Q11_4, Q11_5, Q11_Other)),
         n_challenges = sum(c(Q10_1, Q10_2, Q10_3, Q10_4, Q10_5, Q10_6, 
                              Q10_7, Q10_8, Q10_9, Q10_10, Q10_11, Q10_12, 
                              Q10_13, Q10_14, Q10_15)),
         n_training_needs_gen = sum(c(Q15_1, Q15_2, Q15_3, Q15_4))) %>% 
  ungroup() %>%
  mutate(n_training_needs_ind = if_else(Q16 ==  1, 1, 0),
         gender = if_else(gender == "female", 1, 0),
         n_val_strat = if_else(is.na(n_val_strat), round(mean(n_val_strat, na.rm=T)), n_val_strat),
         n_challenges = if_else(is.na(n_challenges), round(mean(n_challenges, na.rm=T)), n_challenges),
         n_training_needs_gen = if_else(is.na(n_training_needs_gen), round(mean(n_training_needs_gen, na.rm=T)), 
                                        n_training_needs_gen),
         n_training_needs_ind_miss = if_else(is.na(n_training_needs_ind), 1, 0),
         n_training_needs_ind = if_else(is.na(n_training_needs_ind), round(mean(n_training_needs_ind, na.rm=T)), 
                                        n_training_needs_ind),
         comsci = if_else(Q18_1==1, 1, 0),
         econ = if_else(Q18_2==1, 1, 0),
         polisci = if_else(Q18_3==1, 1, 0),
         psy = if_else(Q18_4==1, 1, 0),
         soc = if_else(Q18_5==1, 1, 0),
         other = if_else(Q18_Other==1,1,0),
         stats_softw = if_else(is.na(Q3_1), round(mean(Q3_1, na.rm=T)), Q3_1),
         math_softw = if_else(is.na(Q3_2), round(mean(Q3_2, na.rm=T)), Q3_2),
         netw_softw = if_else(is.na(Q3_3), round(mean(Q3_3, na.rm=T)), Q3_3),
         quali_softw = if_else(is.na(Q3_4), round(mean(Q3_4, na.rm=T)), Q3_4),
         txt_softw = if_else(is.na(Q3_5), round(mean(Q3_5, na.rm=T)), Q3_5),
         txt_platf = if_else(is.na(Q3_6), round(mean(Q3_6, na.rm=T)), Q3_6),
         opensource_platf = if_else(is.na(Q3_7), round(mean(Q3_7, na.rm=T)), Q3_7),
         quali_txt = if_else(is.na(Q2_1), round(mean(Q2_1, na.rm=T)), Q2_1),
         man_txt = if_else(is.na(Q2_2), round(mean(Q2_2, na.rm=T)), Q2_2),
         comp_txt = if_else(is.na(Q2_3), round(mean(Q2_3, na.rm=T)), Q2_3)) %>% 
  select(id, gender, n_val_strat, n_challenges, n_training_needs_gen, n_training_needs_ind,
         comsci, econ, polisci, psy, soc, other, stats_softw,
         math_softw, netw_softw, quali_softw,
         txt_softw, txt_platf, opensource_platf,
         quali_txt, man_txt, comp_txt)
  
remove(df)
                