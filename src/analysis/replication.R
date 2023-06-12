dr <- dr %>% 
  filter(consent == "I consent") %>% 
  select(id = ResponseId,
         gender = gender,
         discipline = field,
         career = rank,
         q2_1:q3_8,
         nocomp1_1:nocomp1_11) %>%
  mutate_at(vars(q2_1:q3_8), 
            funs(recode(., 
                        `I don't use it` = 1, 
                        `I only use it while collaborating with others` = 2,
                        `I rarely use it` = 3,
                        `I regularly use it` = 4,
                        `I would like to use it in the future` = 5))) %>% 
  mutate(gender = recode(gender, `a woman` = "female", 
                         `a man` = "male", 
                         .default = "other")

colnames(dr)
table(dr$q2_1)
