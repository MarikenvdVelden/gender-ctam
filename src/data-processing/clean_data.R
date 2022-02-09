
## clean methods & text block
Q1 <- d %>%
  select(id = ResponseId, matches("Q1_\\d+"), -Q1_7_TEXT) %>%
  pivot_longer(cols = matches("Q1_\\d+")) %>%
  mutate(Q1_label = recode(name,
                           `Q1_1` = "Q1_citizens",
                           `Q1_4` = "Q1_media",
                           `Q1_5` = "Q1_parties",
                           `Q1_6` = "Q1_governments",
                           `Q1_7` = "Q1_other",
                           `Q1_8` = "Q1_politicians",
                           `Q1_9` = "Q1_ngo",
                           `Q1_10` = "Q1_commercialorg",
                           `Q1_11` = "Q1_none"),
         Q1 = replace_na(value, 0)) %>%
  select(id, Q1, Q1_label) %>%
  pivot_wider(names_from = Q1_label, values_from = Q1)

tmp <- d %>%
  select(id = ResponseId, Q1_other_text = Q1_7_TEXT)
Q1 <- left_join(Q1, tmp, by = "id")

mt <- d %>%
  select(id = ResponseId, matches("Q2_\\d+"),
         matches("Q3_\\d+"), Q3_other_software = Q3x, Q3_coding = Q3) %>%
  mutate_at(vars(Q2_1:Q3_7), 
            funs(recode(., `6` = 2, `7` = 3,`8` = 4,`9` = 5)))
mt <- left_join(Q1, mt, by = "id")

## clean languages block
Q4 <- d %>%
  select(id = ResponseId, matches("Q4_\\d+"), -Q4_46_TEXT) %>%
  pivot_longer(cols = matches("Q4_\\d+"),
               values_to = "Q4") %>%
  drop_na() %>%
  mutate(name = recode(name,
                       `Q4_1` = "German",
                       `Q4_2` = "Danish",
                       `Q4_3` = "English",
                       `Q4_4` = "Icelandic",
                       `Q4_5` = "Dutch",
                       `Q4_6` = "Norwegian",
                       `Q4_7` = "Swedish",
                       `Q4_8` = "Catalan",
                       `Q4_9` = "Spanish",
                       `Q4_10` = "French",
                       `Q4_11` = "Italian",
                       `Q4_12` = "Portuguese",
                       `Q4_13` = "Romanian",
                       `Q4_14` = "Bosnian",
                       `Q4_15` = "Bulgarian",
                       `Q4_16` = "Czech",
                       `Q4_17` = "Polish",
                       `Q4_18` = "Russian",
                       `Q4_19` = "Serbian",
                       `Q4_20` = "Slovak",
                       `Q4_21` = "Slovanian",
                       `Q4_22` = "Ukrainian",
                       `Q4_23` = "Albanian",
                       `Q4_24` = "Farsi",
                       `Q4_25` = "Greek",
                       `Q4_26` = "Lithuanian",
                       `Q4_27` = "Latvian",
                       `Q4_28` = "Sinhalese",
                       `Q4_29` = "Arabic",
                       `Q4_30` = "Hebrew",
                       `Q4_31` = "Estonian",
                       `Q4_32` = "Hungarian",
                       `Q4_33` = "Finnish",
                       `Q4_34` = "Turkish",
                       `Q4_35` = "Chinese (Mandarin)",
                       `Q4_36` = "Japanese",
                       `Q4_37` = "Korean",
                       `Q4_38` = "Malayalam",
                       `Q4_39` = "Tagalog",
                       `Q4_40` = "Thai",
                       `Q4_41` = "Tamil",
                       `Q4_42` = "Vietnamese",
                       `Q4_43` = "Croatian",
                       `Q4_44` = "Gaelic",
                       `Q4_45` = "Maltese",
                       `Q4_46` = "Other",
                       `Q4_47` = "Hindi",
                       `Q4_48` = "Punjabi",
                       `Q4_49` = "Bengali",
                       `Q4_50` = "Indonesian",
                       `Q4_51` = "Marathi",
                       `Q4_52` = "Telugu",
                       `Q4_53` = "Urdu",
                       `Q4_54` = "Javanese",
                       `Q4_55` = "Kurdish",
                       `Q4_56` = "Amharic",
                       `Q4_57` = "Zulu",
                       `Q4_58` = "Swahili",
                       `Q4_59` = "Chinese (Wu)",
                       `Q4_60` = "Chinese (Yue)")) %>%
  pivot_wider(values_from = name) %>%
  mutate_at(vars(Dutch:Ukrainian), 
            funs(recode(., .missing = 0, .default = 1))) %>%
  select(-Q4) %>%
  mutate(n_languages = rowSums(across(where(is.numeric))))

tmp <- d %>%
  select(id = ResponseId, Q4_other_text = Q4_46_TEXT)
Q4 <- left_join(Q4, tmp, by = "id")

Q5 <- d %>%
  select(id = ResponseId, matches("Q5_\\d+"), -Q5_9_TEXT) %>%
  mutate_at(vars(Q5_1:Q5_9), 
            funs(recode(., `4` = 1, `5` = 2,`6` = 3, .missing = 0)))

Q6 <- d %>%
  select(id = ResponseId, matches("Q6_")) %>%
  unite(Q6, matches("Q6_"), na.rm = T) %>%
  mutate(Q6 = as.numeric(Q6),
         Q6 = recode(Q6, `1` = 0, `2` = 1))


Q7 <- d %>%
  select(id = ResponseId, matches("Q7_\\w+_\\d+")) %>%
  unite(Q7_1, matches("Q7_\\w+_1"), na.rm = T) %>%
  unite(Q7_2, matches("Q7_\\w+_2"), na.rm = T) %>%
  unite(Q7_3, matches("Q7_\\w+_3"), na.rm = T) %>%
  unite(Q7_4, matches("Q7_\\w+_4"), na.rm = T) %>%
  unite(Q7_5, matches("Q7_\\w+_5"), na.rm = T) %>%
  unite(Q7_6, matches("Q7_\\w+_6"), na.rm = T) %>%
  unite(Q7_7, matches("Q7_\\w+_7"), na.rm = T) %>%
  unite(Q7_8, matches("Q7_\\w+_8"), na.rm = T) %>%
  unite(Q7_Other_text, matches("Q7_\\w+_9_"), na.rm = T) %>%
  unite(Q7_Other, matches("Q7_\\w+_9"), na.rm = T) %>%
  mutate_at(vars(Q7_1:Q7_Other),
            funs(recode(., `4` = 1, `5` = 2, `6` = 3, .default = 0)))

Q8 <- d %>%
  select(id = ResponseId, matches("Q8_\\w+")) %>%
  rename(Q8_1 = Q8_multilingual_1, Q8_2 = Q8_multilingual_2, Q8_3 = Q8_multilingual_3) %>%
  mutate_at(vars(Q8_1:Q8_3),
            funs(recode(., `4` = 1, `5` = 2, `6` = 3, .default = 0, .missing = 0)))

lan <- left_join(Q4, Q5, by = "id")  
lan <- left_join(lan, Q6, by = "id")  
lan <- left_join(lan, Q7, by = "id")  
lan <- left_join(lan, Q8, by = "id")  

## clean computational block
Q9 <- d %>%
  select(id = ResponseId, matches("Q9_\\w+"), Q9_Other = Q9x_cm) %>%
  rename(Q9_1 = Q9_cm_1, Q9_2 = Q9_cm_2, Q9_3 = Q9_cm_3, 
         Q9_4 = Q9_cm_4, Q9_5 = Q9_cm_5, Q9_6 = Q9_cm_6,
         Q9_7 = Q9_cm_7, Q9_8 = Q9_cm_8, Q9_9 = Q9_cm_9,
         Q9_10 = Q9_cm_10, Q9_11 = Q9_cm_11, Q9_12 = Q9_cm_12) %>%
  mutate_at(vars(Q9_1:Q9_12),
            funs(recode(., .missing = 0)))

Q10 <- d %>%
  select(id = ResponseId, matches("Q10_\\w+_\\d+"),
         matches("Q10x_\\w+")) %>%
  unite(Q10_Other, matches("Q10x_\\w+"), na.rm = T) %>%
  unite(Q10_10, matches("Q10_\\w+_10"), na.rm = T) %>%
  unite(Q10_11, matches("Q10_\\w+_11"), na.rm = T) %>%
  unite(Q10_12, matches("Q10_\\w+_12"), na.rm = T) %>%
  unite(Q10_13, matches("Q10_\\w+_13"), na.rm = T) %>%
  unite(Q10_14, matches("Q10_\\w+_14"), na.rm = T) %>%
  unite(Q10_15, matches("Q10_\\w+_15"), na.rm = T) %>%
  unite(Q10_1, matches("Q10_\\w+_1"), na.rm = T) %>%
  unite(Q10_2, matches("Q10_\\w+_2"), na.rm = T) %>%
  unite(Q10_3, matches("Q10_\\w+_3"), na.rm = T) %>%
  unite(Q10_4, matches("Q10_\\w+_4"), na.rm = T) %>%
  unite(Q10_5, matches("Q10_\\w+_5"), na.rm = T) %>%
  unite(Q10_6, matches("Q10_\\w+_6"), na.rm = T) %>%
  unite(Q10_7, matches("Q10_\\w+_7"), na.rm = T) %>%
  unite(Q10_8, matches("Q10_\\w+_8"), na.rm = T) %>%
  unite(Q10_9, matches("Q10_\\w+_9"), na.rm = T) %>%
  mutate_at(vars(Q10_1:Q10_15),
            funs(recode(., `4` = 1, `5` = 2, `6` = 3, .default = 0, .missing = 0)))

Q10c <- d %>%
  select(id = ResponseId, matches("Q10c_\\w+_\\d+")) %>%
  unite(Q10c_Other_text, matches("Q10c_\\w+_13_TEXT"), na.rm = T) %>%
  unite(Q10c_Other, matches("Q10c_\\w+_13"), na.rm = T) %>%
  unite(Q10c_4, matches("Q10c_\\w+_10"), na.rm = T) %>%
  unite(Q10c_5, matches("Q10c_\\w+_11"), na.rm = T) %>%
  unite(Q10c_6, matches("Q10c_\\w+_12"), na.rm = T) %>%
  unite(Q10c_2, matches("Q10c_\\w+_8"), na.rm = T) %>%
  unite(Q10c_3, matches("Q10c_\\w+_9"), na.rm = T) %>%
  unite(Q10c_1, matches("Q10c_\\w+_1"), na.rm = T) %>%
  select(id:Q10c_5, Q10c_6, Q10c_Other, Q10c_Other_text) %>%
  mutate_at(vars(Q10c_1:Q10c_Other),
            funs(recode(.,`1` = 1, .default = 0, .missing = 0)))

Q11 <- d %>%
  select(id = ResponseId, matches("Q11_\\d+")) %>%
  rename(Q11_2 = Q11_8, Q11_3 = Q11_9, Q11_4 = Q11_10,
         Q11_5 = Q11_11, Q11_Other = Q11_13, 
         Q11_Other_text  = Q11_13_TEXT) %>%
  mutate_at(vars(Q11_1:Q11_Other),
            funs(recode(.,`1` = 1, .default = 0, .missing = 0)))

cm <- left_join(Q9, Q10, by = "id")
cm <- left_join(cm, Q10c, by = "id")
cm <- left_join(cm, Q11, by = "id")

## clean Attitude towards Computational Methods block
att <- d %>%
  select(id = ResponseId, matches("Q12\\w+"), Q13) %>%
  mutate_at(vars(Q12a_1:Q12b_2),
            funs(recode(., .missing = 0))) %>%
  mutate(Q13 = recode(Q13, `1` = 1, `11` = 2, `12` = 3, `13` = 4, .missing = 0, .default = 0))

## clean Training block
colnames(d)[230] <- "Q16_1"
train <- d %>%
  select(id = ResponseId, Q14, matches("Q15_\\d+"),
         matches("Q16_\\d+"), matches("Q16a_\\d+"),
         matches("Q17_\\d+")) %>%
  rename(Q15_2 = Q15_4, Q15_3 = Q15_5, Q15_4 = Q15_6, Q15_5 = Q15_7,
         Q15_1_text = Q15_1_TEXT, Q15_2_text = Q15_4_TEXT, 
         Q15_3_text = Q15_5_TEXT, Q15_4_text = Q15_6_TEXT, 
         Q15_5_text = Q15_7_TEXT, Q16 = Q16_1,
         Q16_yes_other = Q16_7_TEXT, Q16_no_other = Q16_10_TEXT) %>%
  mutate(Q16 = recode(Q16,`7` = 1, `8` = 2, `9` = 3, `10` = 4),
         Q14 = recode(Q14, .missing = 0)) %>%
  mutate_at(vars(Q15_1:Q15_5),
            funs(recode(.,`1` = 1, .default = 0, .missing = 0))) %>% 
  mutate_at(vars(Q16a_1:Q16a_3),
            funs(recode(.,`1` = 1, .default = 0, .missing = 0)))

## clean demographics block
colnames(d)[49] <- "Q16_2"
bg <- d %>%
  select(id = ResponseId, matches("Q18_\\d+"),
         career = Q19, gender = Q17, mother_tongue = Q16_2,
         start_date = StartDate, end_date = EndDate, 
         first_name = RecipientFirstName, last_name = RecipientLastName)  %>%
  rename(Q18_2 = Q18_6, Q18_3 = Q18_7, Q18_4 = Q18_8, Q18_5 = Q18_9,
         Q18_Other = Q18_10, Q18_Other_text = Q18_10_TEXT) %>%
  mutate_at(vars(Q18_1:Q18_Other),
            funs(recode(.,`1` = 1, .default = 0, .missing = 0))) %>%
  mutate(career = recode(career, `6` = 2, `7` = 3,`8` = 4,`9` = 5, .missing = 0),
         gender = recode(gender, `1` = "female", `5` = "male", 
                         `6` = "other"))

## Paste all together
df <- left_join(bg, mt, by = "id")
df <- left_join(df, lan, by = "id")
df <- left_join(df, cm, by = "id")
df <- left_join(df, att, by = "id")
df <- left_join(df, train, by = "id")

df <- df %>%
  select(-first_name, -last_name) 

rm(d, bg, tmp, mt, Q1, lan, cm, att, train, Q4, Q5, Q6, Q7, Q8,Q9, Q10, Q10c, Q11)
