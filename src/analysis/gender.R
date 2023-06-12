dt <- haven::read_sav(here("data/raw-private/multilingualism.sav")) %>% 
  select(journal, year, doi, author1:author4) %>% 
  mutate(firstname1 = str_split_fixed(author1, ", ", 2)[,2],
         firstname2 = str_split_fixed(author2, ", ", 2)[,2],
         firstname3 = str_split_fixed(author3, ", ", 2)[,2],
         firstname4 = str_split_fixed(author4, ", ", 2)[,2]) %>% 
  drop_na()

dg <- gender::gender(dt$firstname1, method = "ssa")
dg <- dg %>% 
  mutate(gender = recode(gender,
                         `male` = 1,
                         `female` = 0),
         order = 1)
t.test(dg$gender)       

tmp <- gender::gender(dt$firstname2, method = "ssa") %>% 
  mutate(gender = recode(gender,
                         `male` = 1,
                         `female` = 0),
         order = 2)

dg <- dg %>% 
  add_case(tmp)

tmp <- gender::gender(dt$firstname3, method = "ssa") %>% 
  mutate(gender = recode(gender,
                         `male` = 1,
                         `female` = 0),
         order = 3)

dg <- dg %>% 
  add_case(tmp)

dg %>% 
  group_by(order, gender) %>% 
  count() %>% 
  mutate(perc = ifelse(order==1, n/481, 0),
         perc= ifelse(order == 2, n/359, perc),
         perc = ifelse(order == 3, n/195, perc),
         perc_gender = ifelse(gender == 0, n/445, 0),
         perc_gender = ifelse(gender == 1, n/590, perc_gender))

t.test(dg$gender)       

  

