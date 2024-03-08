d2 <- read_sav(here("data/raw-private/multilingualism.sav"))

tmp <- d2 |> 
  select(doi, author1:author4) |> 
  pivot_longer(cols = author1:author4)

length(which(tmp$value[tmp$name=="author3"]==""))/dim(tmp)[1]

# Auteur voornamen na "," -> splitsen en dan gender package check!
d2 <- d2 %>% 
  mutate(fn1 = str_split_fixed(author1, ", ", n = 2)[,2],
         ln1 = str_split_fixed(author1, ", ", n = 2)[,1],
         fn1 = ifelse(fn1=="", str_split_fixed(author1, " ", n = 2)[,1], fn1),
         ln1 = ifelse(fn1=="", str_split_fixed(author1, " ", n = 2)[,1], ln1),
         fn1 = str_split_fixed(fn1, " ", n = 2)[,1],
         fn2 = str_split_fixed(author2, ", ", n = 2)[,2],
         ln2 = str_split_fixed(author2, ", ", n = 2)[,1],
         fn2 = ifelse(fn2=="", str_split_fixed(author2, " ", n = 2)[,1], fn2),
         ln2 = ifelse(fn2=="", str_split_fixed(author2, " ", n = 2)[,1], ln2),
         fn3 = str_split_fixed(author3, ", ", n = 2)[,2],
         ln3 = str_split_fixed(author3, ", ", n = 2)[,1],
         fn3 = ifelse(fn3=="", str_split_fixed(author3, " ", n = 2)[,1], fn3),
         ln3 = ifelse(fn3=="", str_split_fixed(author3, " ", n = 2)[,1], ln3),
         fn4 = str_split_fixed(author4, ", ", n = 2)[,2],
         ln4 = str_split_fixed(author4, ", ", n = 2)[,1],
         fn4 = ifelse(fn4=="", str_split_fixed(author4, " ", n = 2)[,1], fn4),
         ln4 = ifelse(fn4=="", str_split_fixed(author4, " ", n = 2)[,1], ln4))

df <- read_csv(here("data/raw-private/gender_authors.csv"))
df <- df %>% 
  mutate(gender2 = recode(gender2,
                          `mmm` = "m",
                          .default = gender2),
         gender3 = recode(gender3,
                          `d` = "f",
                          `ff` = "f",
                          .default = gender3))

df %>%
  select(gender1, gender2, gender3) %>% 
  pivot_longer(cols = everything()) %>% 
  drop_na(value) %>% 
  group_by(value) %>% 
  count(tot = n()) #in total 711 female authors and 1133 male

df %>%
  select(gender1, gender2, gender3) %>% 
  pivot_longer(cols = everything()) %>% 
  drop_na(value) %>% 
  group_by(value, name) %>% 
  count(tot = n()) %>% 
  mutate(perc = n/tot)

df1 <- df %>% 
  mutate(gender1b = recode(gender1,
                           `m` = 1,
                           `f` = 0)) %>% 
  drop_na(gender1b)
t1 <- t.test(gender1b ~ 1, df1)

df2 <- df %>% 
  mutate(gender2b = recode(gender2,
                           `m` = 1,
                           `f` = 0)) %>% 
  drop_na(gender2b)
t2 <- t.test(gender2b ~ 1, df2)

df3 <- df %>% 
  mutate(gender3b = recode(gender3,
                           `m` = 1,
                           `f` = 0)) %>% 
  drop_na(gender3b)
t3 <- t.test(gender3b ~ 1, df3)


d2 <- d2 %>% 
  mutate(gender1 = df$gender1,
         gender2 = df$gender2,
         gender3 = df$gender3) 

d2 %>% 
  dplyr::select(gender1:gender3) %>% 
  pivot_longer(cols = everything()) %>% 
  count()


d1 <- d2 %>% 
  filter(meth_man == 1) %>% 
  mutate(gender1b = recode(gender1,
                           `m` = 1,
                           `f` = 0)) %>% 
  drop_na(gender1b)
t4 <- t.test(gender1b ~ 1, d1)

d2b <- d2 %>% 
  filter(meth_man == 1) %>% 
  mutate(gender2b = recode(gender2,
                           `m` = 1,
                           `f` = 0)) %>% 
  drop_na(gender2b)
t5 <- t.test(gender2b ~ 1, d2b)

d3 <- d2 %>% 
  filter(meth_man == 1) %>% 
  mutate(gender3b = recode(gender3,
                           `m` = 1,
                           `f` = 0)) %>% 
  drop_na(gender3b)
t6 <- t.test(gender3b ~ 1, d3)

d1 <- d2 %>% 
  filter(meth_man == 0) %>% 
  mutate(gender1b = recode(gender1,
                           `m` = 1,
                           `f` = 0)) %>% 
  drop_na(gender1b)
t7 <- t.test(gender1b ~ 1, d1)

d2b <- d2 %>% 
  filter(meth_man == 0) %>% 
  mutate(gender2b = recode(gender2,
                           `m` = 1,
                           `f` = 0)) %>% 
  drop_na(gender2b)
t8 <- t.test(gender2b ~ 1, d2b)

d3 <- d2 %>% 
  filter(meth_man == 0) %>% 
  mutate(gender3b = recode(gender3,
                           `m` = 1,
                           `f` = 0)) %>% 
  drop_na(gender3b)
t9 <- t.test(gender3b ~ 1, d3)

viz <- tibble(
  mv = c(unlist(t1[5]), unlist(t2[5]), unlist(t3[5]),
         unlist(t4[5]), unlist(t5[5]), unlist(t6[5]),
         unlist(t7[5]), unlist(t8[5]), unlist(t9[5])),
  stdev = c(unlist(t1[7]), unlist(t2[7]), unlist(t3[7]),
            unlist(t4[7]), unlist(t5[7]), unlist(t6[7]),
            unlist(t7[7]), unlist(t8[7]), unlist(t9[7])),
  value = c("First Authorship", "Second Autorship", "Third Authorship",
            "First Authorship", "Second Autorship", "Third Authorship",
            "First Authorship", "Second Autorship", "Third Authorship"),
  type = c(rep("Full Sample", 3),
           rep("Manual Text Analysis Methods", 3),
           rep("Computational Text Analysis Methods", 3))) %>% 
  mutate(lower = mv - 1.96*stdev,
         upper = mv + 1.96*stdev,
         value = factor(value,
                        levels = c("Third Authorship",
                                   "Second Autorship",
                                   "First Authorship")),
         type = factor(type,
                       levels = c("Full Sample",
                                  "Computational Text Analysis Methods",
                                  "Manual Text Analysis Methods")))
r1 <- ggplot(viz, aes(x = mv, y = value, 
                xmin = lower, xmax = upper,
                color = type)) +
  geom_point(position = position_dodge(.3)) +
  geom_errorbar(width=0,
                position = position_dodge(.3)) +
  labs(x = "Likelihood of Male Scholar being Nth Author compared to Female Scholar", y = "") +
  theme_ipsum(axis_text_size = 14) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  geom_vline(xintercept = .5, linetype = "dashed", color = "gray35")

tmp <- d2 %>% 
  select(gender1:gender3, meth_man) %>% 
  pivot_longer(cols = gender1:gender3) %>% 
  drop_na(value) %>% 
  mutate(value = recode(value,
                        `m` = 1,
                        `f` = 0))
  #group_by(value) %>%
  #group_by(value, meth_man) %>% 
  #count()

t.test(value ~ 1, tmp)
