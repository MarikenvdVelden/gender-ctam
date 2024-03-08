library(here)
library(kableExtra)
library(tidyverse)
library(DeclareDesign)
library(scales)
library(cobalt)
library(ggstatsplot)
library(patchwork)
library(qualtRics)
library(margins)
library(hrbrthemes)
library(broom)
library(haven)
library(stringr)


fig_cols <- yarrr::piratepal(palette = "basel", 
             trans = .2)
fig_cols <- as.character(fig_cols[1:8])

api_key_fn <- here("data/raw-private/qualtrics_api_key.txt")
API <- read_file(api_key_fn) %>% trimws()

render_j2 = function(template, output, data, auto_unbox=TRUE, na="string") {
  data = jsonlite::toJSON(data, pretty=TRUE, auto_unbox=auto_unbox, na=na)
  system(glue::glue("env/bin/j2 --format json {template} -o {output}"), input=data)
}

regression <- function(df){
  
  depVarList <- df %>% select(n_val_strat, n_challenges,
                              n_training_needs_gen, n_training_needs_ind)
  indepVarList <- df %>% select(gender, discipline,career, stats_softw, math_softw,
                                netw_softw, quali_softw, txt_softw, txt_platf,
                                opensource_platf, quali_txt, man_txt, comp_txt) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ gender + factor(discipline) + factor(career) +
                                                   stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
                                                   txt_platf + opensource_platf + quali_txt + man_txt + comp_txt,
                                                 data= indepVarList))
  depVarList <- c("DV: Number of Validation Strategies (H1a)",
                  "DV: Number of Reported Challenges (H1b)",
                  "DV: Amount of Training Needs in General (H2a)",
                  "DV: Individual Training Need (H2b)")
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(lower = estimate - (1.65 * std.error),
               upper = estimate + (1.65 * std.error),
               id = depVarList[i]) %>%
        select(term, estimate, lower, upper, id)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(lower = estimate - (1.65 * std.error),
               upper = estimate + (1.65 * std.error),
               id = depVarList[i]) %>%
        select(term, estimate, lower, upper, id)
      m <- m %>%
        add_case(tmp) %>%
        filter(term == "gender")
      
    }
  }
  return(m)
}

regression2 <- function(df){
  
  depVarList <- df %>% select(n_val_strat, n_challenges,
                              n_training_needs_gen, n_training_needs_ind)
  indepVarList <- df %>% select(gender, discipline,career, stats_softw, math_softw,
                                netw_softw, quali_softw, txt_softw, txt_platf,
                                opensource_platf, quali_txt, man_txt, comp_txt2) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ gender * factor(comp_txt2) + factor(discipline) + factor(career) +
                                                   stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
                                                   txt_platf + opensource_platf + quali_txt + man_txt,
                                                 data= indepVarList))
  depVarList <- depVarList <- c("DV: Number of Validation Strategies (H1a)",
                                "DV: Number of Reported Challenges (H1b)",
                                "DV: Amount of Training Needs in General (H2a)",
                                "DV: Individual Training Need (H2b)")
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "gender", at = list(comp_txt2 = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.65 * SE),
               upper = AME + (1.65 * SE),
               method = c("Not Using CTAM <br/> (N=203)","Using CTAM <br/> (N=230)")) %>%
        select(AME, upper, lower, y, method)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "gender", at = list(comp_txt2 = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.65 * SE),
               upper = AME + (1.65 * SE),
               method = c("Not Using CTAM <br/> (N=203)","Using CTAM <br/> (N=230)")) %>%
        select(AME, upper, lower, y, method)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression3 <- function(df){
  
  depVarList <- df %>% select(n_val_strat, n_challenges,
                              n_training_needs_gen, n_training_needs_ind)
  indepVarList <- df %>% select(gender, discipline,career, stats_softw, math_softw,
                                netw_softw, quali_softw, txt_softw, txt_platf,
                                opensource_platf, quali_txt, man_txt, comp_txt) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ gender * factor(discipline) + factor(career) +
                                                   stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
                                                   txt_platf + opensource_platf + quali_txt + man_txt + comp_txt,
                                                 data= indepVarList))
  depVarList <- depVarList <- c("DV: Number of Validation Strategies (H1a)",
                                "DV: Number of Reported Challenges (H1b)",
                                "DV: Amount of Training Needs in General (H2a)",
                                "DV: Individual Training Need (H2b)")
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "gender", at = list(discipline = 0:3))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.65 * SE),
               upper = AME + (1.65 * SE),
               interact = c("Discipline: Other <br/> (N=80)",
                              "Discipline: Communication Science <br/> (N=127)",
                              "Discipline: Political Science <br/> (N=83)",
                              "Multi-Disciplinary <br/> (N=143)")) %>%
        select(AME, upper, lower, y, interact)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "gender", at = list(discipline = 0:3))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.65 * SE),
               upper = AME + (1.65 * SE),
               interact = c("Discipline: Other <br/> (N=80)",
                            "Discipline: Communication Science <br/> (N=127)",
                            "Discipline: Political Science <br/> (N=83)",
                            "Multi-Disciplinary <br/> (N=143)")) %>%
        select(AME, upper, lower, y, interact)
      m <- m %>%
        add_case(tmp)%>%
        mutate(term = "Discipline")
      
    }
  }
  return(m)
}

regression4 <- function(df){
  
  depVarList <- df %>% select(n_val_strat, n_challenges,
                              n_training_needs_gen, n_training_needs_ind)
  indepVarList <- df %>% select(gender, discipline,career, stats_softw, math_softw,
                                netw_softw, quali_softw, txt_softw, txt_platf,
                                opensource_platf, quali_txt, man_txt, comp_txt) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ gender * factor(career) + factor(discipline) +
                                                   stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
                                                   txt_platf + opensource_platf + quali_txt + man_txt + comp_txt,
                                                 data= indepVarList))
  depVarList <- depVarList <- c("DV: Number of Validation Strategies (H1a)",
                                "DV: Number of Reported Challenges (H1b)",
                                "DV: Amount of Training Needs in General (H2a)",
                                "DV: Individual Training Need (H2b)")
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "gender", at = list(career = 0:3))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.65 * SE),
               upper = AME + (1.65 * SE),
               interact = c("PhD Student <br/> (N=38)",
                          "Early-Career Researcher (<5 years since PhD) <br/> (N=110)",
                          "Mid-Career Researcher (5-15 years since PhD) <br/> (N=205)",
                          "Senior Researcher (>15 years since PhD) <br/> (N=80)")) %>%
        select(AME, upper, lower, y, interact)
      
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "gender", at = list(career = 0:3))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.65 * SE),
               upper = AME + (1.65 * SE),
               interact = c("PhD Student <br/> (N=38)",
                            "Early-Career Researcher (<5 years since PhD) <br/> (N=110)",
                            "Mid-Career Researcher (5-15 years since PhD) <br/> (N=205)",
                            "Senior Researcher (>15 years since PhD) <br/> (N=80)")) %>%
        select(AME, upper, lower, y, interact)
      
      m <- m %>%
        add_case(tmp) %>%
        mutate(term = "Career")
      
    }
  }
  return(m)
}

regression5 <- function(df){
  
  depVarList <- df %>% select(v_check_doc, v_check_findings, v_gold_standard, 
                              v_check_rules, v_chech_procedure, v_check_other)
  indepVarList <- df %>% select(gender, discipline,career, stats_softw, math_softw,
                                netw_softw, quali_softw, txt_softw, txt_platf,
                                opensource_platf, quali_txt, man_txt, comp_txt) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ gender + factor(discipline) + factor(career) +
                                                   stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
                                                   txt_platf + opensource_platf + quali_txt + man_txt + comp_txt,
                                                 data= indepVarList))
  depVarList <- c("DV: Check documentation of software developer",
                  "DV: Check finding for plausibility and interpretability",
                  "DV: Check against gold standard",
                  "DV: Check validity of classification rules",
                  "DV: Check algorithmic procedures")
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(lower = estimate - (1.65 * std.error),
               upper = estimate + (1.65 * std.error),
               id = depVarList[i]) %>%
        select(term, estimate, lower, upper, id)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(lower = estimate - (1.65 * std.error),
               upper = estimate + (1.65 * std.error),
               id = depVarList[i]) %>%
        select(term, estimate, lower, upper, id)
      m <- m %>%
        add_case(tmp) %>%
        filter(term == "gender")
      
    }
  }
  return(m)
}

regression6 <- function(df){
  
  depVarList <- df %>% select(v_check_doc, v_check_findings, v_gold_standard, 
                              v_check_rules, v_chech_procedure, v_check_other)
  indepVarList <- df %>% select(gender, discipline,career, stats_softw, math_softw,
                                netw_softw, quali_softw, txt_softw, txt_platf,
                                opensource_platf, quali_txt, man_txt, comp_txt2) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ gender * factor(comp_txt2) + factor(discipline) + factor(career) +
                                                   stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
                                                   txt_platf + opensource_platf + quali_txt + man_txt,
                                                 data= indepVarList))
  depVarList <- depVarList <- c("DV: Check documentation of software developer",
                                "DV: Check finding for plausibility and interpretability",
                                "DV: Check against gold standard",
                                "DV: Check validity of classification rules",
                                "DV: Check algorithmic procedures")
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "gender", at = list(comp_txt2 = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.65 * SE),
               upper = AME + (1.65 * SE),
               method = c("Not Using CTAM <br/> (N=203)","Using CTAM <br/> (N=230)")) %>%
        select(AME, upper, lower, y, method)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "gender", at = list(comp_txt2 = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.65 * SE),
               upper = AME + (1.65 * SE),
               method = c("Not Using CTAM <br/> (N=203)","Using CTAM <br/> (N=230)")) %>%
        select(AME, upper, lower, y, method)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression7 <- function(df){
  
  depVarList <- df %>% select(c_effort, 
                              c_funding, 
                              c_training, 
                              c_instruction_too_high, 
                              c_instruction_too_low, 
                              c_documentation, 
                              c_measurement, 
                              c_language, 
                              c_comparability, 
                              c_validity, 
                              c_loss_contact, 
                              c_editors_skepticism, 
                              c_peers_skepticism, 
                              c_own_skepticism, 
                              c_not_relevant)
  indepVarList <- df %>% select(gender, discipline,career, stats_softw, math_softw,
                                netw_softw, quali_softw, txt_softw, txt_platf,
                                opensource_platf, quali_txt, man_txt, comp_txt) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ gender + factor(discipline) + factor(career) +
                                                   stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
                                                   txt_platf + opensource_platf + quali_txt + man_txt + comp_txt,
                                                 data= indepVarList))
  depVarList <- c("DV: Time/effort required",
                  "DV: Funding required",
                  "DV: Training required",
                  "DV: Level of available instruction/materials higher than needed",
                  "DV: Level of available instruction/materials lower than needed",
                  "DV: Limited methodological guidance/documentation of tools",
                  "DV: Availability of suitable computational tools for specific measurement purposes",
                  "DV: Availability of suitable computational tools for the language(s) that I study",
                  "DV: Comparability of computational tools in different languages",
                  "DV: Issues concerning measurement validity/limited nuance",
                  "DV: Loss of manual contact with the material",
                  "DV: Reviewers’/editors’ skepticism toward computational methods",
                  "DV: Peers’ skepticism toward computational methods",
                  "DV: Own skepticism toward computational methods",
                  "DV: Computational methods are unnecessary/less relevant for my research")
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(lower = estimate - (1.65 * std.error),
               upper = estimate + (1.65 * std.error),
               id = depVarList[i]) %>%
        select(term, estimate, lower, upper, id)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(lower = estimate - (1.65 * std.error),
               upper = estimate + (1.65 * std.error),
               id = depVarList[i]) %>%
        select(term, estimate, lower, upper, id)
      m <- m %>%
        add_case(tmp) %>%
        filter(term == "gender")
      
    }
  }
  return(m)
}

regression8 <- function(df){
  
  depVarList <- df %>% select(c_effort, 
                              c_funding, 
                              c_training, 
                              c_instruction_too_high, 
                              c_instruction_too_low, 
                              c_documentation, 
                              c_measurement, 
                              c_language, 
                              c_comparability, 
                              c_validity, 
                              c_loss_contact, 
                              c_editors_skepticism, 
                              c_peers_skepticism, 
                              c_own_skepticism, 
                              c_not_relevant)
  indepVarList <- df %>% select(gender, discipline,career, stats_softw, math_softw,
                                netw_softw, quali_softw, txt_softw, txt_platf,
                                opensource_platf, quali_txt, man_txt, comp_txt2) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ gender * factor(comp_txt2) + factor(discipline) + factor(career) +
                                                   stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
                                                   txt_platf + opensource_platf + quali_txt + man_txt,
                                                 data= indepVarList))
  depVarList <- depVarList <- c("DV: Time/effort required",
                                "DV: Funding required",
                                "DV: Training required",
                                "DV: Level of available instruction/materials higher than needed",
                                "DV: Level of available instruction/materials lower than needed",
                                "DV: Limited methodological guidance/documentation of tools",
                                "DV: Availability of suitable computational tools for specific measurement purposes",
                                "DV: Availability of suitable computational tools for the language(s) that I study",
                                "DV: Comparability of computational tools in different languages",
                                "DV: Issues concerning measurement validity/limited nuance",
                                "DV: Loss of manual contact with the material",
                                "DV: Reviewers’/editors’ skepticism toward computational methods",
                                "DV: Peers’ skepticism toward computational methods",
                                "DV: Own skepticism toward computational methods",
                                "DV: Computational methods are unnecessary/less relevant for my research")
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "gender", at = list(comp_txt2 = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.65 * SE),
               upper = AME + (1.65 * SE),
               method = c("Not Using CTAM <br/> (N=203)","Using CTAM <br/> (N=230)")) %>%
        select(AME, upper, lower, y, method)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "gender", at = list(comp_txt2 = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.65 * SE),
               upper = AME + (1.65 * SE),
               method = c("Not Using CTAM <br/> (N=203)","Using CTAM <br/> (N=230)")) %>%
        select(AME, upper, lower, y, method)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression9 <- function(df){
  
  depVarList <- df %>% select(t_open_tools,
                              t_software, 
                              t_theory, 
                              t_ethics)
  indepVarList <- df %>% select(gender, discipline,career, stats_softw, math_softw,
                                netw_softw, quali_softw, txt_softw, txt_platf,
                                opensource_platf, quali_txt, man_txt, comp_txt) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ gender + factor(discipline) + factor(career) +
                                                   stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
                                                   txt_platf + opensource_platf + quali_txt + man_txt + comp_txt,
                                                 data= indepVarList))
  depVarList <- c("DV: Data & Open Access Tools",
                  "DV: Programming & Software skills",
                  "DV: Theory & Concepts",
                  "DV: Research Inteegrity & Ethics")
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(lower = estimate - (1.65 * std.error),
               upper = estimate + (1.65 * std.error),
               id = depVarList[i]) %>%
        select(term, estimate, lower, upper, id)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(lower = estimate - (1.65 * std.error),
               upper = estimate + (1.65 * std.error),
               id = depVarList[i]) %>%
        select(term, estimate, lower, upper, id)
      m <- m %>%
        add_case(tmp) %>%
        filter(term == "gender")
      
    }
  }
  return(m)
}

regression10 <- function(df){
  
  depVarList <- df %>% select(t_open_tools,
                              t_software, 
                              t_theory, 
                              t_ethics)
  indepVarList <- df %>% select(gender, discipline,career, stats_softw, math_softw,
                                netw_softw, quali_softw, txt_softw, txt_platf,
                                opensource_platf, quali_txt, man_txt, comp_txt2) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ gender * factor(comp_txt2) + factor(discipline) + factor(career) +
                                                   stats_softw + math_softw + netw_softw + quali_softw + txt_softw + 
                                                   txt_platf + opensource_platf + quali_txt + man_txt,
                                                 data= indepVarList))
  depVarList <- depVarList <- c("DV: Data & Open Access Tools",
                                "DV: Programming & Software skills",
                                "DV: Theory & Concepts",
                                "DV: Research Inteegrity & Ethics")
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "gender", at = list(comp_txt2 = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.65 * SE),
               upper = AME + (1.65 * SE),
               method = c("Not Using CTAM <br/> (N=203)","Using CTAM <br/> (N=230)")) %>%
        select(AME, upper, lower, y, method)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "gender", at = list(comp_txt2 = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.65 * SE),
               upper = AME + (1.65 * SE),
               method = c("Not Using CTAM <br/> (N=203)","Using CTAM <br/> (N=230)")) %>%
        select(AME, upper, lower, y, method)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

