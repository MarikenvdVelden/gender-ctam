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

theme_ipsum <- function(base_family="Arial Narrow", base_size = 12,
                        plot_title_family=base_family, plot_title_size = 24,
                        plot_title_face="bold", plot_title_margin = 10,
                        subtitle_family=base_family, subtitle_size = 20,
                        subtitle_face = "plain", subtitle_margin = 15,
                        strip_text_family = base_family, strip_text_size = 12,
                        strip_text_face = "plain",
                        caption_family = base_family, caption_size = 9,
                        caption_face = "italic", caption_margin = 10,
                        axis_text_size = base_size,
                        axis_title_family = subtitle_family, axis_title_size = 10,
                        axis_title_face = "plain", axis_title_just = "rt",
                        plot_margin = margin(30, 30, 30, 30),
                        grid_col = "#CCCCCC", grid = TRUE,
                        axis_col = "#CCCCCC", axis = FALSE, ticks = FALSE) {
  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)
  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())
  if (inherits(grid, "character") | grid == TRUE) {
    ret <- ret + theme(panel.grid=element_line(color=grid_col, size=0.2))
    ret <- ret + theme(panel.grid.major=element_line(color=grid_col, size=0.2))
    ret <- ret + theme(panel.grid.minor=element_line(color=grid_col, size=0.15))
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
    }
  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color="#2B2B2B", size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  ret <- ret + theme(axis.text.x=element_text(size=axis_text_size, margin=margin(t=0)))
  ret <- ret + theme(axis.text.y=element_text(size=axis_text_size, margin=margin(r=0)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y.right=element_text(hjust=yj, size=axis_title_size, angle=90,
                                                     family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face, family=strip_text_family))
  ret <- ret + theme(panel.spacing=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0.5, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family, face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family, face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family, face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)
  ret
}
