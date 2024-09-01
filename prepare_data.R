# Title: Data pre-processing for the temporal scheduling and preference project
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-01-04
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Load packages
library(tidyverse)

# Experiment 1 ------------------------------------------------------------

exp1_prepared <- read_csv("./data/raw/exp1.csv") %>%
  drop_na() %>%
  mutate(
    sex = factor(sex, levels = c("female", "male"), labels = c("Female", "Male")),
    time = factor(time, levels = c("tomorrow", "future"), labels = c("Tomorrow", "Future")),
    choice = factor(choice, levels = c("easy", "hard"), labels = c("Easy and Boring", "Hard and Interesting"))
  )

saveRDS(exp1_prepared, "./data/prepared/exp1.Rds")

# Experiment 2 ------------------------------------------------------------

# Create coding for task attribute preference
labs <- c("Del", "Dif", "Att")
x0 <- expand.grid(x = 1:3, y = 1:3, z = 1:3)
names(x0) <- labs
check <- unlist(apply(x0, 1, function(x) length(unique(x))))
x1 <- x0[check == length(labs), ]
ord_atts <- with(x1, paste0(Del, Dif, Att))
pref_atts <- apply(t(apply(x1, 1, function(x) labs[order(as.numeric(x))])), 1, paste, collapse = "")
names(pref_atts) <- ord_atts

# Create coding for task type preference 
labs <- abbreviate(c("EasyInteresting", "EasyBoring", "HardInteresting", "HardBoring"))
x0 <- expand.grid(x = 1:4, y = 1:4, z = 1:4, v = 1:4)
names(x0) <- labs
check <- unlist(apply(x0, 1, function(x) length(unique(x))))
x1 <- x0[check == length(labs), ]
ord_tasks <- with(x1, paste0(EsyI, EsyB, HrdI, HrdB))
pref_tasks <- apply(t(apply(x1, 1, function(x) labs[order(as.numeric(x))])), 1, paste, collapse = "")
names(pref_tasks) <- ord_tasks

# Restructure data
exp2_restructured <- read_csv("./data/raw/exp2.csv") %>%
  slice(3:n()) %>%
  mutate(
    pref_atts = str_c(Q7.1_1, Q7.1_2, Q7.1_3),
    pref_atts = recode(pref_atts, !!!pref_atts),
    pref_tasks = str_c(Q7.2_1, Q7.2_2, Q7.2_3, Q7.2_4),
    pref_tasks = recode(pref_tasks, !!!pref_tasks)
  ) %>%
  select(
    id = ResponseId,
    gender = Q9.1,
    age = Q9.2,
    focus = Q9.3,
    serious = Q9.4,
    attention_check = Q179,
    Q3.2:Q3.7, # TC
    Q4.2:Q4.7, # TN
    Q5.2:Q5.7, # LC
    Q6.2:Q6.7, # LN
    pref_atts,
    pref_tasks,
    which_now = Q7.3,
    which_later = Q7.4,
    TCestimate, TNestimate, LCestimate, LNestimate
  ) %>%
  gather(tasks, choice, Q3.2:Q6.7) %>%
  mutate(
    tasks = case_when(
      str_detect(tasks, "Q3") ~ "HardInteresting",
      str_detect(tasks, "Q4") ~ "HardBoring",
      str_detect(tasks, "Q5") ~ "EasyInteresting",
      str_detect(tasks, "Q6") ~ "EasyBoring"
    ),
    delay = case_when(
      tasks == "HardInteresting" ~ TCestimate,
      tasks == "HardBoring" ~ TNestimate,
      tasks == "EasyInteresting" ~ LCestimate,
      tasks == "EasyBoring" ~ LNestimate
    ),
    choice = case_when(str_detect(choice, "wcześniej") ~ "Earlier", str_detect(choice, "później") ~ "Later"),
    gender = case_when(
      gender == "Mężczyzna" ~ "Male",
      gender == "Kobieta" ~ "Female",
      T ~ "Other"
    ),
    attention_check = case_when(is.na(attention_check) ~ 1, T ~ 0),
    across(
      c(which_now, which_later), 
      ~case_when(
        str_detect(.x, "trudne") & str_detect(.x, "ciekawe") ~ "HardInteresting",
        str_detect(.x, "trudne") & str_detect(.x, "nudne") ~ "HardBoring",
        str_detect(.x, "łatwe") & str_detect(.x, "ciekawe") ~ "EasyInteresting",
        str_detect(.x, "łatwe") & str_detect(.x, "nudne") ~ "EasyBoring"
      )
    ),
    condition = tasks,
    across(c(which_now, which_later, condition), abbreviate),
    difficulty = case_when(
      str_detect(condition, "Esy") ~ "Easy",
      str_detect(condition, "Hrd") ~ "Hard",
    ),
    attractiveness = case_when(
      str_detect(condition, "B$") ~ "Boring",
      str_detect(condition, "I$") ~ "Interesting"
    )
  ) %>%
  select(-contains("estimate")) %>%
  arrange(id, delay) %>%
  group_by(id) %>%
  mutate(
    trial = 1:n()
  ) %>%
  ungroup() %>%
  select(
    id, gender, age, attention_check, focus, serious,
    pref_atts, pref_tasks, which_now, which_later, 
    trial, condition, delay, difficulty, attractiveness, choice
  )

# Filter and transform
exp2_prepared <- exp2_restructured %>%
  filter(attention_check == 1) %>%
  select(-attention_check) %>%
  drop_na() %>%
  group_by(id, condition) %>%
  filter(trial == max(trial)) %>%
  ungroup() %>%
  mutate(
    condition = as.factor(condition),
    difficulty = factor(difficulty, levels = c("Easy", "Hard")),
    attractiveness = factor(attractiveness, levels = c("Boring", "Interesting")),
    choice = factor(choice, levels = c("Earlier", "Later")),
    across(c(id, gender, pref_atts, pref_tasks), as.factor),
    across(c(which_now, which_later), as.factor),
    across(!where(is.factor), as.numeric),
    log_t = if_else(delay > 0, log(delay), delay)
  )

saveRDS(exp1_prepared, "./data/prepared/exp2.Rds")

# Experiment 3 ------------------------------------------------------------

# Create coding for task attribute preference
labs <- c("Del", "Dif", "Att")
x0 <- expand.grid(x = 1:3, y = 1:3, z = 1:3)
names(x0) <- labs
check <- unlist(apply(x0, 1, function(x) length(unique(x))))
x1 <- x0[check == length(labs), ]
ord_atts <- with(x1, paste0(Del, Dif, Att))
pref_atts <- apply(t(apply(x1, 1, function(x) labs[order(as.numeric(x))])), 1, paste, collapse = "")
names(pref_atts) <- ord_atts

# Create coding for task type preference 
labs <- abbreviate(c("EasyInteresting", "EasyBoring", "HardInteresting", "HardBoring"))
x0 <- expand.grid(x = 1:4, y = 1:4, z = 1:4, v = 1:4)
names(x0) <- labs
check <- unlist(apply(x0, 1, function(x) length(unique(x))))
x1 <- x0[check == length(labs), ]
ord_tasks <- with(x1, paste0(EsyI, EsyB, HrdI, HrdB))
pref_tasks <- apply(t(apply(x1, 1, function(x) labs[order(as.numeric(x))])), 1, paste, collapse = "")
names(pref_tasks) <- ord_tasks

# Restructure data
exp3_restructured <- read_csv("./data/raw/exp3.csv") %>%
  slice(3:n()) %>%
  filter(Finished == "True") %>%
  mutate(
    pref_atts = str_c(pref_atts_1, pref_atts_2, pref_atts_3),
    pref_atts = recode(pref_atts, !!!pref_atts),
    pref_tasks = str_c(pref_tasks_1, pref_tasks_2, pref_tasks_3, pref_tasks_4),
    pref_tasks = recode(pref_tasks, !!!pref_tasks)
  ) %>%
  select(
    id = ResponseId, gender, age, attn_check,
    pref_atts, pref_tasks,
    which_now, which_later,
    EsyBVSHrdI_1:EsyBVSHrdI_6,
    EsyIVSHrdI_1:EsyIVSHrdI_6,
    EsyIVSHrdB_1:EsyIVSHrdB_6,
    EsyBVSHrdB_1:EsyBVSHrdB_6,
    EsyBVSEsyI_1:EsyBVSEsyI_6,
    HrdBVSHrdI_1:HrdBVSHrdI_6
  ) %>%
  gather(tasks, choice, EsyBVSHrdI_1:HrdBVSHrdI_6) %>%
  mutate(
    attn_check = if_else(is.na(attn_check), 1, 0),
    gender = case_when(
      gender == "Mężczyzna" ~ "Male",
      gender == "Kobieta" ~ "Female",
      T ~ "Other"
    ),
    condition = str_remove_all(tasks, "\\d+|_"),
    delay = case_when(
      str_detect(choice, "\\d{1,2} dni") ~ as.numeric(str_extract(choice, "\\d{1,2}")),
      str_detect(choice, "\\d{1} mies") ~ as.numeric(str_extract(choice, "\\d{1}")) * 30,
      str_detect(choice, "dzisiaj") ~ 0
    ),
    OptionA = str_extract(condition, "\\w+(?=VS)"),
    OptionB = str_extract(condition, "(?<=VS)\\w+"),
    across(
      c(which_now, which_later, choice),
      ~case_when(
        str_detect(.x, "trudne") & str_detect(.x, "ciekawe") ~ "HrdI",
        str_detect(.x, "trudne") & str_detect(.x, "nudne") ~ "HrdB",
        str_detect(.x, "łatwe") & str_detect(.x, "ciekawe") ~ "EsyI",
        str_detect(.x, "łatwe") & str_detect(.x, "nudne") ~ "EsyB"
      )
    ),
    chosenopt = case_when(choice == OptionA ~ "Option A", choice == OptionB ~ "Option B")
  ) %>%
  arrange(id, tasks, delay) %>%
  group_by(id) %>%
  mutate(trial = 1:n()) %>%
  ungroup() %>%
  select(-tasks) %>%
  gather(opt, alt, OptionA, OptionB) %>%
  mutate(
    difficulty = case_when(
      str_detect(alt, "Esy") ~ "Easy",
      str_detect(alt, "Hrd") ~ "Hard",
    ),
    attractiveness = case_when(
      str_detect(alt, "B$") ~ "Boring",
      str_detect(alt, "I$") ~ "Interesting"
    ),
    chosen = if_else(choice == alt, 1, 0)
  ) %>%
  arrange(id, trial) %>%
  mutate(chid = rep(row_number(), each = 2, length.out = n())) %>%
  group_by(chid) %>%
  mutate(
    diff_difficulty = case_when(
      row_number() == 1 & difficulty[1] == "Hard" & difficulty[2] == "Easy" ~ "More Difficult",
      row_number() == 1 & difficulty[1] == "Hard" & difficulty[2] == "Hard" ~ "Same Difficulty",
      row_number() == 1 & difficulty[1] == "Easy" & difficulty[2] == "Easy" ~ "Same Difficulty",
      row_number() == 1 & difficulty[1] == "Easy" & difficulty[2] == "Hard" ~ "Less Difficult",
      row_number() == 2 & difficulty[2] == "Hard" & difficulty[1] == "Easy" ~ "More Difficult",
      row_number() == 2 & difficulty[2] == "Hard" & difficulty[1] == "Hard" ~ "Same Difficulty",
      row_number() == 2 & difficulty[2] == "Easy" & difficulty[1] == "Easy" ~ "Same Difficulty",
      row_number() == 2 & difficulty[2] == "Easy" & difficulty[1] == "Hard" ~ "Less Difficult"
    ),
    diff_attractiveness = case_when(
      row_number() == 1 & attractiveness[1] == "Interesting" & attractiveness[2] == "Boring" ~ "More Attractive",
      row_number() == 1 & attractiveness[1] == "Interesting" & attractiveness[2] == "Interesting" ~ "Same Attractiveness",
      row_number() == 1 & attractiveness[1] == "Boring" & attractiveness[2] == "Boring" ~ "Same Attractiveness",
      row_number() == 1 & attractiveness[1] == "Boring" & attractiveness[2] == "Interesting" ~ "Less Attractive",
      row_number() == 2 & attractiveness[2] == "Interesting" & attractiveness[1] == "Boring" ~ "More Attractive",
      row_number() == 2 & attractiveness[2] == "Interesting" & attractiveness[1] == "Interesting" ~ "Same Attractiveness",
      row_number() == 2 & attractiveness[2] == "Boring" & attractiveness[1] == "Boring" ~ "Same Attractiveness",
      row_number() == 2 & attractiveness[2] == "Boring" & attractiveness[1] == "Interesting" ~ "Less Attractive"
    )
  ) %>%
  select(
    id, attn_check, gender, age,
    pref_atts, pref_tasks, which_now, which_later,
    chid, trial, condition, alt, 
    delay, difficulty, attractiveness, diff_difficulty, diff_attractiveness,
    choice, chosenopt, chosen
  )

# Filter and transform
exp3_prepared <- exp3_restructured %>%
  filter(attn_check == 1) %>%
  select(-attn_check) %>%
  drop_na() %>%
  mutate(
    choice = factor(choice, levels = c("EsyB", "EsyI", "HrdB", "HrdI")),
    chosenopt = factor(chosenopt, levels = c("Option A", "Option B")),
    difficulty = factor(difficulty, levels = c("Easy", "Hard")),
    attractiveness = factor(attractiveness, levels = c("Boring", "Interesting")),
    diff_difficulty = factor(diff_difficulty, levels = c("Less Difficult", "Same Difficulty", "More Difficult")),
    diff_attractiveness = factor(diff_attractiveness, levels = c("Less Attractive", "Same Attractiveness", "More Attractive")),
    across(c(id, gender, pref_atts, pref_tasks, condition, alt), as.factor),
    across(c(which_now, which_later), as.factor),
    across(!where(is.factor), as.numeric),
    log_t = if_else(delay > 0, log(delay), delay)
  )

saveRDS(exp3_prepared, "./data/prepared/exp3.Rds")
