library(tidyverse)
library(openxlsx)

criteriaf <- "data/criteria.csv"

criteria <- read_csv(criteriaf)

criteria <- set_names(criteria$label, criteria$name)

blank <- "<span style='display:none'>foo</span>"

intro <- 
  str_c(
    "THANK YOU FOR PARTICIPATING IN THE CRITERIA COMPARISON EXERCISE.",
    "THE FOLLOWING SURVEY HAS BEEN DESIGNED TO HELP YOU RECORD YOUR OPINION.",
    sep = " "
  )

survey <- 
  cross_df(names(criteria) %>% list(criteria1 = ., criteria2 = .)) %>% 
  filter((row_number()-1) %>% {. %/% length(criteria) > . %% length(criteria)}) %>% 
  transmute(
    criteria = 
      map2(criteria1, criteria2,
           ~tibble(
             type = c("note", "select_one priority", "note", "select_one importance"),
             name = str_c(c("note", "prio", "blank", "imp"), "_", ..1, "_x_", ..2),
             label = c(str_glue("__Compare__ {criteria[..1]}\n__with__ {criteria[..2]}"),
                       blank, blank, blank),
             appearance = c("w1", "w1 likert", "w2", "w2 horizontal-compact"),
             relevant = c(NA, NA,
                          str_c("${prio_", ..1, "_x_", ..2, "} = '", c('', '0'), "'",
                                collapse = " or "),
                          str_c("${prio_", ..1, "_x_", ..2, "} = '", c('1', '-1'), "'",
                                collapse = " or ")),
             required = c(NA, TRUE, NA, TRUE)))) %>% 
  unnest()

survey <- 
  bind_rows(
    tribble(
      ~type,         ~name,        ~label,           ~appearance,
      "note",        "intro",      intro,            NA,
      "begin_group", "criteria",   "Criteria",       "w4",
      "note",        "compare",    blank,            "w1",
      "note",        "priority",   "__Priority__",   "w1",
      "note",        "importance", "__Importance__", "w2"
    ),
    survey,
    c("type" = "end_group")
  )

choices <- 
  tribble(
    ~list_name,   ~name, ~label,
    "priority",   1,     "1st",
    "priority",   0,     "equal",
    "priority",  -1,     "2nd",
    "importance", 3,     "moderate",
    "importance", 5,     "strong",
    "importance", 7,     "very strong",
    "importance", 9,     "extreme"
  )

settings <- tibble(style = "theme-grid")

xlsform <- createWorkbook()
addWorksheet(xlsform, "survey"); writeData(xlsform, "survey", survey)
addWorksheet(xlsform, "choices"); writeData(xlsform, "choices", choices)
addWorksheet(xlsform, "settings"); writeData(xlsform, "settings", settings)

saveWorkbook(xlsform, "out/form.xlsx", overwrite = TRUE)
