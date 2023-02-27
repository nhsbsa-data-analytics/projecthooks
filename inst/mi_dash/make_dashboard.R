# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(tidyfilter)
library(glue)
library(snakecase)
library(skimr)
library(fs)
library(rlang)
library(mi.r.utils)


open <- TRUE
overwrite <- TRUE

# Create backup folders ---------------------------------------------------
backup_dir <- dir_create(
  file.path("backup", str_replace_all(str_replace(now(), " ", "_"), ":", "."))
)
ui_backup_dir <- dir_create(file.path(backup_dir, "ui"))
pages_backup_dir <- dir_create(file.path(ui_backup_dir, "pages"))
server_backup_dir <- dir_create(file.path(backup_dir, "server"))
global_backup_dir <- dir_create(file.path(backup_dir, "global"))

# Regexes -----------------------------------------------------------------
regexes <- list(
  pid = readLines(
    system.file("extdata", "pid_regex.txt", package = "tidyfilter")
  ),
  profanity = readLines(
    system.file("extdata", "profane_words_basic.txt", package = "tidyfilter")
  ),
  final_brackets = "(\\(.*)?\\K\\(.*((\\.{3}|\\s\\w+\\)))",
  dot.dot.dot = "\\.{2,}",
  reasons_for_rating = ".*reason((\\(s\\))|s)? for your rating.*",
  other_cues = "^(other|please type in)",
  option_other = " option other",
  option_col = "_option_"
)

# Transformations ---------------------------------------------------------
transforms <- list(
  date_first_of_month = function(x) {
    floor_date(dmy(x), "month")
  },
  integer = function(x) {
    as.integer(gsub("[^[:digit:]]", "", x))
  },
  tidy_levels = function(x) {
    lvls <- trimws(gsub(regexes$final_brackets, "", x, perl = TRUE))
    lvls <- if_else(startsWith(lvls, "Prefer") | lvls == "", "Undisclosed", lvls)
    replace_na(lvls, "Undisclosed")
    # as.factor(x)
  },
  tidy_levels_keep_blanks = function(x) {
    lvls <- trimws(gsub(regexes$final_brackets, "", x, perl = TRUE))
    lvls <- if_else(startsWith(lvls, "Prefer"), "Undisclosed", lvls)
    replace_na(lvls, "Undisclosed")
    # as.factor(x)
  }
)

trans <- function(x, y) {
  fn_name <- data_meta %>%
    filter(rename_to == y) %>%
    pull(transform_to)
  transforms[[fn_name]](x)
}

# Read in metadata --------------------------------------------------------
app_meta <- read.csv("data/app_meta.csv") %>%
  as_tibble()

data_meta <- read_csv(
  "data/data_meta.csv",
  col_types = cols(use = col_logical(), .default = col_character())
) %>%
  as_tibble()

# Process metadata --------------------------------------------------------
ignore_top_rows <- app_meta %>% pull_value("ignore_top_rows")
app_type <- app_meta %>% pull_value("app_type")
app_title <- app_meta %>% pull_value("app_title")
main_title <- app_meta %>% pull_value("main_title")
subtitle <- app_meta %>% pull_value("subtitle")

data_meta <- data_meta %>%
  filter(use) %>%
  mutate(datafield = gsub(":", "\\.", datafield)) %>%
  mutate(rename_to = if_else(is.na(rename_to), datafield, rename_to)) %>%
  mutate(
    label_set = !is.na(label),
    label = if_else(
      label_set,
      label,
      {
        lbl <- gsub(regexes$final_brackets, "", question_text, perl = TRUE)
        lbl <- gsub(regexes$dot.dot.dot, "", lbl, perl = TRUE)
        lbl <- gsub(
          regexes$reasons_for_rating, "Reasons for rating", lbl,
          perl = TRUE
        )
        lbl <- trimws(lbl)
        if_else(
          str_detect(tolower(lbl), regexes$other_cues),
          gsub(
            regexes$option_other,
            "",
            to_sentence_case(paste("Other", lag(rename_to)))
          ),
          lbl
        )
      }
    ),
    transform_to = if_else(
      output_func %in% c(
        "stacked_vertical",
        "stacked_horizontal",
        "pie",
        "horizontal_bar",
        "coding_horizontal_bar"
      ),
      if_else(
        grepl(regexes$option_col, rename_to),
        "tidy_levels_keep_blanks",
        "tidy_levels"
      ),
      transform_to
    )
  )

needs_transform <- data_meta %>%
  filter(nchar(transform_to) > 0) %>%
  select(rename_to, transform_to)

# Read and process data ---------------------------------------------------
data <- read.csv("data/csat.csv") %>%
  as_tibble() %>%
  slice(-(1:ignore_top_rows)) %>%
  select(all_of(data_meta$datafield)) %>%
  rename_with(~ data_meta$rename_to) %>%
  mutate(across(needs_transform$rename_to, ~ trans(.x, cur_column()))) %>%
  ### Put any custom data processing below

  ### Put any custom data processing above
  encode_char_cols("latin1") %>%
  filter_text(
    regexes[c("pid", "profanity")] %>% unlist(use.names = FALSE),
    "#",
    ends_with("comment")
  )

data %>% saveRDS("data/data.rds")

# Read and process targets ------------------------------------------------
targets <- "data/targets.csv"

if (file.exists(targets)) {
  read_csv(
    targets,
    col_types = cols(Month = col_date("%d/%m/%Y"))
  ) %>%
    rename(
      first_of_month = 1
    ) %>%
    saveRDS("data/targets.rds")
}

# Build app ---------------------------------------------------------------

## Create response levels -------------------------------------------------
levels_meta <- data_meta %>%
  filter(startsWith(transform_to, "tidy_levels"))

try(
  data %>%
    select(levels_meta %>% pull(rename_to)) %>%
    add_levels(levels_meta, global_backup_dir, overwrite = overwrite, open = open)
)

## Create aspect radio choices --------------------------------------------
choices_meta <- data_meta %>%
  filter(arg7 == TRUE) %>%
  select(rename_to, output_name)

try(
  add_choices(choices_meta, global_backup_dir, overwrite = overwrite, open = open)
)

## Create app.R -----------------------------------------------------------
try(create_app_r(app_type, app_title, backup_dir, overwrite = overwrite, open = open))

## Create ui.R ------------------------------------------------------------
try(create_ui_r(app_type, main_title, subtitle, ui_backup_dir, overwrite = overwrite, open = open))

## Create pages -----------------------------------------------------------
pages <- app_meta %>%
  filter(startsWith(property, "page_")) %>%
  separate(property, into = c("page", "number"), remove = FALSE, convert = TRUE) %>%
  arrange(number) %>%
  select(-page, -number)

try({
  add_pages(
    pages %>% pull(value),
    backup_dirs = c(pages_backup_dir, server_backup_dir),
    overwrite = overwrite, open = open
  )

  ## Create outputs ---------------------------------------------------------
  output_meta <- unique(
    data_meta %>%
      select(page, output_name, output_type, output_func, starts_with("arg"))
  ) %>%
    filter(nchar(page) > 0) %>%
    # Do I need to do this? What if an arg is actually ""?
    # mutate(across(everything(), ~ na_if(.,""))) %>%
    mutate(
      page = glue("{to_snake_case(page)}_outputs.R"),
      render_func = glue(
        "render{toupper(substr(output_type, 1, 1))}",
        "{substr(output_type, 2, nchar(output_type))}"
      ),
      output_type = NULL,
      arg3 = case_when(
        output_func == "comment_table" &
          arg3 == TRUE ~ glue("\"{output_name}_group\""),
        TRUE ~ arg3
      ),
      arg7 = case_when(
        output_func == "stacked_vertical" &
          arg7 == TRUE ~ glue("\"{output_name}_radio\""),
        TRUE ~ arg7
      )
    ) %>%
    unite(args, starts_with("arg"), sep = ", ")

  pmap(output_meta, add_output)

  ## Create ui elements------------------------------------------------------
  # TODO: can element_num be removed?
  ui_meta <- unique(
    data_meta %>%
      select(page, section, label, output_type, output_name, arg3, arg7)
  ) %>%
    filter(nchar(page) > 0) %>%
    mutate(
      ui_func = case_when(
        tolower(output_type) == "custom" ~ glue("CUSTOM_OUTPUT"),
        TRUE ~ glue("{output_type}Output")
      ),
      output_type = NULL
    ) %>%
    # TODO: Not happy handling this way; will be better once using modules with golem
    rename(use_comment_group = arg3, use_aspect_radio = arg7) %>%
    mutate(
      use_comment_group = case_when(
        is.na(use_comment_group) | use_comment_group != TRUE ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    mutate(
      use_aspect_radio = case_when(
        is.na(use_aspect_radio) | use_aspect_radio != TRUE ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    group_by(page, section) %>%
    mutate(element_num = sequence(n())) %>%
    ungroup() %>%
    group_by(page, element_num) %>%
    mutate(
      section_num = sequence((n()))
    ) %>%
    ungroup() %>%
    select(-element_num)

  pmap(ui_meta, add_ui)
})

# Data summary ------------------------------------------------------------
data_summary <- summarise_data(data, data_meta)
