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
library(tools)


open <- FALSE
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
  },
  tidy_levels_keep_blanks = function(x) {
    lvls <- trimws(gsub(regexes$final_brackets, "", x, perl = TRUE))
    lvls <- if_else(startsWith(lvls, "Prefer"), "Undisclosed", lvls)
    replace_na(lvls, "Undisclosed")
  }
)

trans <- function(x, y) {
  fn_name <- data_meta %>%
    filter(rename_to == y) %>%
    pull(transform_to)
  transforms[[fn_name]](x)
}

# Read in metadata --------------------------------------------------------
app_meta <- read.csv("data-raw/app_meta.csv") %>%
  as_tibble()

data_meta <- read_csv(
  "data-raw/data_meta.csv",
  col_types = cols(
    use = col_logical(),
    arg4 = col_logical(),
    .default = col_character()
  )
) %>%
  as_tibble()

# Process metadata --------------------------------------------------------
app_type <- app_meta %>% pull_value("app_type")
data_file <- app_meta %>% pull_value("data_file")
data_prefix <- file_path_sans_ext(data_file)
app_title <- app_meta %>% pull_value("app_title")
main_title <- app_meta %>% pull_value("main_title")
subtitle <- app_meta %>% pull_value("subtitle")
month_col <- app_meta %>% pull_value("month_col")
month_col <- glue("{data_prefix}_{month_col}")
region_col <- app_meta %>% pull_value("region_col")
region_col <- glue("{data_prefix}_{region_col}")
survey_doc <- app_meta %>% pull_value("survey_doc")
ignore_top_rows <- app_meta %>% pull_value("ignore_top_rows")
accordion_menu <- app_meta %>% pull_value("accordion_menu")
accordion_menu <- accordion_menu %>% as.logical()
page_1 <- app_meta %>% pull_value("page_1")


data_meta <- data_meta %>%
  filter(use) %>%
  mutate(datafield = gsub(":", "\\.", datafield)) %>%
  mutate(rename_to = if_else(is.na(rename_to), datafield, rename_to)) %>%
  mutate(
    rename_to = glue("{data_prefix}_{rename_to}"),
    output_name = if_else(
      !is.na(output_name),
      glue("{data_prefix}_{output_name}"),
      output_name
    ),
    arg1 = if_else(
      !is.na(output_name) & is.na(arg1),
      glue("filtered_{data_prefix}_data[[x]]"),
      arg1
    ),
    arg2 = if_else(
      !is.na(output_name) & is.na(arg2),
      glue("\"{output_name}\""),
      arg2
    ),
    arg3 = if_else(
      output_func %in% c(
        "line_chart", "group_table", "horizontal_bar", "stacked_vertical"
      ),
      glue("\"{month_col}\""),
      arg3
    ),
    arg4 = case_when(
      output_func == "group_table" & arg4 ~ glue("input${output_name}_group"),
      output_func == "stacked_vertical" & arg4 ~ glue("input${output_name}_radio"),
    ),
    arg5 = if_else(
      output_func %in% c("line_chart", "stacked_horizontal"),
      "last_valid_month_range_selection()",
      arg5
    ),
    arg6 = if_else(
      output_func %in% c("line_chart", "group_table"),
      glue("\"{data_prefix}\""),
      arg6
    )
  ) %>%
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
data <- read.csv(glue("data-raw/{data_file}")) %>%
  filter(ID.completed == "completed") %>%
  as_tibble() %>%
  {
    if (ignore_top_rows == 0L) {
      .
    } else {
      slice(., -(1:ignore_top_rows))
    }
  } %>%
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

data %>% saveRDS(glue("data/{data_prefix}.rds"))

# Read and process targets ------------------------------------------------
targets <- "data-raw/targets.csv"

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
  filter(startsWith(transform_to, "tidy_levels")) %>%
  mutate(arg7 = as.logical(arg7))

if (nrow(levels_meta) > 0) {
  try(
    add_levels(
      app_type,
      data_prefix,
      data %>% select(levels_meta %>% pull(rename_to)),
      levels_meta, global_backup_dir,
      overwrite = overwrite, open = open
    )
  )
}

## Create aspect radio choices --------------------------------------------
choices_meta <- data_meta %>%
  filter(
    output_func == "stacked_vertical" &
      nchar(arg4) > 0
  ) %>%
  select(rename_to, output_name)

if (nrow(choices_meta) > 0) {
  try(
    add_choices(
      choices_meta, global_backup_dir,
      overwrite = overwrite, open = open
    )
  )
}

## Create app.R -----------------------------------------------------------
try(
  create_app_r(
    app_type, app_title, data_prefix, month_col, region_col,
    backup_dir,
    overwrite = overwrite, open = open
  )
)

## Create ui.R ------------------------------------------------------------
try(
  create_ui_r(
    app_type, data_prefix, month_col, region_col,
    main_title, subtitle, ui_backup_dir,
    overwrite = overwrite, open = open
  )
)

## Create pages -----------------------------------------------------------
pages <- app_meta %>%
  filter(startsWith(property, "page_")) %>%
  separate(property, into = c("page", "number"), remove = FALSE, convert = TRUE) %>%
  arrange(number) %>%
  select(-page, -number)

try({
  add_pages(
    app_type,
    data_prefix,
    accordion_menu,
    pages %>% pull(value),
    backup_dirs = c(pages_backup_dir, server_backup_dir),
    overwrite = overwrite, open = open
  )
})

## Create outputs ---------------------------------------------------------
try({
  output_meta <- unique(
    data_meta %>%
      select(page, output_name, output_type, output_func, starts_with("arg"))
  ) %>%
    filter(nchar(page) > 0) %>%
    mutate(
      page = glue("{data_prefix}_{to_snake_case(page)}_outputs.R"),
      render_func = glue(
        "render{toupper(substr(output_type, 1, 1))}",
        "{substr(output_type, 2, nchar(output_type))}"
      ),
      output_type = NULL,
      arg6 = if_else(
        output_func %in% c(
          "stacked_vertical",
          "stacked_horizontal",
          "pie",
          "horizontal_bar"
        ) & is.na(arg6),
        glue("{output_name}_responses"),
        arg6
      )
    ) %>%
    unite(args, starts_with("arg"), sep = ", ") %>%
    mutate(app_type = app_type)

  pwalk(output_meta, add_output)
})

## Create ui elements------------------------------------------------------
try({
  ui_meta <- unique(
    data_meta %>%
      select(page, section, label, output_type, output_name, output_func, arg4)
  ) %>%
    filter(nchar(page) > 0) %>%
    mutate(
      page = glue("{data_prefix}_{(page)}"),
      ui_func = if_else(
        tolower(output_type) == "custom",
        glue("CUSTOM_OUTPUT"),
        glue("{output_type}Output")
      ),
      output_type = NULL,
      use_comment_group = case_when(
        output_func == "group_table" & nchar(arg4) ~ TRUE,
        TRUE ~ FALSE
      ),
      use_aspect_radio = case_when(
        output_func == "stacked_vertical" & nchar(arg4) ~ TRUE,
        TRUE ~ FALSE
      ),
      output_func = NULL,
      arg4 = NULL
    ) %>%
    group_by(page, section) %>%
    mutate(
      element_num = row_number(),
      section_num = if_else(element_num == 1, 1, 0),
      element_num = NULL
    ) %>%
    group_by(page) %>%
    mutate(section_num = cumsum(section_num)) %>%
    ungroup() %>%
    mutate(app_type = app_type)

  pwalk(ui_meta, add_ui)
})

## Create ui outputs-----------------------------------------------------
try({
  create_ui_outputs_r(
    app_type, data_prefix, page_1, survey_doc,
    server_backup_dir,
    overwrite = overwrite, open = open
  )
})

# Data summary ------------------------------------------------------------
data_summary <- summarise_data(data, data_meta)
