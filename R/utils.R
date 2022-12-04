#' Title
#'
#' @param files
#' @param backup_dirs
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
check_overwrite <- function(files, backup_dirs, overwrite) {
  .check_overwrite <- function(.file, .backup_dir, overwrite) {
    if (file.exists(.file)) {
      if (overwrite) {
        file_copy <- file.copy(.file, .backup_dir, overwrite)

        warning(
          glue::glue(
            "Overwriting existing file: {.file}. Previous file saved to backup \\
            folder {.backup_dir}."
          ),
          call. = FALSE
        )
      } else {
        stop(glue::glue("File {.file} already exists...aborting"))
      }
    }
  }

  mapply(.check_overwrite, files, backup_dirs, MoreArgs = list(overwrite = overwrite))
}


#' Title
#'
#' @param app_title
#' @param backup_dir
#' @param overwrite
#' @param open
#'
#' @return
#' @export
#'
#' @examples
create_app_r <- function(app_title, backup_dir = NULL,
                         overwrite = FALSE, open = FALSE) {
  app_r <- "app.R"
  check_overwrite(app_r, backup_dir, overwrite)

  app_r_lines <- readLines("templates/app_template.txt")
  app_r_lines <- glue::glue_collapse(app_r_lines, sep = "\n")

  writeLines(app_r_lines, app_r)

  if (open) rstudioapi::navigateToFile(app_r)
}


#' Title
#'
#' @param main_title
#' @param subtitle
#' @param backup_dir
#' @param overwrite
#' @param open
#'
#' @return
#' @export
#'
#' @examples
create_ui_r <- function(main_title, subtitle, backup_dir = NULL,
                        overwrite = FALSE, open = FALSE) {
  ui_r <- "ui/ui.R"

  check_overwrite(ui_r, backup_dir, overwrite)

  ui_r_lines <- readLines("templates/ui/ui_template.txt")
  ui_r_lines <- glue::glue_collapse(ui_r_lines, sep = "\n")

  writeLines(ui_r_lines, ui_r)

  if (open) rstudioapi::navigateToFile(ui_r)
}


#' Title
#'
#' @param lines
#' @param to_insert
#' @param insert_at
#'
#' @return
#' @export
#'
#' @examples
insert_lines_at <- function(lines, to_insert, insert_at) {
  # edge cases: inserting at the end or the beginning (or past the end)
  if (is.na(insert_at) || insert_at >= length(lines)) {
    return(c(lines, to_insert))
  } else if (insert_at == 0) {
    return(c(to_insert, lines))
  }

  pre <- lines[1:insert_at]
  post <- lines[(insert_at + 1):length(lines)]
  return(c(pre, to_insert, post))
}


#' Title
#'
#' @param .file
#' @param .lines
#' @param .match
#' @param .nth_match
#' @param open
#'
#' @return
#' @export
#'
#' @examples
file_insert_lines <- function(.file, .lines, .match = NA,
                              .nth_match = 1, open = FALSE) {
  lines <- readLines(.file)
  insert_at <- which(lines == .match)[.nth_match] - 1
  lines <- insert_lines_at(lines, .lines, insert_at)

  writeLines(lines, .file)

  if (open) rstudioapi::navigateToFile(.file, insert_at + 1)
}


#' Title
#'
#' @param lines
#' @param to_remove
#'
#' @return
#' @export
#'
#' @examples
remove_line <- function(lines, to_remove) {
  remove_at <- match(to_remove, lines)
  if (!is.na(remove_at)) {
    pre <- lines[1:remove_at - 1]
    post <- if (remove_at < length(lines)) lines[(remove_at + 1):length(lines)] else c()

    return(c(pre, post))
  }

  return(invisible(NULL))
}


#' Title
#'
#' @param .file
#' @param .match
#' @param open
#'
#' @return
#' @export
#'
#' @examples
file_remove_line <- function(.file, .match = NA, open = FALSE) {
  lines <- readLines(.file)
  lines <- remove_line(lines, .match)
  if (!is.null(lines)) writeLines(lines, .file)

  if (open) rstudioapi::navigateToFile(.file)
}


#' Title
#'
#' @param page_name
#' @param page_menu_name
#' @param visibility
#' @param backup_dirs
#' @param overwrite
#' @param open
#'
#' @return
#' @export
#'
#' @examples
add_page <- function(page_name, page_menu_name = page_name, visibility = "hidden",
                     backup_dirs = NULL, overwrite = FALSE, open = FALSE) {
  page_file_name <- snakecase::to_snake_case(page_name)
  page_r <- glue::glue("ui/pages/{page_file_name}.R")
  server_r <- glue::glue("server/{page_file_name}_outputs.R")

  check_overwrite(
    c(page_r, server_r), backup_dirs, overwrite
  )

  page_r_temp <- readLines("templates/ui/ui_page_template.txt")
  page_r_temp <- glue::glue(glue::glue_collapse(page_r_temp, sep = "\n"))

  file.create(page_r)
  writeLines(page_r_temp, page_r)
  if (open) rstudioapi::navigateToFile(page_r)

  file.create(server_r)
  if (open) rstudioapi::navigateToFile(server_r)

  app_r <- "app.R"
  ui_r <- "ui/ui.R"

  app_lines <- readLines(app_r)
  ui_lines <- readLines(ui_r)

  target <- glue::glue('  source("./server/{page_file_name}_outputs.R", local = TRUE)')
  if (is.na(match(target, app_lines))) {
    file_insert_lines(app_r, target, "  ### Page outputs above", open = open)
  }

  target <- glue::glue('source("./ui/pages/{page_file_name}.R", local = TRUE)')
  if (is.na(match(target, ui_lines))) {
    file_insert_lines(ui_r, target, "### Source pages above", open = open)
  }

  target <- glue::glue("  {page_file_name}_page,")
  if (is.na(match(target, ui_lines))) {
    file_insert_lines(ui_r, target, "  ### Add pages above", open = open)
  }

  target <- glue::glue(
    "      a(class = 'navigation homeitem item', '{page_menu_name}', ",
    "`data-value` = '{page_file_name}'),"
  )
  if (is.na(match(target, ui_lines))) {
    file_insert_lines(ui_r, target, "    ### Add page links above", open = open)
  }
}


#' Title
#'
#' @param pages
#' @param page_menu_names
#' @param backup_dirs
#' @param overwrite
#' @param open
#'
#' @return
#' @export
#'
#' @examples
add_pages <- function(pages, page_menu_names = pages, backup_dirs = NULL,
                      overwrite = FALSE, open = FALSE) {
  add_page(
    pages[1],
    page_menu_names[1],
    visibility = "visible",
    backup_dirs = backup_dirs, overwrite = overwrite, open = open
  )
  purrr::walk2(
    pages[-1],
    page_menu_names[-1],
    add_page,
    visibility = "hidden",
    backup_dirs = backup_dirs, overwrite = overwrite, open = open
  )
}


#' Title
#'
#' @param page_name
#' @param page_menu_name
#' @param backup_dirs
#' @param open
#'
#' @return
#' @export
#'
#' @examples
remove_page <- function(page_name, page_menu_name = page_name,
                        backup_dirs = NULL, open = FALSE) {
  page_file_name <- snakecase::to_snake_case(page_name)
  page_r <- glue::glue("ui/pages/{page_file_name}.R")
  server_r <- glue::glue("server/{page_file_name}_outputs.R")

  ui_exists <- file.exists(page_r)
  server_exists <- file.exists(server_r)

  if (is.null(backup_dirs)) {
    backup_dir <- dir_create(
      file.path("backup", str_replace_all(str_replace(now(), " ", "_"), ":", "."))
    )
    ui_backup_dir <- dir_create(file.path(backup_dir, "ui"))
    pages_backup_dir <- dir_create(file.path(ui_backup_dir, "pages"))
    server_backup_dir <- dir_create(file.path(backup_dir, "server"))

    backup_dirs <- c(pages_backup_dir, server_backup_dir)
  }

  if (ui_exists) {
    file.copy(page_r, backup_dirs[1], overwrite = TRUE)
  }
  if (server_exists) {
    file.copy(server_r, backup_dirs[2], overwrite = TRUE)
  }

  unlink(page_r)
  unlink(server_r)

  if (any(c(ui_exists, server_exists))) {
    both <- all(c(ui_exists, server_exists))
    maybe <- if (both) {
      list(" & ", "s", "They have", "respectively")
    } else {
      list("", "", "It has", "")
    }
    names(maybe) <- c("and", "s", "they_have", "respectively")
    files <- glue::glue(
      "{if (ui_exists) page_r else ''}{maybe$and}{if (server_exists)
      server_r else ''}"
    )
    backups <- glue::glue(
      "{if (ui_exists) pages_backup_dir else ''}{maybe$and}{if (server_exists)
      server_backup_dir else ''}{maybe$respectively}"
    )
    message(
      glue::glue(
        "File{maybe$s} deleted: {files}.
        {maybe$they_have} been saved to the backup folder{maybe$s} {backups}."
      )
    )
  }

  app_r <- "app.R"
  ui_r <- "ui/ui.R"

  target <- glue::glue('  source("./server/{page_file_name}_outputs.R", local = TRUE)')
  file_remove_line(app_r, target, open = open)

  target <- glue::glue('source("./ui/pages/{page_file_name}.R", local = TRUE)')
  file_remove_line(ui_r, target, open = open)

  target <- glue::glue("  {page_file_name}_page,")
  file_remove_line(ui_r, target, open = open)

  target <- glue::glue(
    "      a(class = 'navigation homeitem item', '{page_menu_name}', ",
    "`data-value` = '{page_file_name}'),"
  )
  file_remove_line(ui_r, target, open = open)
}


#' Title
#'
#' @param pages
#' @param page_menu_names
#' @param backup_dirs
#' @param open
#'
#' @return
#' @export
#'
#' @examples
remove_pages <- function(pages, page_menu_names = pages,
                         backup_dirs = NULL, open = FALSE) {
  purrr::walk2(
    pages,
    page_menu_names,
    remove_page,
    backup_dirs = backup_dirs,
    open = open
  )
}


#' Title
#'
#' @param output_name
#' @param render_func
#' @param output_func
#' @param args
#'
#' @return
#' @export
#'
#' @examples
create_output <- function(output_name, render_func, output_func, args) {
  if (tolower(output_func) == "custom") {
    output_func <- "{CUSTOM_FUNC}"
    args <- ""
  } else {
    args_names <- names(formals(output_func))
    args_split <- stringr::str_split_fixed(args, ", ", n = length(args_names) + 1)
    length(args_split) <- length(args_names)
    args_to_check <- paste(args_names, "=", args_split)
    args_to_combine <- args_to_check[!endsWith(args_to_check, "NA")]
    args <- paste(args_to_combine, collapse = ", ")
  }

  output_temp <- readLines("templates/server/output_template.txt")
  if (output_func == "{CUSTOM_FUNC}") {
    comments <- rep("# ", length(output_temp))
    output_temp <- paste0(comments, output_temp)
  }

  glue::glue(glue::glue_collapse(output_temp, sep = "\n"), "\n\n")
}


#' Title
#'
#' @param page
#' @param output_name
#' @param render_func
#' @param output_func
#' @param args
#'
#' @return
#' @export
#'
#' @examples
add_output <- function(page, output_name, render_func, output_func, args) {
  output <- create_output(output_name, render_func, output_func, args)

  file_insert_lines(file.path("server/", page), output, .match = NA)
}

# TODO: refactor and rename like create/add_output
#' Title
#'
#' @param page
#' @param label
#' @param output_name
#' @param ui_func
#' @param section_num
#' @param use_comment_group
#' @param use_aspect_radio
#'
#' @return
#' @export
#'
#' @examples
add_ui_element <- function(page, label, output_name, ui_func, section_num,
                           use_comment_group, use_aspect_radio) {
  if (use_comment_group) {
    element_temp <- readLines("templates/ui/ui_comment_group_template.txt")
    group_select_id <- glue::glue("{output_name}_group")
  } else if (use_aspect_radio) {
    element_temp <- readLines("templates/ui/ui_stacked_vertical_radio_template.txt")
    radio_select_id <- glue::glue("{output_name}_radio")
    radio_select_choices <- glue::glue("{output_name}_choices")
  } else {
    element_temp <- readLines("templates/ui/ui_element_template.txt")
  }

  if (ui_func == "CUSTOM_OUTPUT") {
    len_et <- length(element_temp)
    comments <- rep("# ", len_et - 1)
    element_temp <- c(
      paste0(comments, element_temp[1:(len_et - 1)]),
      element_temp[len_et]
    )
  }

  new_element <- glue::glue(glue::glue_collapse(element_temp, sep = "\n"), .trim = FALSE)
  target <- "  ),"

  file_insert_lines(page, new_element, target, section_num)
}

# TODO: refactor and rename like create/add_output
#' Title
#'
#' @param page
#' @param section
#' @param label
#' @param output_name
#' @param ui_func
#' @param section_num
#' @param use_comment_group
#' @param use_aspect_radio
#'
#' @return
#' @export
#'
#' @examples
add_ui <- function(page, section, label, output_name, ui_func, section_num,
                   use_comment_group, use_aspect_radio) {
  # section may exist already
  page_file_name <- snakecase::to_snake_case(page)
  page_r <- glue::glue("ui/pages/{page_file_name}.R")
  lines <- readLines(glue::glue("ui/pages/{page_file_name}.R"))
  target <- glue::glue("    \"{section}\",")

  if (is.na(match(target, lines))) {
    # section does not exist, so add
    section_temp <- readLines("templates/ui/ui_section_template.txt")
    new_section <- glue::glue(glue::glue_collapse(section_temp, sep = "\n"), .trim = FALSE)
    target <- glue::glue("  htmlOutput(\"{page_file_name}trigger\")")

    file_insert_lines(page_r, new_section, target)
    lines <- readLines(glue::glue("ui/pages/{page_file_name}.R"))
  }

  add_ui_element(
    page_r, label, output_name, ui_func, section_num, use_comment_group, use_aspect_radio
  )
}


#' Title
#'
#' @param .data
#' @param .property
#'
#' @return
#' @export
#'
#' @examples
pull_value <- function(.data, .property) {
  .data %>%
    dplyr::filter(property == .property) %>%
    dplyr::pull(value)
}


# The sparkline histograms don't work in English locale, so temporarily switch to
# Chinese locale. Normally, the sleep would not be needed, but I find a small pause
# is required or by the time the summary prints, the locale is set back!
# Also, when factor columns have missing values a warning is generated, which we don't
# need to know.
#' Title
#'
#' @param .data
#' @param .data_meta
#'
#' @return
#' @export
#'
#' @examples
summarise_data <- function(.data, .data_meta) {
  withr::with_locale(c("LC_CTYPE" = "Chinese"), {
    suppress_warnings(
      {
        print(skimr::skim(.data))
        skim_data <- skimr::skim_with(numeric = sfl(hist = NULL))(.data)
      },
      endsWith,
      "value(s) of \"\" that have been converted to \"empty\"."
    )
    Sys.sleep(0.01)
  })

  cat("", sep = "\n")

  level_cols <- .data_meta %>%
    dplyr::filter(startsWith(transform_to, "tidy_levels"))

  print(summary(.data %>% dplyr::select(dplyr::all_of(level_cols$rename_to))))
  skim_data
}


#' Title
#'
#' @param data
#' @param levels_meta
#'
#' @return
#' @export
#'
#' @examples
create_levels <- function(data, levels_meta) {
  level_names <- levels_meta %>%
    dplyr::select(output_name) %>%
    dplyr::group_by(output_name) %>%
    dplyr::add_count()

  multi_col_level_names <- level_names %>%
    dplyr::filter(n > 1) %>%
    dplyr::pull(output_name) %>%
    unique()

  single_col_level_names <- setdiff(level_names$output_name, multi_col_level_names)

  levels <- lapply(data, unique) %>%
    lapply(setdiff, "")

  levels <- purrr::map(levels, ~ paste0("  \"", .x, "\"", " = \"", .x, "\""))
  levels <- purrr::imap(
    levels,
    ~ `if`(
      length(.x) == 1,
      stringr::str_replace(.x, "(?<=([\"']))(?:(?=(\\\\?))\\2.)*?(?=\\1)", .y),
      .x
    )
  )
  levels <- purrr::imap(
    levels,
    ~ `if`(
      all(endsWith(.x, "\"\"")),
      {
        warning(
          glue::glue("Option text inferred in levels for {.y}. Please check before using."),
          call. = FALSE
        )
        stringr::str_replace(
          .x,
          "(?<=([\"']))(?=\\1)",
          snakecase::to_sentence_case(stringr::str_split(.y, "option_")[[1]][2])
        )
      },
      .x
    )
  )

  multi_col_levels <- purrr::map(
    multi_col_level_names,
    function(prefix) {
      levels %>%
        purrr::keep(startsWith(names(.), prefix)) %>%
        purrr::set_names(stringr::str_replace(names(.), names(.), prefix))
    }
  ) %>% rlang::squash()

  multi_col_levels <- purrr::map(
    purrr::set_names(multi_col_level_names),
    ~ unique(unlist(multi_col_levels[names(multi_col_levels) == .], use.names = FALSE))
  )

  levels <- c(levels[single_col_level_names], multi_col_levels)
  levels <- purrr::map(levels, ~ paste0(.x, collapse = ",\n"))
  levels <- purrr::imap(levels, ~ paste0(.y, "_responses <- c(\n", .x, "\n)"))

  paste_lvls <- function(out, input) paste(out, input, sep = "\n\n")

  levels %>% purrr::reduce(paste_lvls)
}


#' Title
#'
#' @param data
#' @param levels_meta
#' @param backup_dir
#' @param overwrite
#' @param open
#'
#' @return
#' @export
#'
#' @examples
add_levels <- function(data, levels_meta, backup_dir = NULL,
                       overwrite = FALSE, open = FALSE) {
  levels <- create_levels(data, levels_meta)

  levels_r <- glue::glue("global/levels.R")

  check_overwrite(levels_r, backup_dir, overwrite)

  levels_r_temp <- readLines("templates/global/levels_template.txt")
  levels_r_temp <- glue::glue(glue::glue_collapse(levels_r_temp, sep = "\n"))

  file.create(levels_r)
  writeLines(levels_r_temp, levels_r)
  if (open) rstudioapi::navigateToFile(levels_r)
}


#' Title
#'
#' @param choices_meta
#'
#' @return
#' @export
#'
#' @examples
create_choices <- function(choices_meta) {
  choices <- setNames(choices_meta$rename_to, choices_meta$output_name)
  choices <- purrr::map(choices, ~ paste0("  \"", .x, "\"", " = \"", .x, "\""))
  choices <- purrr::map(
    purrr::set_names(unique(names(choices))),
    ~ unique(unlist(choices[names(choices) == .], use.names = FALSE))
  )
  choices <- purrr::map(choices, ~ paste0(.x, collapse = ",\n"))
  choices <- purrr::imap(choices, ~ paste0(.y, "_choices <- c(\n", .x, "\n)"))

  paste_lvls <- function(out, input) paste(out, input, sep = "\n\n")

  choices %>% purrr::reduce(paste_lvls)
}


#' Title
#'
#' @param choices_meta
#' @param backup_dir
#' @param overwrite
#' @param open
#'
#' @return
#' @export
#'
#' @examples
add_choices <- function(choices_meta, backup_dir = NULL,
                        overwrite = FALSE, open = FALSE) {
  choices <- create_choices(choices_meta)

  choices_r <- glue::glue("global/choices.R")

  check_overwrite(choices_r, backup_dir, overwrite)

  warning(
    glue::glue("Choices created will have human unfriendly names. Please edit before using."),
    call. = FALSE
  )

  file.create(choices_r)
  writeLines(choices, choices_r)
  if (open) rstudioapi::navigateToFile(choices_r)
}
