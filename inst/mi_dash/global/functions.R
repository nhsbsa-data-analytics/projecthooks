# Date range picker -------------------------------------------------------

controlLabel <- function(controlName, label) {
  if (!is.null(label)) {
    label %AND% tags$label(class = "control-label", `for` = controlName, label)
  }
}

# the datePickerDependency is taken from
# https://github.com/rstudio/shiny/blob/master/R/input-date.R
datePickerDependency <- htmltools::htmlDependency(
  "bootstrap-datepicker", "1.6.4", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/bootstrap-datepicker3.min.css",
  # Need to enable noConflict mode. See https://github.com/rstudio/shiny/issues/1346.
  head = "<script>
           (function() {
           var datepicker = $.fn.datepicker.noConflict();
           $.fn.bsDatepicker = datepicker;
           $.fn.bsDatepicker.defaults.autoclose = true;
           })();
         </script>"
)

### Date range picker - shows months only by default
dateRangeMonthsInput <- function(inputId, label = NULL, start = NULL, end = NULL,
                                 min = NULL, max = NULL, format = "yyyy-mm-dd",
                                 startview = "month", minviewmode = "months",
                                 weekstart = 0, language = "en", separator = " to ",
                                 width = NULL, init_hide = FALSE) {

  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(start, "Date")) start <- format(start, "%Y-%m-%d")
  if (inherits(end, "Date")) end <- format(end, "%Y-%m-%d")
  if (inherits(min, "Date")) min <- format(min, "%Y-%m-%d")
  if (inherits(max, "Date")) max <- format(max, "%Y-%m-%d")

  maybe_hide <- if (init_hide) {
    "display:none;"
  } else {
    ""
  }

  htmltools::attachDependencies(
    div(
      id = inputId,
      class = "shiny-date-range-input form-group shiny-input-container",
      style = if (!is.null(width)) {
        paste0("width: ", validateCssUnit(width), ";", maybe_hide)
      } else {
        maybe_hide
      },
      controlLabel(inputId, label),
      # input-daterange class is needed for dropdown behaviour
      div(
        class = "input-daterange input-group",
        tags$input(
          class = "input-sm form-control",
          type = "text",
          `data-date-language` = language,
          `data-date-weekstart` = weekstart,
          `data-date-format` = format,
          `data-date-start-view` = startview,
          `data-date-min-view-mode` = minviewmode, # added manually
          `data-min-date` = min,
          `data-max-date` = max,
          `data-initial-date` = start
        ),
        span(class = "input-group-addon", separator),
        tags$input(
          class = "input-sm form-control",
          type = "text",
          `data-date-language` = language,
          `data-date-weekstart` = weekstart,
          `data-date-format` = format,
          `data-date-start-view` = startview,
          `data-date-min-view-mode` = minviewmode, # added manually
          `data-min-date` = min,
          `data-max-date` = max,
          `data-initial-date` = end
        )
      )
    ),
    datePickerDependency
  )
}


# Use like x %AND% y. Return y if both x and y are non-null and non-NA, else null.
`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x)) {
    if (!is.null(y) && !is.na(y)) {
      return(y)
    }
  }
  return(NULL)
}


# Use like x %||% y. If x is null, return y. If x is not null, return x.
`%||%` <- function(x, y) if (is_null(x)) y else x


# Add leading + to non-negative numbers
with_plus <- function(x, ...) {
  # TODO: check usage, is ... used? If not, better is
  # if (x >= 0) glue("+{x}") else as.character(x)

  if (x >= 0) {
    sprintf(
      fmt = "+%s",
      format(x, ...)
    )
  } else {
    x
  }
}


# Set number of decimal places for x to k.
# e.g. decimal_places(3.1412, 2) -> 3.14
decimal_places <- function(x, k) trimws(format(round(x, k), nsmall = k))


# Set format of date to e.g. Jan 2021
date_Ymd_to_bY <- function(date_to_format) format(date_to_format, format = "%b %Y")


# Get number of months between 2 dates, considering month and year only.
# e.g. month_diff(as.Date("2022-02-28"), as.Date("2022-03-01")) -> 1
# e.g. month_diff(as.Date("2022-02-01"), as.Date("2022-02-02")) -> 0
# Can pass each month as separate arg, or together as a single list arg
month_diff <- function(d1, d2 = NULL) {
  if (is.null(d2)) {
    d2 <- d1[2]
    d1 <- d1[1]
  }

  abs(12 * (as.yearmon(as.Date(d1)) - as.yearmon(as.Date(d2))))
}


# Make colour ramp palette. Default start colour is NHS Dark Blue. To allow for
# avoiding having white as last colour, n_colours + 1 are created, with the last
# colour (white) discarded by default.
colour_ramp <- function(n_colours, first = "#003087", last = "#FFFFFF",
                        include_white = FALSE) {
  palette <- colorRampPalette(c(first, last))

  if (include_white) {
    c(palette(n_colours + 1))
  } else {
    c(palette(n_colours + 1)[1:n_colours])
  }
}


# https://www.dummies.com/education/math/statistics/
# how-to-compare-two-population-proportions/
# Why use 1.93? It seems this corresponds to a confidence interval of ~94.64%.
# Should it not be 1.96?
per_s_error_vect <- function(base1, per1, base2, per2, size = 3) {
  if (any(is.na(c(base1, per1, base2, per2)))) {
    return("")
  }

  pooled_per <- (base1 * per1 + base2 * per2) / (100 * (base1 + base2))
  s_error <- sqrt((pooled_per * (1 - pooled_per)) * ((1 / base1) + (1 / base2)))
  sig_value <- abs((per1 - per2) / (100 * s_error))

  if_else(
    sig_value > 1.93,
    if_else(
      per2 < per1,
      glue("<font color = '#DA291C', size = '{size}'>&#x25BC;</font>"),
      glue("<font color = '#009639', size = '{size}'>&#x25B2;</font>")
    ),
    ""
  )
}


content_box <- function(title, ...) {
  div(
    class = "sixteen wide column",
    style = "overflow: visible!important;",
    div(
      class = "sixteen wide column",
      div(
        class = "column",
        div(
          class = "ui segment",
          span(
            style = "font-size: 1.17em; font-weight: bolder;",
            title
          ),
          br(),
          br(),
          ...
        )
      )
    )
  )
}


# Labels for display elements. Style matches that used by highcharter.
label <- function(label_text) {
  div(
    style = paste0(
      "text-align:center;color:#333333;font-size:14px;font-weight:700;fill:#333333;"
    ),
    label_text
  )
}


# Row with 2 elements. The 2nd element is initially hidden.
# The output_fun should be a Shiny ...Output function.
fluidRow_2cols_toggle_B <- function(output_fun, output_name, legend = FALSE) {
  tagList(
    fluidRow(
      id = output_name,
      div(class = "groupA col-sm-12", output_fun(glue("{output_name}_A"))),
      div(class = "groupB hidden", output_fun(glue("{output_name}_B")))
    ),
    if (legend) plotOutput(glue("{output_name}_legend"), height = "50px"),
    br()
  )
}


# Dropdown for selecting an NPS group
user_group_select <- function(id) {
  div(
    fluidRow(
      column(
        3,
        shiny::selectInput(
          id,
          "Select rating group:",
          c("All", "Detractor", "Passive", "Promoter")
        )
      ),
      column(9),
    )
  )
}


# Radio for selecting from multiple similar aspects in stacked vertical charts
aspect_radio_select <- function(id, choices) {
  radioButtons(
    id,
    "Aspect",
    choices = choices,
    inline = TRUE
  )
}


suppress_warnings <- function(.expr, .f, ...) {
  eval.parent(substitute(
    withCallingHandlers(.expr, warning = function(w) {
      cm <- conditionMessage(w)
      cond <-
        if (is.character(.f)) grepl(.f, cm) else rlang::as_function(.f)(cm, ...)
      if (cond) {
        invokeRestart("muffleWarning")
      }
    })
  ))
}


info_tooltip <- function(content, style = "left-padding: 1px", ...) {
  span(
    class = "info-tippy", style = style, `data-tippy-content` = content,
    HTML("&#x1F6C8;")
  )
}
