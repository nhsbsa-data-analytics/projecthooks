### Used to display selected month range in top right of UI
month_range_indicator <- function(month_range) {
  if (month_range[1] == month_range[2]) {
    format(month_range[1], format = "%B %Y")
  } else {
    paste(
      format(month_range[1], format = "%B %Y"),
      "-",
      format(month_range[2], format = "%B %Y")
    )
  }
}


### Use for line charts over time (monthly and 3 month rolling
### average); measure_type should be either "NPS" or "NES"
line_chart <- function(data,
                       measure_column, measure_type, measure_aux = NULL,
                       targets = NULL,
                       three_month_rolling = FALSE,
                       title = NULL, subtitle = NULL) {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  if (three_month_rolling) {
    validate(
      need(
        month_diff(
          min(data$first_of_month) %m-% months(2), max(data$first_of_month)
        ) >= 3,
        "Less than 3 months of data!"
      )
    )
  }

  chart <- data %>%
    {
      if (!is.null(measure_aux)) {
        select(., first_of_month) %>%
          unique() %>%
          left_join(
            measure_aux
          )
      } else {
        select(
          .,
          first_of_month,
          !!measure_column,
        ) %>%
          na.omit(
            !!measure_column
          ) %>%
          filter(
            !!measure_column != "",
          ) %>%
          mutate(
            group = case_when(
              !!sym(measure_column) %in% c(c(1:6), nes_responses[1:3]) ~ "Detractor",
              !!sym(measure_column) %in% c(c(7:8), nes_responses[4:5]) ~ "Passive",
              !!sym(measure_column) %in% c(c(9:10), nes_responses[6:7]) ~ "Promoter"
            )
          ) %>%
          group_by(
            first_of_month,
            group
          ) %>%
          count() %>%
          ungroup() %>%
          full_join(
            expand.grid(
              first_of_month = unique(data$first_of_month),
              group = names(nps_groups)
            ),
            by = c("first_of_month", "group")
          ) %>%
          replace_na(list(n = 0)) %>%
          complete(
            group,
            nesting(first_of_month),
            fill = list(n = 0)
          ) %>%
          select(
            first_of_month,
            group,
            n
          ) %>%
          spread(
            group,
            n
          )
      }
    } %>%
    {
      if (!is.null(targets)) {
        arrange(., first_of_month) %>%
          left_join(
            targets,
            by = "first_of_month"
          )
      } else {
        .
      }
    } %>%
    mutate(
      base = Detractor + Passive + Promoter,
      prevDetractor = lag(Detractor, 1),
      prevPassive = lag(Passive, 1),
      prevPromoter = lag(Promoter, 1),
    ) %>%
    {
      if (three_month_rolling) {
        mutate(
          .,
          prevDetractor2 = lag(Detractor, 2),
          prevPassive2 = lag(Passive, 2),
          prevPromoter2 = lag(Promoter, 2),
          det3MR = Detractor + prevDetractor + prevDetractor2,
          pas3MR = Passive + prevPassive + prevPassive2,
          pro3MR = Promoter + prevPromoter + prevPromoter2,
          prevdet3MR = lag(det3MR, 1),
          prevpas3MR = lag(pas3MR, 1),
          prevpro3MR = lag(pro3MR, 1),
          base3MR = Detractor + Passive + Promoter +
            prevDetractor + prevPassive + prevPromoter +
            prevDetractor2 + prevPassive2 + prevPromoter2,
          Score3MR = round(((pro3MR / base3MR) - (det3MR / base3MR)) * 100)
        )
      } else {
        mutate(
          .,
          !!sym(measure_column) := round(
            ((Promoter / base) - (Detractor / base)) * 100
          )
        )
      }
    } %>%
    {
      if (three_month_rolling) {
        filter(
          .,
          !is.na(Score3MR)
        )
      } else {
        .
      }
    }

  chart$score_sig <- ""

  if (three_month_rolling) {
    for (i in 1:nrow(chart)) {
      chart$score_sig[i] <- nps_moe_test(
        chart$pro3MR[i],
        chart$pas3MR[i],
        chart$det3MR[i],
        chart$prevpro3MR[i],
        chart$prevpas3MR[i],
        chart$prevdet3MR[i]
      )
    }

    y <- sym("Score3MR")
    base <- sym("base3MR")
    name <- glue("{measure_type} (3MR)")
  } else {
    for (i in 1:nrow(chart)) {
      chart$score_sig[i] <- nps_moe_test(
        chart$Promoter[i],
        chart$Passive[i],
        chart$Detractor[i],
        chart$prevPromoter[i],
        chart$prevPassive[i],
        chart$prevDetractor[i]
      )
    }

    y <- sym(measure_column)
    base <- sym("base")
    name <- measure_type
  }

  if (!is.null(targets)) {
    targets <- sym(glue("{measure_type}_Target"))
  }

  chart <- chart %>%
    mutate(
      score_sig = case_when(
        score_sig == -1 ~ "<font color = '#DA291C', size = '3'>&#x25BC;</font>",
        score_sig == +1 ~ "<font color = '#009639', size = '3'>&#x25B2;</font>",
        TRUE ~ ""
      )
    )

  highchart() %>%
    hc_add_series(
      type = "line",
      name = as_name(name),
      color = blues[2],
      data = chart,
      hcaes(
        x = first_of_month,
        y = !!y,
        base = !!base,
        sig = score_sig
      ),
      dataLabels = list(
        enabled = TRUE,
        formatter = JS("function () { return this.point.y + this.point.sig; }"),
        useHTML = TRUE
      )
    ) %>%
    hc_add_series(
      type = "line",
      name = "Base",
      color = "white",
      data = chart,
      visible = FALSE,
      marker = list(
        enabled = FALSE
      ),
      hcaes(
        x = first_of_month,
        y = base
      ),
      showInLegend = FALSE
    ) %>%
    {
      if (!is.null(targets)) {
        hc_add_series(
          .,
          name = "Target",
          color = "black",
          data = chart,
          type = "line",
          hcaes(
            x = first_of_month,
            y = !!targets
          ),
          dataLabels = list(
            enabled = F
          ),
          marker = list(
            enabled = F
          ),
          dashStyle = "dash"
        )
      } else {
        .
      }
    } %>%
    hc_xAxis(
      type = "datetime",
      minTickInterval = 30 * 24 * 3600 * 1000,
      labels = list(
        rotation = -45,
        step = 1
      )
    ) %>%
    hc_yAxis(
      max = 100,
      min = -100
    ) %>%
    hc_tooltip(
      shared = TRUE,
      style = list(
        zIndex = 10000
      ),
      xDateFormat = "%b '%y",
      valueDecimals = 0,
      backgroundColor = unname(neutrals["NHS Pale Grey"]),
      useHTML = FALSE
    ) %>%
    hc_title(
      text = title
    ) %>%
    hc_subtitle(
      text = subtitle
    ) %>%
    hc_plotOptions(
      series = list(
        events = list(
          legendItemClick = JS("function() {return false;}")
        )
      )
    )
}


### Use for NPS groups barcharts (monthly and 3 month rolling average)
nps_group_chart <- function(data, three_month_rolling = FALSE) {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  if (three_month_rolling) {
    validate(
      need(
        month_diff(
          min(data$first_of_month) %m-% months(2), max(data$first_of_month)
        ) >= 3,
        "Less than 3 months of data!"
      )
    )
  }

  chart <- data %>%
    select(
      first_of_month,
      nps
    ) %>%
    na.omit(
      nps
    ) %>%
    filter(
      nps != ""
    ) %>%
    mutate(
      group = case_when(
        nps %in% c(1:6) ~ "Detractor",
        nps %in% c(7:8) ~ "Passive",
        nps %in% c(9:10) ~ "Promoter"
      )
    ) %>%
    group_by(
      first_of_month,
      group
    ) %>%
    count() %>%
    ungroup() %>%
    complete(
      group,
      nesting(first_of_month),
      fill = list(n = 0)
    ) %>%
    full_join(
      expand.grid(
        first_of_month = unique(data$first_of_month),
        group = names(nps_groups)
      ),
      by = c("first_of_month", "group")
    ) %>%
    replace_na(list(n = 0)) %>%
    select(
      first_of_month,
      group,
      n
    ) %>%
    spread(
      group,
      n
    ) %>%
    mutate(
      across(
        !first_of_month,
        ~ round(100 * . / (Detractor + Passive + Promoter)),
        .names = "{.col}_perc"
      )
    ) %>%
    {
      if (three_month_rolling) {
        mutate(
          .,
          prevDetrator = lag(Detractor, 1),
          prevPassive = lag(Passive, 1),
          prevPromoter = lag(Promoter, 1),
          prevDetrator2 = lag(Detractor, 2),
          prevPassive2 = lag(Passive, 2),
          prevPromoter2 = lag(Promoter, 2),
          det3MR = Detractor + prevDetrator + prevDetrator2,
          pas3MR = Passive + prevPassive + prevPassive2,
          pro3MR = Promoter + prevPromoter + prevPromoter2,
          prevdet3MR = lag(det3MR, 1),
          prevpas3MR = lag(pas3MR, 1),
          prevpro3MR = lag(pro3MR, 1),
          base3MR = Detractor + Passive + Promoter +
            prevDetrator + prevPassive + prevPromoter +
            prevDetrator2 + prevPassive2 + prevPromoter2,
          Score3MR = round(((pro3MR / base3MR) - (det3MR / base3MR)) * 100),
          Detractor = det3MR,
          Passive = pas3MR,
          Promoter = pro3MR
        ) %>%
          filter(
            !is.na(Score3MR)
          )
      } else {
        .
      }
    } %>%
    filter(
      first_of_month %in% sort(
        unique(data$first_of_month),
        decreasing = TRUE
      )[1:3]
    )

  means <<- data %>%
    select(
      first_of_month,
      nps
    ) %>%
    na.omit(
      nps
    ) %>%
    filter(
      nps != ""
    ) %>%
    group_by(
      first_of_month
    ) %>%
    summarise(
      base = n(),
      mean = suppress_warnings(
        {
          as.numeric(format(round(mean(nps), 1), nsmall = 1))
        },
        endsWith,
        "by coercion"
      ),
    ) %>%
    ungroup() %>%
    {
      if (three_month_rolling) {
        mutate(
          .,
          prevBase1 = lag(base, 1),
          prevMean1 = lag(mean, 1),
          prevBase2 = lag(base, 2),
          prevMean2 = lag(mean, 2),
        ) %>%
          filter(
            first_of_month %in% sort(
              unique(data$first_of_month),
              decreasing = TRUE
            )[1:3]
          ) %>%
          mutate(
            mean = (
              (mean * base) + (prevBase1 * prevMean1) + (prevMean2 * prevBase2)) /
              (base + prevBase1 + prevBase2),
            mean = suppress_warnings(
              {
                as.numeric(format(round(mean(mean), 1), nsmall = 1))
              },
              endsWith,
              "by coercion"
            ),
            base = (base + prevBase1 + prevBase2),
            label = glue("<b>{format(first_of_month, format = '%b %y')}</b><br>
                         Mean score: <b>{mean}</b><br>
                         Base: <b>{base}</b>")
          ) %>%
          select(
            first_of_month,
            base,
            mean,
            label
          )
      } else {
        filter(
          .,
          first_of_month %in% sort(
            unique(data$first_of_month),
            decreasing = TRUE
          )[1:3]
        ) %>%
          mutate(
            label = glue("<b>{format(first_of_month, format = '%b %y')}</b><br>
                         Mean score: <b>{mean}</b><br>
                         Base: <b>{base}</b>")
          )
      }
    }

  chart <- chart %>%
    left_join(
      means,
      by = "first_of_month"
    )

  highchart() %>%
    hc_add_series(
      type = "column",
      data = chart,
      color = highlights["Emergency Services Red"],
      name = "Detractors",
      hcaes(
        x = label,
        y = Detractor,
        perc = Detractor_perc
      ),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.perc} %"
      )
    ) %>%
    hc_add_series(
      type = "column",
      data = chart,
      color = highlights["NHS Orange"],
      name = "Passives",
      hcaes(
        x = label,
        y = Passive,
        perc = Passive_perc
      ),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.perc} %"
      )
    ) %>%
    hc_add_series(
      type = "column",
      data = chart,
      color = support_greens["NHS Green"],
      name = "Promoters",
      hcaes(
        x = label,
        y = Promoter,
        perc = Promoter_perc
      ),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.perc} %"
      )
    ) %>%
    hc_xAxis(
      type = "category"
    ) %>%
    hc_tooltip(
      shared = TRUE,
      style = list(
        zIndex = 10000
      ),
      valueDecimals = 0,
      backgroundColor = unname(neutrals["NHS Pale Grey"]),
      useHTML = TRUE
    )
}


### Use for free text tables such as comments and reasons; give the column of
### interest and the associated selectInput id to use for NPS grouping as strings.
comment_table <- function(data, comment_column, group_select_id = NULL,
                          comment_name_in_DT = "Reasons") {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  columns <- c("first_of_month", comment_column)
  if (!is.null(group_select_id)) columns <- c(columns, "nps")

  table <- data %>%
    select(
      all_of(columns)
    ) %>%
    na.omit() %>%
    filter(
      !!sym(comment_column) != ""
    ) %>%
    {
      if (!is.null(group_select_id)) {
        mutate(
          .,
          group = case_when(
            nps %in% c(1:6) ~ "Detractor",
            nps %in% c(7:8) ~ "Passive",
            nps %in% c(9:10) ~ "Promoter"
          )
        )
      } else {
        .
      }
    }

  if (!is.null(group_select_id)) {
    if (input[[group_select_id]] != "All") {
      table <- table %>%
        filter(
          group == input[[group_select_id]]
        )
    }
  }

  table <- table %>%
    {
      if (!is.null(group_select_id)) {
        select(
          .,
          first_of_month,
          !!comment_column,
          group
        )
      } else {
        select(
          .,
          first_of_month,
          !!comment_column
        )
      }
    } %>%
    arrange(
      desc(first_of_month),
      !!comment_column
    ) %>%
    mutate(
      first_of_month = format(first_of_month, format = "%b %Y")
    )

  colnames <- c("Month", comment_name_in_DT)
  if (!is.null(group_select_id)) colnames <- c(colnames, "Group")

  datatable(
    table,
    rownames = FALSE,
    colnames = colnames
  )
}


### Use for vertical stacked bar charts showing percentages of some categorical
### feature.
stacked_vertical <- function(data, columns, cols_start_with = TRUE,
                             responses = NULL, colours = "ramp",
                             radio_select_id = NULL) {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  column_prefix <- columns

  if (!is.null(radio_select_id)) {
    columns <- input[[radio_select_id]]
  } else {
    if (cols_start_with) columns <- names(data %>% select(starts_with(columns)))
  }

  options <- data %>%
    select(
      first_of_month,
      all_of(columns)
    ) %>%
    pivot_longer(
      -first_of_month,
      names_to = "question_column",
      values_drop_na = TRUE
    ) %>%
    filter(value != "")

  if (is.null(responses)) {
    if (exists(glue("{column_prefix}_responses"))) {
      responses <- get(glue("{column_prefix}_responses"))
    } else {
      distinct_responses <- options %>%
        select(-first_of_month) %>%
        distinct()

      responses <- distinct_responses$value
    }
  }

  if (colours == "ramp") {
    colours <- colour_ramp(length(responses))
  }

  ggplotly(
    options %>%
      mutate(
        question_column = case_when(
          length(unique(question_column)) == 1 ~ value,
          TRUE ~ question_column
        )
      ) %>%
      group_by(
        first_of_month,
        question_column
      ) %>%
      count() %>%
      na.omit() %>%
      group_by(
        first_of_month
      ) %>%
      mutate(
        base_for_month = sum(n),
        percentage = n / sum(n) * 100
      ) %>%
      mutate(
        question_column = factor(
          question_column,
          levels = names(responses) %||% responses,
          labels = responses
        )
      ) %>%
      select(
        first_of_month,
        question_column,
        percentage,
        base_for_month
      ) %>%
      ggplot(
        aes(
          x = first_of_month,
          y = percentage,
          fill = question_column,
          text = glue(
            "{date_Ymd_to_bY(first_of_month)} ({base_for_month})\n",
            "{question_column}\n",
            "{decimal_places(percentage, 0)}%"
          ),
        )
      ) +
      geom_col() +
      labs(
        y = "Percentage of respondents (%)",
        x = element_blank()
      ) +
      scale_x_date(
        date_breaks = "1 month",
        expand = c(0, 0),
        date_labels = "%B '%y"
      ) +
      scale_fill_manual(
        values = setNames(
          colours[1:length(responses)],
          responses
        )
      ) +
      theme_classic() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1
        )
      ),
    tooltip = "text"
  ) %>%
    config(
      displayModeBar = FALSE
    ) %>%
    layout(
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.4)
    )
}


### Use for horizontal stacked barcharts showing percentages of some categorical
### feature. These show a 3 month rolling bar followed by a bar for each of the
### latest 3 months.
# TODO: add some data to use this + refactor responses & colours
stacked_horizontal <- function(data, question_column, responses = NULL,
                               colours = "ramp", legend_names = NULL) {
  question_column <- "region"
  responses <- NULL
  colours <- "ramp"
  legend_names <- region_responses

  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  question <- data %>%
    select(
      first_of_month,
      !!question_column
    ) %>%
    na.omit() %>%
    filter(
      !!question_column != ""
    )

  if (is.null(responses)) {
    if (exists(glue("{question_column}_responses"))) {
      responses <- get(glue("{question_column}_responses"))
    } else {
      distinct_responses <- question %>%
        select(-first_of_month) %>%
        distinct()

      responses <- distinct_responses$value
      names(responses) <- distinct_responses$question_column
    }
  }

  if (colours == "ramp") {
    colours <- colour_ramp(length(responses))
  }

  if (is.null(legend_names)) {
    legend_names <- responses
  }

  question <- question %>%
    group_by(
      first_of_month
    )

  chart <- question %>%
    select(first_of_month) %>%
    unique()

  for (name in names(responses)) {
    summ <- question %>%
      summarise(
        !!name := sum(.data[[question_column]] == responses[name])
      )

    chart <- chart %>%
      left_join(
        summ,
        by = "first_of_month"
      )
  }

  chart <- chart %>%
    mutate(
      base = sum(across(!starts_with("first_of_month"))),
      across(
        !starts_with(c("first_of_month", "base")),
        ~ 100 * . / base,
        .names = "percent_{.col}"
      )
    ) %>%
    ungroup()

  for (name in names(chart)[-c(0:length(responses) + 1)]) {
    chart <- chart %>%
      mutate(
        "prev_{name}" := lag(!!sym(name))
      )
  }

  for (name in names(responses)) {
    chart <- chart %>%
      mutate(
        !!glue("{name}_significant") := per_s_error_vect(
          prev_base,
          !!sym(glue("prev_percent_{name}")),
          base,
          !!sym(glue("percent_{name}"))
        )
      )
  }

  chart <- chart %>%
    filter(
      first_of_month %in% sort(
        unique(data$first_of_month),
        decreasing = TRUE
      )[1:3]
    ) %>%
    mutate(
      first_of_month = paste0(
        format(
          first_of_month,
          format = "%b %y"
        ),
        "<br> (base: ",
        base,
        ")"
      )
    )

  rolling <- data %>%
    filter(
      first_of_month %in% sort(
        unique(data$first_of_month),
        decreasing = TRUE
      )[1:3]
    ) %>%
    select(
      first_of_month,
      !!question_column
    ) %>%
    na.omit() %>%
    filter(
      !!question_column != ""
    ) %>%
    group_by(
      !!sym(question_column)
    ) %>%
    count() %>%
    ungroup()

  rolling_base <- sum(rolling$n)

  vals_list <- sapply(
    names(responses),
    function(x) {
      sum(
        rolling$n[rolling[[question_column]] == responses[x]]
      ) / rolling_base * 100
    }
  )

  rolling_percentages <- setNames(
    vals_list,
    names(responses)
  )

  rolling_data <- data.frame(
    "first_of_month" = paste0(
      "<b>3 Month Rolling<br>(base: ", rolling_base, ")</b>"
    )
  )

  for (name in names(responses)) {
    rolling_data <- rolling_data %>%
      mutate(
        !!name := NA
      )
  }

  rolling_data <- rolling_data %>%
    mutate(
      base = rolling_base
    )

  for (name in names(responses)) {
    rolling_data <- rolling_data %>%
      mutate(
        "percent_{name}" := rolling_percentages[[name]]
      )
  }

  rolling_data <- rolling_data %>%
    mutate(
      prev_base = NA
    )

  for (name in names(responses)) {
    rolling_data <- rolling_data %>%
      mutate(
        "prev_percent_{name}" := NA
      )
  }

  for (name in names(responses)) {
    rolling_data <- rolling_data %>%
      mutate(
        "{name}_significant" := ""
      )
  }

  chart <- rolling_data %>%
    bind_rows(chart)

  hc <- highchart()

  i <- 1
  for (response in names(responses)) {
    hc <- hc %>%
      hc_add_series(
        name = legend_names[[responses[[response]]]],
        color = colours[i],
        data = chart,
        type = "bar",
        hcaes(
          x = first_of_month,
          y = round(!!sym(glue("percent_{response}"))),
          sig = !!sym(glue("{response}_significant"))
        )
      )

    i <- i + 1
  }

  hc %>%
    hc_xAxis(
      type = "category"
    ) %>%
    hc_plotOptions(
      series = list(
        stacking = "normal",
        dataLabels = list(
          backgroundColor = "rgba(255,255,255,0.6)",
          borderRadius = 5,
          padding = 2,
          enabled = TRUE,
          inside = TRUE,
          style = list(
            textOutline = "none",
            color = "black",
            fontWeight = "bolder",
            fontSize = "15px"
          ),
          formatter = JS(
            "function() {
              var yval = this.point.y.toFixed(0);
              var sigval = this.point.sig;
              return yval + sigval;
            }"
          ),
          useHTML = TRUE
        )
      )
    ) %>%
    hc_yAxis(
      max = 100
    ) %>%
    hc_legend(
      reversed = TRUE
    ) %>%
    hc_tooltip(
      shared = FALSE,
      valueDecimals = 0,
      backgroundColor = unname(neutrals["NHS Pale Grey"]),
      useHTML = TRUE,
      pointFormat = "<b>Percentage: {point.y}%</b><br>"
    )
}


### Pie chart showing percentage of unique responses. Use for non-rating style
### questions with 3 or less possible responses only.
pie <- function(data, question_column, responses = NULL, colours = "ramp",
                chart_title = NULL) {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  chart <- data %>%
    select(
      !!question_column
    ) %>%
    na.omit() %>%
    filter(
      !!sym(question_column) != ""
    )

  if (is.null(responses)) {
    if (exists(glue("{question_column}_responses"))) {
      responses <- get(glue("{question_column}_responses"))
    } else {
      distinct_responses <- chart %>%
        distinct()

      responses <- distinct_responses$value
    }
  }

  if (colours == "ramp") {
    colours <- colour_ramp(length(responses))
  }

  chart <- chart %>%
    mutate(
      !!question_column := factor(
        !!sym(question_column),
        levels = names(responses) %||% responses,
        labels = responses
      )
    ) %>%
    group_by(
      !!sym(question_column),
      .drop = FALSE
    ) %>%
    summarise(
      n = n()
    ) %>%
    mutate(
      per = round(n / sum(n) * 100)
    ) %>%
    arrange(
      !!sym(question_column)
    ) %>%
    mutate(
      colour = colours[which(levels(!!sym(question_column)) == !!sym(question_column))]
    )

  base <- sum(chart$n)

  highchart() %>%
    hc_add_series(
      name = "Response",
      data = chart,
      type = "pie",
      hcaes(
        x = !!sym(question_column),
        y = n,
        color = colour
      )
    ) %>%
    {
      if (!is.null(chart_title)) {
        hc_title(
          .,
          text = chart_title
        )
      } else {
        .
      }
    } %>%
    hc_subtitle(
      text = paste0("Base: ", base)
    ) %>%
    hc_tooltip(
      shared = TRUE,
      style = list(
        zIndex = 10000
      ),
      pointFormat = "<b>Percentage:</b> {point.per}%<b><br>Count: </b>{point.n}<br>",
      valueDecimals = 0,
      backgroundColor = unname(neutrals["NHS Pale Grey"]),
      useHTML = FALSE
    ) %>%
    hc_legend(
      enabled = FALSE,
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal"
    ) %>%
    hc_plotOptions(
      pie = list(
        dataLabels = list(
          enabled = FALSE
        ),
        showInLegend = TRUE
      )
    )
}


### Use for horizontal barcharts showing percentages of some categorical
### feature. Use for non-rating style questions with more than 3 responses only.
horizontal_bar <- function(data, columns, cols_start_with = FALSE,
                           responses = NULL, colour = blues[2], chart_title = NULL,
                           arrange_desc = TRUE) {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  column_prefix <- columns
  if (cols_start_with) columns <- names(data %>% select(starts_with(columns)))

  options <- data %>%
    select(
      first_of_month,
      all_of(columns)
    ) %>%
    pivot_longer(
      -first_of_month,
      names_to = "question_column",
      values_drop_na = TRUE
    ) %>%
    filter(value != "") %>%
    mutate(
      question_column = case_when(
        length(unique(question_column)) == 1 ~ value,
        TRUE ~ question_column
      )
    )

  if (is.null(responses)) {
    if (exists(glue("{column_prefix}_responses"))) {
      responses <- get(glue("{column_prefix}_responses"))
    } else {
      distinct_responses <- options %>%
        distinct()

      responses <- distinct_responses$value
    }
  }

  chart <- options %>%
    group_by(
      question_column
    ) %>%
    select(
      question_column
    ) %>%
    count() %>%
    ungroup() %>%
    mutate(
      percentage = n / sum(n) * 100
    ) %>%
    mutate(
      question_column = factor(
        question_column,
        levels = names(responses) %||% responses,
        labels = responses
      )
    ) %>%
    select(
      question_column,
      percentage,
      n
    ) %>%
    {
      if (arrange_desc) {
        arrange(
          .,
          desc(percentage)
        )
      } else {
        .
      }
    }

  base <- sum(chart$n)

  highchart() %>%
    hc_add_series(
      type = "bar",
      color = colour,
      data = chart,
      hcaes(
        x = question_column,
        y = round(percentage)
      )
    ) %>%
    hc_xAxis(
      type = "category"
    ) %>%
    hc_legend(
      enabled = FALSE
    ) %>%
    hc_yAxis(
      title = list(
        text = "% responses"
      )
    ) %>%
    {
      if (!is.null(chart_title)) {
        hc_title(
          .,
          text = chart_title
        )
      } else {
        .
      }
    } %>%
    hc_subtitle(
      text = paste0("Base: ", base)
    ) %>%
    hc_tooltip(
      shared = TRUE,
      style = list(
        zIndex = 10000
      ),
      pointFormat = "<b>Percentage:</b> {point.y}%<b><br>Count: </b>{point.n}<br>",
      valueDecimals = 0,
      backgroundColor = unname(neutrals["NHS Pale Grey"]),
      useHTML = FALSE
    )
}


### Use for displaying % of coded responses.
coding_horizontal_bar <- function(data, coded_column, total_column, colour,
                                  chart_title, arrange_desc = TRUE) {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  chart <- data %>%
    na.omit() %>%
    select(
      !!coded_column,
      !!total_column
    ) %>%
    group_by(
      !!sym(coded_column)
    ) %>%
    mutate(
      !!total_column := sum(!!sym(total_column))
    ) %>%
    unique() %>%
    ungroup() %>%
    mutate(
      percentage = !!sym(total_column) / sum(!!sym(total_column)) * 100
    ) %>%
    select(
      !!coded_column,
      n = !!total_column,
      percentage
    ) %>%
    {
      if (arrange_desc) {
        arrange(
          .,
          desc(percentage)
        )
      } else {
        .
      }
    } %>%
    filter(
      percentage > 1
    )

  base <- sum(chart$n)

  highchart() %>%
    hc_add_series(
      type = "bar",
      color = colour,
      data = chart,
      hcaes(
        x = !!coded_column,
        y = round(percentage)
      )
    ) %>%
    hc_xAxis(
      type = "category"
    ) %>%
    hc_legend(
      enabled = FALSE
    ) %>%
    hc_yAxis(
      title = list(
        text = "% responses"
      )
    ) %>%
    hc_title(
      text = chart_title
    ) %>%
    hc_subtitle(
      text = paste0("Base: ", base)
    ) %>%
    hc_tooltip(
      shared = TRUE,
      style = list(
        zIndex = 10000
      ),
      pointFormat = "<b>Percentage:</b> {point.y}%<b><br>Count: </b>{point.n}<br>",
      valueDecimals = 0,
      backgroundColor = unname(neutrals["NHS Pale Grey"]),
      useHTML = FALSE
    )
}


# Legend for plots that shows all possible options in items.
create_legend <- function(items) {
  items <- glue("{items}_responses")
  (ggplot(data.frame(x = 1, y = 1), aes(x, y, fill = "white")) +
    geom_col() +
    scale_fill_manual(
      name = element_blank(),
      breaks = get(items),
      values = setNames(
        colour_ramp(length(get(items))),
        unname(get(items))
      )
    ) +
    theme(legend.direction = "horizontal")) %>%
    get_legend() %>%
    as_ggplot()
}
