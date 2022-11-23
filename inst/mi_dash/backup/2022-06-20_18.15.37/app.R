# Load packages -----------------------------------------------------------
library(shiny)
library(shinyjs)
library(shinyWidgets)

### Plotting
library(cowplot)
library(grid)
library(highcharter)
library(plotly)

### Data manipulation
library(DT)
library(tidyverse)

### Dates
library(lubridate)
library(zoo)

### Utility
library(glue)
library(RColorBrewer)
library(rlang)
library(tippy)

### Non-CRAN
library(NpsMoeTest)


# Error options -----------------------------------------------------------
options(warn = 1)
options(shiny.sanitize.errors = TRUE)
options(shiny.error = browser)


# Source files ------------------------------------------------------------
source("./global/index.R", local = TRUE)
source("./global/colours.R", local = TRUE)
source("./global/functions.R", local = TRUE)
source("./global/hcoptions.R", local = TRUE)
source("./global/levels.R", local = TRUE)
source("./global/choices.R", local = TRUE)
source("./ui/ui.R", local = TRUE)


ui <- function(app_title) {
  fluidPage(
    useShinyjs(),
    HTML('<html lang="en">'),
    HTML(
      '<meta name="viewport" content="width=device-width, initial-scale=0.86,
      maximum-scale=5.0, minimum-scale=0.86">'
    ),
    title = app_title,
    tags$head(
      tags$link(
        id = "stylesheet", rel = "stylesheet", href = "semantic/semantic.min.css"
      ),
      tags$link(rel = "stylesheet", href = "fonts.css"),
      tags$link(rel = "stylesheet", href = "style.css"),
      tags$link(rel = "stylesheet", href = "dataTables.min.css"),
      tags$link(rel = "stylesheet", href = "calendar.min.css"),
      tags$script(type = "text/javascript", src = "datatables.min.js"),
      tags$script(type = "text/javascript", src = "moment-with-locales.min.js"),
      tags$script(type = "text/javascript", src = "calendar.min.js"),
      tags$script(type = "text/javascript", src = "https://unpkg.com/@popperjs/core@2"),
      tags$script(type = "text/javascript", src = "https://unpkg.com/tippy.js@6"),
      # tags$script("tippy.setDefaultProps({hideOnClick: false, trigger: 'click', theme: 'nhs-blue', allowHTML: true});")
      tags$script("tippy.setDefaultProps({theme: 'nhs-blue', allowHTML: true});")
    ),
    sidemenu,
    header,
    content,
    footer,
    tags$script(src = "navigation_item_handler.js"),
    HTML("</html>")
  )
}


server <- function(input, output, session) {

  ### Reactive values for data
  ### There is a group A and B for which different regions can belong to.
  ### Each has a monthly and 3 month rolling (TMR) version
  groups <- list("A", "B")

  filter_data <- function(.data, .regions, three_month_rolling = FALSE) {
    minus_months <- months(0)
    if (three_month_rolling) minus_months <- months(2)

    .data %>%
      filter(
        between(
          first_of_month,
          input$month_range[1] %m-% minus_months,
          input$month_range[2]
        ),
        region %in% .regions
      )
  }

  filtered_data <- reactiveValues(
    A     = NULL,
    B     = NULL,
    TMR_A = NULL,
    TMR_B = NULL
  )

  observeEvent(
    {
      regions_saved_A()
      input$month_range
    },
    {
      filtered_data$A <- filter_data(data, regions_saved_A())
      filtered_data$TMR_A <- filter_data(data, regions_saved_A(), TRUE)
    }
  )

  observeEvent(
    {
      regions_saved_B()
      input$month_range
    },
    {
      filtered_data$B <- filter_data(data, regions_saved_B())
      filtered_data$TMR_B <- filter_data(data, regions_saved_B(), TRUE)
    }
  )


  # Source outputs ----------------------------------------------------------
  source("./server/output_functions.R", local = TRUE)
  source("./server/ui_outputs.R", local = TRUE)

  ### Page outputs under here, in menu order
  ### Page outputs above
}

shinyApp(ui("NHS Jobs 3 Employer Satisfaction Survey"), server)
