library(shiny)
library(shinyWidgets)
library(bsplus)

region_responses <- c(
  "East of England"                      = "East of England",
  "London"                               = "London",
  "Midlands"                             = "Midlands",
  "National"                             = "National",
  "North East and Yorkshire"             = "North East and Yorkshire",
  "North West"                           = "North West",
  "South East"                           = "South East",
  "South West"                           = "South West",
  "Wales"                                = "Wales",
  "Other (please type in the box below)" = "Other",
  "Prefer not to say"                    = "Undisclosed"
)

# Swaps names and values for use in region selection UI so that e.g. selecting
# "Undisclosed" passes the actual data value "Prefer not to say"
regions <- setNames(names(region_responses), unname(region_responses))

ui <- fluidPage(
  tags$head(
    tags$style("
      .fa {
        font-family: 'Font Awesome 5 Free' !important;
      }

      i.icon.fa-times {
        opacity: 1 !important;
        color: #ffffff
      }

      .fa-times:before {
        content: '/f00d'
      }

      .fa-check:before {
        content: '/f00c'
      }

      .pretty input:not(:checked)~.state.p-success label:after {
        background-color: #DA291C;
      }
    "),
    tags$link(
      id = "stylesheet", rel = "stylesheet", href = "semantic.min.css"
    )
  ),
  tags$h1("Pretty checkbox group"),
  br(),
  bs_collapse(id = "id_yeah", tags$p("Yeah Yeah Yeah")),
  tags$a(href = "#", "She Loves You") %>%
    bs_attach_collapse("id_yeah"),
  prettyCheckboxGroup(
    inputId = "regions_cb_A",
    label = NULL,
    status = "success",
    choiceNames = names(regions),
    choiceValues = names(regions),
    selected = names(regions),
    shape = "curve", # c("square", "curve", "round"),
    outline = FALSE,
    fill = FALSE,
    thick = TRUE,
    animation = "smooth", # smooth, jelly, tada, rotate, pulse
    icon = icon("check"),
    plain = FALSE,
    bigger = FALSE,
    inline = FALSE,
    width = NULL
  ),
  fluidRow(
    column(
      width = 4,
      prettyCheckboxGroup(
        inputId = "checkgroup1",
        label = "Click me!",
        choices = c("Click me !", "Me !", "Or me !")
      ),
      verbatimTextOutput(outputId = "res1"),
      br(),
      prettyCheckboxGroup(
        inputId = "checkgroup4",
        label = "Click me!",
        choices = c("Click me !", "Me !", "Or me !"),
        status = "success",
        icon = icon("times")
      ),
      verbatimTextOutput(outputId = "res4")
    ),
    column(
      width = 4,
      prettyCheckboxGroup(
        inputId = "checkgroup2",
        label = "Click me!",
        thick = TRUE,
        choices = c("Click me !", "Me !", "Or me !"),
        animation = "pulse",
        status = "info"
      ),
      verbatimTextOutput(outputId = "res2"),
      br(),
      prettyCheckboxGroup(
        inputId = "checkgroup5",
        label = "Click me!",
        icon = icon("check"),
        choices = c("Click me !", "Me !", "Or me !"),
        animation = "tada",
        status = "default"
      ),
      verbatimTextOutput(outputId = "res5")
    ),
    column(
      width = 4,
      prettyCheckboxGroup(
        inputId = "checkgroup3",
        label = "Click me!",
        choices = c("Click me !", "Me !", "Or me !"),
        shape = "round",
        status = "danger",
        fill = TRUE,
        inline = TRUE
      ),
      verbatimTextOutput(outputId = "res3")
    )
  ),
  tags$script("
    jQuery('.pretty.p-icon input:checkbox').change(function () {
      if (this.checked) {
        $(this).next().children(':first').addClass('fa-check');
        $(this).next().children(':first').removeClass('fa-times');
      }
      else
      {
        $(this).next().children(':first').removeClass('fa-check');
        $(this).next().children(':first').addClass('fa-times');
      }
    });
  ")
)

server <- function(input, output, session) {
  output$res1 <- renderPrint(input$checkgroup1)
  output$res2 <- renderPrint(input$checkgroup2)
  output$res3 <- renderPrint(input$checkgroup3)
  output$res4 <- renderPrint(input$checkgroup4)
  output$res5 <- renderPrint(input$checkgroup5)
}

shinyApp(ui, server)
