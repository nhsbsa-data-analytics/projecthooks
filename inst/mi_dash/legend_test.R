library(ggpubr)
library(ggplot2)
library(RColorBrewer)
library(magrittr)

training_option_responses <- c(
  "training_option_faqs" = "Frequently asked questions (FAQs)",
  "training_option_guides" = "User guides",
  "training_option_videos" = "Videos",
  "training_option_website" = "NHS Jobs training website",
  "training_option_none" = "None of the above"
)

colour_ramp <- function(n_colours, first = "#003087", last = "#FFFFFF",
                        include_white = FALSE) {
  palette <- colorRampPalette(c(first, last))

  if (include_white) {
    c(palette(n_colours + 1))
  } else {
    c(palette(n_colours + 1)[1:n_colours])
  }
}

ui <- fluidPage(plotOutput("legend"))

server <- function(input, output, session) {
  output$legend <- renderPlot(
    (ggplot(data.frame(x = 1, y = 1), aes(x, y, fill = "white")) +
      geom_col() +
      scale_fill_manual(
        name = element_blank(),
        breaks = training_option_responses,
        values = setNames(
          colour_ramp(length(training_option_responses)),
          unname(training_option_responses)
        )
      ) +
      theme(legend.direction = "horizontal")) %>%
      get_legend() %>%
      as_ggplot()
  )
}

shinyApp(ui, server)
