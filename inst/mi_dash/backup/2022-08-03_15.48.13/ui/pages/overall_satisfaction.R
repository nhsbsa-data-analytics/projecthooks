overall_satisfaction_page <- div(
  class = "ui overall_satisfaction page one column grid container dashboard visible",
  style = "padding-top: 1em !important;",
  div(class = "sixteen wide column", h1("Overall Satisfaction")),
  content_box(
    "Net Promoter Score",
    label("Overall, how satisfied are you with the new NHS Jobs service?"),
    fluidRow_2cols_toggle_B(highchartOutput, "nps_monthly"),
    label("Reasons for rating"),
    user_group_select("nps_comment_group"),
    fluidRow_2cols_toggle_B(dataTableOutput, "nps_comment"),
  ),
  content_box(
    "Net Easy Score",
    label("Overall, how easy is it to use the new NHS Jobs service?"),
    fluidRow_2cols_toggle_B(highchartOutput, "nes_monthly"),
    label("Reasons for rating"),
    user_group_select("nes_comment_group"),
    fluidRow_2cols_toggle_B(dataTableOutput, "nes_comment"),
  ),
  htmlOutput("overall_satisfactiontrigger")
)
