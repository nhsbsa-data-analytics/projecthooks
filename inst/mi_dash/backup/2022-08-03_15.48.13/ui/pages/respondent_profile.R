respondent_profile_page <- div(
  class = "ui respondent_profile page one column grid container dashboard hidden",
  style = "padding-top: 1em !important;",
  div(class = "sixteen wide column", h1("Respondent Profile")),
  content_box(
    "Training",
    label("Which of the following training and support resources hav"),
    fluidRow_2cols_toggle_B(plotlyOutput, "training_option", TRUE),
    label("How easy was the training to use?"),
    fluidRow_2cols_toggle_B(plotlyOutput, "training_ease_rating", TRUE),
    label("Reasons for rating"),
    user_group_select("training_ease_comment_group"),
    fluidRow_2cols_toggle_B(dataTableOutput, "training_ease_comment"),
    label("Other region"),
    fluidRow_2cols_toggle_B(dataTableOutput, "region_other_comment"),
    label("What is your current role in relation to recruitment?"),
    fluidRow_2cols_toggle_B(highchartOutput, "role"),
    label("Other role"),
    fluidRow_2cols_toggle_B(dataTableOutput, "role_other_comment"),
  ),
  content_box(
    "Respondent Details",
    label("What type of organisation do you work for?"),
    fluidRow_2cols_toggle_B(highchartOutput, "organisation"),
    label("Other organisation"),
    fluidRow_2cols_toggle_B(dataTableOutput, "organisation_other_comment"),
    label("In which region are you based?"),
    fluidRow_2cols_toggle_B(highchartOutput, "region"),
    label("Do you only use NHS Jobs or a combination of NHS Jobs and other systems?"),
    fluidRow_2cols_toggle_B(highchartOutput, "system_used", TRUE),
    label("Which other system(s) do you use?"),
    fluidRow_2cols_toggle_B(plotlyOutput, "system_used_option", TRUE),
    label("Other system used"),
    fluidRow_2cols_toggle_B(dataTableOutput, "system_used_other_comment"),
  ),
  content_box(
    "System Use",
    label("On average how often do you use NHS Jobs as part of your recruitment process?"),
    fluidRow_2cols_toggle_B(highchartOutput, "frequency_used"),
    label("How long have you been using the new NHS Jobs service?"),
    fluidRow_2cols_toggle_B(highchartOutput, "how_long_used"),

    #     label("Approximately how many vacancies have you published via the new NHS Jobs portal?"),
    #     fluidRow_2cols_toggle_B(CUSTOM_OUTPUT, "num_of_vacancies"),
  ),
  htmlOutput("respondent_profiletrigger")
)
