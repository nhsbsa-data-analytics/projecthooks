open_comments_page <- div(
  class = "ui open_comments page one column grid container dashboard hidden",
  style = "padding-top: 1em !important;",
  div(class = "sixteen wide column", h1("Open Comments")),
  content_box(
    "Open Comments",
    label("What do you feel is the most important feature of the new NHS Jobs service?"),
    user_group_select("important_feature_comment_group"),
    fluidRow_2cols_toggle_B(dataTableOutput, "important_feature_comment"),
    label("What features of the new NHS Jobs service worked well for you?"),
    user_group_select("worked_well_comment_group"),
    fluidRow_2cols_toggle_B(dataTableOutput, "worked_well_comment"),
    label("What would you improve about the new NHS Jobs service?"),
    user_group_select("would_improve_comment_group"),
    fluidRow_2cols_toggle_B(dataTableOutput, "would_improve_comment"),
  ),
  htmlOutput("open_commentstrigger")
)
