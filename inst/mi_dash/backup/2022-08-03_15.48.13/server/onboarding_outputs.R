# involved_onboarding
lapply(groups, function(x) {
  output[[glue("involved_onboarding_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "involved_onboarding")
  })
})

output$involved_onboarding_legend <- renderPlot(
  create_legend("involved_onboarding")
)

# answered_qs_onboarding
lapply(groups, function(x) {
  output[[glue("answered_qs_onboarding_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "answered_qs_onboarding")
  })
})

output$answered_qs_onboarding_legend <- renderPlot(
  create_legend("answered_qs_onboarding")
)

# onboarding_aspects_rating
lapply(groups, function(x) {
  output[[glue("onboarding_aspects_rating_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "onboarding_aspects_rating", radio_select_id = "onboarding_aspects_rating_radio")
  })
})

output$onboarding_aspects_rating_legend <- renderPlot(
  create_legend("onboarding_aspects_rating")
)

# onboarding_aspects_comment
lapply(groups, function(x) {
  output[[glue("onboarding_aspects_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "onboarding_aspects_comment", group_select_id = "onboarding_aspects_comment_group")
  })
})

# challenge_option
lapply(groups, function(x) {
  output[[glue("challenge_option_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "challenge_option")
  })
})

output$challenge_option_legend <- renderPlot(
  create_legend("challenge_option")
)

# challenge_other_comment
lapply(groups, function(x) {
  output[[glue("challenge_other_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "challenge_other_comment", group_select_id = "challenge_other_comment_group")
  })
})

# transition_reqs_met
lapply(groups, function(x) {
  output[[glue("transition_reqs_met_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "transition_reqs_met")
  })
})

output$transition_reqs_met_legend <- renderPlot(
  create_legend("transition_reqs_met")
)

# transition_reqs_not_met_comment
lapply(groups, function(x) {
  output[[glue("transition_reqs_not_met_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "transition_reqs_not_met_comment", group_select_id = "transition_reqs_not_met_comment_group")
  })
})

# contact
lapply(groups, function(x) {
  output[[glue("contact_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "contact")
  })
})

output$contact_legend <- renderPlot(
  create_legend("contact")
)

# contact_option
lapply(groups, function(x) {
  output[[glue("contact_option_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "contact_option")
  })
})

output$contact_option_legend <- renderPlot(
  create_legend("contact_option")
)

# contact_other_comment
lapply(groups, function(x) {
  output[[glue("contact_other_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "contact_other_comment", group_select_id = "contact_other_comment_group")
  })
})

# contact_instances
lapply(groups, function(x) {
  output[[glue("contact_instances_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "contact_instances")
  })
})

output$contact_instances_legend <- renderPlot(
  create_legend("contact_instances")
)

# contact_resolved
lapply(groups, function(x) {
  output[[glue("contact_resolved_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "contact_resolved")
  })
})

output$contact_resolved_legend <- renderPlot(
  create_legend("contact_resolved")
)

# contact_not_resolved_comment
lapply(groups, function(x) {
  output[[glue("contact_not_resolved_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "contact_not_resolved_comment", group_select_id = "contact_not_resolved_comment_group")
  })
})

# contact_available
lapply(groups, function(x) {
  output[[glue("contact_available_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "contact_available")
  })
})

output$contact_available_legend <- renderPlot(
  create_legend("contact_available")
)

# contact_not_available_option
lapply(groups, function(x) {
  output[[glue("contact_not_available_option_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "contact_not_available_option")
  })
})

output$contact_not_available_option_legend <- renderPlot(
  create_legend("contact_not_available_option")
)

# contact_not_available_other_comment
lapply(groups, function(x) {
  output[[glue("contact_not_available_other_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "contact_not_available_other_comment", group_select_id = "contact_not_available_other_comment_group")
  })
})

# contact_how_easy_rating
lapply(groups, function(x) {
  output[[glue("contact_how_easy_rating_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "contact_how_easy_rating", cols_start_with = FALSE)
  })
})

output$contact_how_easy_rating_legend <- renderPlot(
  create_legend("contact_how_easy_rating")
)

# contact_how_easy_rating_comment
lapply(groups, function(x) {
  output[[glue("contact_how_easy_rating_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "contact_how_easy_rating_comment", group_select_id = "contact_how_easy_rating_comment_group")
  })
})

# contact_aspects_rating
lapply(groups, function(x) {
  output[[glue("contact_aspects_rating_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "contact_aspects_rating", radio_select_id = "contact_aspects_rating_radio")
  })
})

output$contact_aspects_rating_legend <- renderPlot(
  create_legend("contact_aspects_rating")
)

# comms_aspects_rating
lapply(groups, function(x) {
  output[[glue("comms_aspects_rating_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "comms_aspects_rating", radio_select_id = "comms_aspects_rating_radio")
  })
})

output$comms_aspects_rating_legend <- renderPlot(
  create_legend("comms_aspects_rating")
)

# comms_right_amount
lapply(groups, function(x) {
  output[[glue("comms_right_amount_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "comms_right_amount")
  })
})

output$comms_right_amount_legend <- renderPlot(
  create_legend("comms_right_amount")
)

# comms_reqs_met
lapply(groups, function(x) {
  output[[glue("comms_reqs_met_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "comms_reqs_met")
  })
})

output$comms_reqs_met_legend <- renderPlot(
  create_legend("comms_reqs_met")
)

# comms_reqs_not_met_comment
lapply(groups, function(x) {
  output[[glue("comms_reqs_not_met_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "comms_reqs_not_met_comment", group_select_id = "comms_reqs_not_met_comment_group")
  })
})

# would_improve_onboarding_comment
lapply(groups, function(x) {
  output[[glue("would_improve_onboarding_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "would_improve_onboarding_comment", group_select_id = "would_improve_onboarding_comment_group")
  })
})

# feedback_onboarding_comment
lapply(groups, function(x) {
  output[[glue("feedback_onboarding_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "feedback_onboarding_comment", group_select_id = "feedback_onboarding_comment_group")
  })
})

# nps_onboarding
lapply(groups, function(x) {
  output[[glue("nps_onboarding_{x}")]] <- renderHighchart({
    line_chart(data = filtered_data[[x]], measure_column = "nps_onboarding", measure_type = "NPS")
  })
})

# nes_onboarding
lapply(groups, function(x) {
  output[[glue("nes_onboarding_{x}")]] <- renderHighchart({
    line_chart(data = filtered_data[[x]], measure_column = "nes_onboarding", measure_type = "NES")
  })
})
