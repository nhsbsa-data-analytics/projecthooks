# involved_onboarding
lapply(groups, function(x) {
  output[[glue("involved_onboarding_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "involved_onboarding")
  })
})

# answered_qs_onboarding
lapply(groups, function(x) {
  output[[glue("answered_qs_onboarding_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "answered_qs_onboarding")
  })
})

# onboarding_aspects_rating
lapply(groups, function(x) {
  output[[glue("onboarding_aspects_rating_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "onboarding_aspects_rating", radio_select_id = "Rating")
  })
})

# onboarding_aspects_comment
lapply(groups, function(x) {
  output[[glue("onboarding_aspects_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "onboarding_aspects_comment", group_select_id = "onboarding_aspects_comment_group")
  })
})

# challenge_option
lapply(groups, function(x) {
  output[[glue("challenge_option_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "challenge_option", radio_select_id = "Challenge")
  })
})

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

# contact_option
lapply(groups, function(x) {
  output[[glue("contact_option_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "contact_option", radio_select_id = "Method")
  })
})

# contact_other_comment
lapply(groups, function(x) {
  output[[glue("contact_other_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "contact_other_comment", group_select_id = "contact_other_comment_group")
  })
})

# contact_instances
lapply(groups, function(x) {
  output[[glue("contact_instances_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "contact_instances", radio_select_id = "Number of Contacts")
  })
})

# contact_resolved
lapply(groups, function(x) {
  output[[glue("contact_resolved_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "contact_resolved")
  })
})

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

# contact_not_available_option
lapply(groups, function(x) {
  output[[glue("contact_not_available_option_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "contact_not_available_option", radio_select_id = "Reason")
  })
})

# contact_not_available_other_comment
lapply(groups, function(x) {
  output[[glue("contact_not_available_other_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "contact_not_available_other_comment", group_select_id = "contact_not_available_other_comment_group")
  })
})

# contact_how_easy_rating
lapply(groups, function(x) {
  output[[glue("contact_how_easy_rating_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "contact_how_easy_rating", cols_start_with = FALSE, radio_select_id = "Rating")
  })
})

# contact_how_easy_rating_comment
lapply(groups, function(x) {
  output[[glue("contact_how_easy_rating_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "contact_how_easy_rating_comment", group_select_id = "contact_how_easy_rating_comment_group")
  })
})

# contact_aspects_rating
lapply(groups, function(x) {
  output[[glue("contact_aspects_rating_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "contact_aspects_rating", radio_select_id = "Rating")
  })
})

# comms_aspects_rating
lapply(groups, function(x) {
  output[[glue("comms_aspects_rating_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "comms_aspects_rating", radio_select_id = "Rating")
  })
})

# comms_right_amount
lapply(groups, function(x) {
  output[[glue("comms_right_amount_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "comms_right_amount")
  })
})

# comms_reqs_met
lapply(groups, function(x) {
  output[[glue("comms_reqs_met_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "comms_reqs_met")
  })
})

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
