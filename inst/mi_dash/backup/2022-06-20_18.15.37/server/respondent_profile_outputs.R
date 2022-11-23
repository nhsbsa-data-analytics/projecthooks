# training_option
lapply(groups, function(x) {
  output[[glue("training_option_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "training_option", legend_title = "Training")
  })
})

# training_ease_rating
lapply(groups, function(x) {
  output[[glue("training_ease_rating_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "training_ease_rating", legend_title = "Rating")
  })
})

# training_ease_comment
lapply(groups, function(x) {
  output[[glue("training_ease_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "training_ease_comment", group_select_id = "training_ease_comment_group")
  })
})

# organisation
lapply(groups, function(x) {
  output[[glue("organisation_{x}")]] <- renderHighchart({
    horizontal_bar(data = filtered_data[[x]], columns = "organisation")
  })
})

# organisation_other_comment
lapply(groups, function(x) {
  output[[glue("organisation_other_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "organisation_other_comment", comment_name_in_DT = "Organisation")
  })
})

# region
lapply(groups, function(x) {
  output[[glue("region_{x}")]] <- renderHighchart({
    horizontal_bar(data = filtered_data[[x]], columns = "region")
  })
})

# region_other_comment
lapply(groups, function(x) {
  output[[glue("region_other_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "region_other_comment", comment_name_in_DT = "Region")
  })
})

# role
lapply(groups, function(x) {
  output[[glue("role_{x}")]] <- renderHighchart({
    horizontal_bar(data = filtered_data[[x]], columns = "role")
  })
})

# role_other_comment
lapply(groups, function(x) {
  output[[glue("role_other_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "role_other_comment", comment_name_in_DT = "Role")
  })
})

# frequency_used
lapply(groups, function(x) {
  output[[glue("frequency_used_{x}")]] <- renderHighchart({
    horizontal_bar(data = filtered_data[[x]], columns = "frequency_used")
  })
})

# how_long_used
lapply(groups, function(x) {
  output[[glue("how_long_used_{x}")]] <- renderHighchart({
    horizontal_bar(data = filtered_data[[x]], columns = "how_long_used")
  })
})

# # num_of_vacancies
# lapply(groups, function(x) {
#   output[[glue("num_of_vacancies_{x}")]] <- renderCustom({
#     {CUSTOM_FUNC}()
#   })
# })

# system_used
lapply(groups, function(x) {
  output[[glue("system_used_{x}")]] <- renderHighchart({
    pie(data = filtered_data[[x]], question_column = "system_used")
  })
})

# system_used_option
lapply(groups, function(x) {
  output[[glue("system_used_option_{x}")]] <- renderPlotly({
    stacked_vertical(data = filtered_data[[x]], columns = "system_used_option", legend_title = "System")
  })
})

# system_used_other_comment
lapply(groups, function(x) {
  output[[glue("system_used_other_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "system_used_other_comment", comment_name_in_DT = "System")
  })
})
