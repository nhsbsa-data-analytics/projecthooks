# important_feature_comment
lapply(groups, function(x) {
  output[[glue("important_feature_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "important_feature_comment", group_select_id = "important_feature_comment_group")
  })
})

# worked_well_comment
lapply(groups, function(x) {
  output[[glue("worked_well_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "worked_well_comment", group_select_id = "worked_well_comment_group")
  })
})

# would_improve_comment
lapply(groups, function(x) {
  output[[glue("would_improve_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "would_improve_comment", group_select_id = "would_improve_comment_group")
  })
})
