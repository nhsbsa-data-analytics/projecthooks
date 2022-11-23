# nps_monthly
lapply(groups, function(x) {
  output[[glue("nps_monthly_{x}")]] <- renderHighchart({
    line_chart(data = filtered_data[[x]], measure_column = "nps", measure_type = "NPS", targets = targets)
  })
})

# nps_comment
lapply(groups, function(x) {
  output[[glue("nps_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "nps_comment", group_select_id = "nps_comment_group")
  })
})

# nes_monthly
lapply(groups, function(x) {
  output[[glue("nes_monthly_{x}")]] <- renderHighchart({
    line_chart(data = filtered_data[[x]], measure_column = "nes", measure_type = "NES", targets = targets)
  })
})

# nes_comment
lapply(groups, function(x) {
  output[[glue("nes_comment_{x}")]] <- renderDataTable({
    comment_table(data = filtered_data[[x]], comment_column = "nes_comment", group_select_id = "nes_comment_group")
  })
})
