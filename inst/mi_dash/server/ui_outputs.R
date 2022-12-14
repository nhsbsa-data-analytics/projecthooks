### Month range controls ###

month_range_gte_3 <- reactive({
  if (month_diff(input$month_range) >= 3) TRUE else FALSE
})


past_3_months <- reactive({
  seq(as.Date(input$month_range[2]), by = "-1 month", length = 3)
})

observeEvent(input$month_range, {
  if (input$month_range[2] < input$month_range[1]) {
    updateDateRangeInput(session, "month_range", start = input$month_range[2])
  }
})

observeEvent(input$all_months, {
  reset("month_range")
  showElement(selector = "#month_range > div > span")
  showElement(selector = "#month_range > div > input:nth-child(1)")
})

observeEvent(input$single_month, {
  updateDateRangeInput(session, "month_range", start = input$month_range[2])
  hideElement(selector = "#month_range > div > span")
  hideElement(selector = "#month_range > div > input:nth-child(1)")
})

observeEvent(input$range_months, {
  showElement(selector = "#month_range > div > span")
  showElement(selector = "#month_range > div > input:nth-child(1)")
})

observeEvent(month_range_gte_3(), {
  if (month_range_gte_3()) {
    hideElement(selector = "#nps_groups_hc")
    hideElement(selector = "#nps_groups_hc_TMR")
    showElement(selector = "#nps_groups_gg")
    showElement(selector = "#nps_groups_gg_TMR")
  } else {
    hideElement(selector = "#nps_groups_gg")
    hideElement(selector = "#nps_groups_gg_TMR")
    showElement(selector = "#nps_groups_hc")
    showElement(selector = "#nps_groups_hc_TMR")
  }
})

output$month_range_text <- renderText({
  month_range_indicator(input$month_range)
})



### Region selection controls ###

# Track saved choices
regions_saved_A <- reactiveVal(value = regions)
regions_saved_B <- reactiveVal(value = NULL)

# Reset button rendering (only shown when there are differences to default)
output$reset_regions_btn <- renderUI({
  status <- ifelse(
    identical(input$regions_cb_A, regions) &&
      identical(input$regions_cb_B, NULL),
    "light", "success"
  )

  actionButton(
    inputId = "reset_regions",
    label = "Reset",
    class = paste0("btn btn-", status)
  )
})

# Functionality of reset button
observeEvent(
  input$reset_regions,
  {
    updatePrettyCheckboxGroup(session, "regions_cb_A", selected = regions)
    updatePrettyCheckboxGroup(session, "regions_cb_B", selected = character(0))

    addClass(selector = "#regions_cb_A i", class = "fa-check")
    removeClass(selector = "#regions_cb_A i", class = "fa-times")
    removeClass(selector = "#regions_cb_B i", class = "fa-check")
    addClass(selector = "#regions_cb_B i", class = "fa-times")
  }
)

# Save button rendering. Since the button is destroyed and recreated whenever a
# checkbox is clicked, the script needs to be run so the event observer is bound
# to the new button.
output$save_regions_btn <- renderUI({
  status <- ifelse(
    identical(input$regions_cb_A, regions_saved_A()) &&
      identical(input$regions_cb_B, regions_saved_B()),
    "light", "success"
  )

  tagList(
    actionButton(
      inputId = "save_regions",
      label = "Save",
      class = paste0("btn btn-", status)
    ),
    tags$script(HTML("
      $('#save_regions').click(function () {
        var regions_saved_A = [];
        $.each($('#regions_cb_A input:checkbox:checked'), function(){
          regions_saved_A.push($(this).val());
        });

        var regions_saved_B = [];
        $.each($('#regions_cb_B input:checkbox:checked'), function(){
          regions_saved_B.push($(this).val());
        });

        tippy_regions_A.setContent(
          'Selected regions in group A:<br>' + regions_saved_A.join('<br>')
        );

        tippy_regions_B.setContent(
          'Selected regions in group B:<br>' + regions_saved_B.join('<br>')
        );
      });
    "))
  )
})

# Update groupA and groupB classes according to whether the B column should be shown.
update_A_B_columns <- function(x) {
  if (x == 0) {
    removeClass(selector = ".groupA", class = "col-sm-6")
    addClass(selector = ".groupA", class = "col-sm-12")
    removeClass(selector = ".groupB", class = "col-sm-6")
    addClass(selector = ".groupB", class = "hidden")
    runjs("$(window).trigger('resize')")
  } else {
    removeClass(selector = ".groupA", class = "col-sm-12")
    addClass(selector = ".groupA", class = "col-sm-6")
    removeClass(selector = ".groupB", class = "hidden")
    addClass(selector = ".groupB", class = "col-sm-6")
    showElement(selector = ".groupB")
  }
}

# Functionality of save button
observeEvent(
  input$save_regions,
  {
    regions_saved_A(input$regions_cb_A)
    regions_saved_B(input$regions_cb_B)

    update_A_B_columns(length(regions_saved_B()))
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE
)

# Save on closing the dropmenu. We observe for the appearance of the dropmenu,
# using the MutationObserver API. See the link for details:
# https://www.smashingmagazine.com/2019/04/mutationobserver-api-guide/
# Based on the observed state, a Shiny input is set, which in turn is observed on
# the server.
runjs("
  let mRegionDropmenu = document.getElementById('region_dropmenu_dropmenu'),
  options = {
    childList: true
  },

  observer = new MutationObserver(mCallback);

  function mCallback (mutations) {
    if ($('#tippy-5').is(':visible')) {
      Shiny.setInputValue('regions_dropmenu_has_closed', false)
    } else {
      Shiny.setInputValue('regions_dropmenu_has_closed', true)
    }
  }

  observer.observe(mRegionDropmenu, options);
")

observeEvent(input$regions_dropmenu_has_closed, {
  if (input$regions_dropmenu_has_closed) {
    regions_saved_A(input$regions_cb_A)
    regions_saved_B(input$regions_cb_B)

    update_A_B_columns(length(regions_saved_B()))

    runjs("
      var regions_saved_A = [];
      $.each($('#regions_cb_A input:checkbox:checked'), function(){
        regions_saved_A.push($(this).val());
      });

      var regions_saved_B = [];
      $.each($('#regions_cb_B input:checkbox:checked'), function(){
        regions_saved_B.push($(this).val());
      });

      tippy_regions_A.setContent(
        'Selected regions in group A:<br>' + regions_saved_A.join('<br>')
      );

      tippy_regions_B.setContent(
        'Selected regions in group B:<br>' + regions_saved_B.join('<br>')
      );

      tippy_regions_A.enable();
      tippy_regions_B.enable();
    ")
  } else {
    runjs("
      tippy_regions_A.disable();
      tippy_regions_B.disable();
    ")
  }
})
