$(function() {

  $(document).on('shiny:connected', function(e) {
    $('.dropdown').blur();
    $('.ui.accordion').accordion();
  });

  $(document).on('shiny:visualchange', function(event) {
    var obj = event['binding']['el']

    if ($(obj).children('thead').length > 0) {
      if(init_tables[$(obj).attr('id')][1] == 0){
        $('#' + $(obj).attr('id')).DataTable({
          "order": [[init_tables[$(obj).attr('id')][0] , "desc" ]]
        });
        init_tables[$(obj).attr('id')][1] = 1;
      }
    }
  });

  $(document).on('click', '.navigation.item', function() {
    var page = $(this).attr('data-value');

    if (page !== 'survey_download') {
      $('.visible.dashboard')
        .removeClass('visible')
        .addClass('hidden');
      $('.' + $(this).attr('data-value') + '.dashboard')
        .removeClass('hidden')
        .addClass('visible');
      $('#' + $(this).attr('data-value') + 'trigger')
        .trigger('show')
        .show()
        .trigger('shown');

      Shiny.onInputChange($(this).attr('data-value') + 'trigger', 'trigger');

      $(window).scrollTop(0);

      Shiny.setInputValue('page_title', $(this).text());
    }
  });

  $(document).on('click', '.sideheader', function() {
    var tar = $(this).attr('value');

    $('.item.visible')
      .removeClass('visible')
      .removeClass('transition')
      .addClass('hidden').css('display', '');

    $(tar).transition('fade down');
  });

  $(document).on('click', '.homeitem', function() {
    $('.item.visible')
      .removeClass('visible')
      .removeClass('transition')
      .addClass('hidden')
      .css('display', '');
  });

  $(window).scroll(function() {
    $(window).trigger('resize');
  });
});
