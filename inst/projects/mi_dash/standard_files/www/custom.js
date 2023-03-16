$(function() {

  $(document).on('shiny:connected', function(e) {
    $('.dropdown').blur();
    $('.ui.accordion').accordion();
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
    var page_group = $(this).attr('value');

    $('.item.visible')
      .removeClass('visible')
      .removeClass('transition')
      .addClass('hidden')
      .css('display', '');

    $(page_group).transition('fade down');
  });

  $(document).on('click', '.homeitem', function() {
    var page_group = $(this).parent().parent().prev().attr('value');

    $('.sideheader.item:not(a[value="' + page_group + '"])')
      .next()
      .removeClass('visible')
      .removeClass('transition')
      .addClass('hidden')
      .css('display', '');
  });

  $(window).scroll(function() {
    $(window).trigger('resize');
  });
});
