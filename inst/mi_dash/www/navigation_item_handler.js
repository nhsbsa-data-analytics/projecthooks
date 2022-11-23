$(function() {

  $(document).on('click', '.navigation.item', function() {

    var page = $(this).attr('data-value');

    $('.visible.dashboard').removeClass('visible').addClass('hidden');
    $('.' + $(this).attr('data-value') + '.dashboard').removeClass('hidden').addClass('visible');
    $('#' + $(this).attr('data-value') + 'trigger').trigger('show').show().trigger('shown');

    Shiny.onInputChange($(this).attr('data-value') + 'trigger', 'trigger');

    window.scrollTo(0, 0);

  });

});
