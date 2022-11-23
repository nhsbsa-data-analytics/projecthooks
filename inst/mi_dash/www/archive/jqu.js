$(function() {

  $(document).on('shiny:connected', function(e) {

    $('.dropdown').blur();

    $('.ui.accordion').accordion();

});

$(document).on('shiny:connected', function(e) {

  $('.dropdown').blur();

  $('.ui.accordion').accordion();

});

var init_tables = {
  lsf_about_your_course_table_1: [1, 0],
  lsf_about_your_course_table_2: [1, 0],
  lsf_satisfaction_comments: [0, 0],
  lsf_finding_out_table_1: [1, 0],
  lsf_finding_out_table_2: [1, 0],
  lsf_finding_out_table_3: [1, 0],
  lsf_finding_out_table_4: [1, 0],
  lsf_finding_out_more_table_1: [1, 0],
  cda_page_child_table: [1, 0],
  cda_page_comment_table: [0, 0],
  tdae_whynotapply_table: [1, 0],
  tdae_comments: [0, 0],
  lsf_financial_whynot: [1, 0],
  lsf_esf_didnt_table: [1, 0],
  lsf_esf_comments: [0, 0],
  lsf_needs_table: [0, 0]
}

$(document).on('shiny:visualchange', function(event) {

  var obj = event['binding']['el']

  if ($(obj).children('thead').length > 0) {

    if(init_tables[$(obj).attr('id')][1] == 0){

      $('#' + $(obj).attr('id')).DataTable({
        "order": [[init_tables[$(obj).attr('id')][0] , "desc" ]]
      });

      init_tables[$(obj).attr('id')][1] = 1

    }

  }

});

$(document).on('click', '.navigation.item', function() {

  var page = $(this).attr('data-value')

  $('.visible.dashboard').removeClass('visible').addClass('hidden')
  $('.' + $(this).attr('data-value') + '.dashboard').removeClass('hidden').addClass('visible')
  $('#' + $(this).attr('data-value') + 'trigger').trigger('show').show().trigger('shown');

  Shiny.onInputChange($(this).attr('data-value') + 'trigger', 'trigger');
  
  if(['summary','ehicsat', 'eligibility', 'exemption', 'ehicac', 'ehicae', 'liscsat', 'lisdigi', 'lisac', 'lisae', 'matexcsat', 'matexcsat', 'matexac', 'matexae', 'medexcsat', 'medexac', 'medexae', 'ppccsat', 'ppcdigi1', 'ppcdigi2', 'ppcac', 'ppcae', 'tccsat', 'tcac', 'tcae'].includes(page)){

          $(window).scrollTop(0);


        }

});

$(document).on('click', '.sideheader', function() {

  /*$('.item.visible').removeClass('visible').addClass('hidden');

  $($(this).attr('value')).removeClass('hidden').addClass('visible');*/

  var tar = $(this).attr('value')

  $('.item.visible').removeClass('visible').removeClass('transition').addClass('hidden').css('display', '');

  $(tar).transition('fade down')


});

$(document).on('click', '.homeitem', function() {

  $('.item.visible').removeClass('visible').removeClass('transition').addClass('hidden').css('display', '');

});


$(window).scroll(function() {
  $(window).trigger('resize');
});

});
