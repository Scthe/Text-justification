$(function() {
  var range_type = 'input[type=range]';
  var range_wrapper = '.range-field';
  var range_mousedown = false;

  $(range_type).each(function() {
    var thumb = $('<span class="thumb"><span class="value center"></span></span>');
    $(this).after(thumb);
  });

  $(document).on("mousedown", range_wrapper, function(e) {
    var thumb = $(this).children('.thumb');
    if (thumb.length <= 0) {
      thumb = $('<span class="thumb"><span class="value center"></span></span>');
      $(this).append(thumb);
    }
    range_mousedown = true;
    $(this).addClass('active');
    var left = e.pageX - $(this).offset().left;
    var width = $(this).outerWidth();
    left = Math.min(Math.max(0, left), width);
    thumb.addClass('active').css('left', left);
    thumb.find('.value').html($(this).children('input[type=range]').val());
  });

  $(document).on("mouseup", range_wrapper, function() {
    range_mousedown = false;
    $(this).removeClass('active');
    // console.log("> "+$(this).children('input[type=range]').val());
  });

  $(document).on("mousemove", range_wrapper, function(e) {
    var thumb = $(this).children('.thumb');
    if (range_mousedown) {
      var left = e.pageX - $(this).offset().left,
        width = $(this).outerWidth(),
        value = $(this).children('input[type=range]').val();
      // console.log(value);
      left = Math.min(Math.max(0, left), width);
      thumb.addClass('active').css('left', left);
      thumb.find('.value').html(value);
    }
  });

  $(document).on("mouseout", range_wrapper, function() {
    if (!range_mousedown) {
      var thumb = $(this).children('.thumb');
      thumb.removeClass('active');
    }
  });
});
