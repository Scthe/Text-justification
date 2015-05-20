$(function() {
  var rangeType = 'input[type=range]',
    rangeWrapper = '.range-field',
    rangeMousedown = false,
    thumbHTMLStr = '<span class="thumb"><span class="value center"></span></span>';

  $(rangeType).each(function() {
    var thumb = $(thumbHTMLStr);
    $(this).after(thumb);
  });

  $(document).on("mousedown", rangeWrapper, function(e) {
    rangeMousedown = true;
    updateThumb(e, this);
  });

  $(document).on("mousemove", rangeWrapper, function(e) {
    if (rangeMousedown) {
      updateThumb(e, this);
    }
  });

  $(document).on("mouseup", rangeWrapper, function() {
    rangeMousedown = false;
    $(this).children('.thumb').removeClass('active');
    console.log("out > " + $(this).children('input[type=range]').val());
  });

  $(document).on("mouseout", rangeWrapper, function() {
    if (!rangeMousedown) {
      $(this).children('.thumb').removeClass('active');
    }
  });

  function updateThumb(event, parentEl) {
    var thumb = $(parentEl).children('.thumb');
    if (thumb.length <= 0) {
      thumb = $(thumbHTMLStr);
      $(parentEl).append(thumb);
    }

    var left = event.pageX - $(parentEl).offset().left,
      width = $(parentEl).outerWidth(),
      value = $(parentEl).children('input[type=range]').val();
    left = Math.min(Math.max(0, left), width);
    thumb.addClass('active').css('left', left);
    thumb.find('.value').html(value);

    // console.log(event)
    // console.log(parentEl)
    // console.log(value);
  }

});
