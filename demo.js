// keycodes that trigger special behavior

var Key = {
  CTRL:  17,
  DEL:   8,
  DOWN:  40,
  ENTER: 13,
  ESC:   27,
  LEFT:  37,
  RIGHT: 39,
  SPACE: 32,
  TAB:   9,
  UP:    38
};

// selection management

var $selected = $('.selected');

function select($elem) {
  $selected.removeClass('selected');
  $selected = $elem;
  $selected.addClass('selected');
}

function exists($elem) {
  return $elem.length !== 0;
}

// edit mode managament

var editMode = false;

function enableEditMode() {
  if ($selected.hasClass('token')) {
    editMode = true;
    $selected.attr('contenteditable', true);
    $selected.get(0).focus();
  } else if ($selected.hasClass('coll')) {
    goDown();
    enableEditMode();
  }
}

function disableEditMode() {
  if ($selected.hasClass('token')) {
    editMode = false;
    $selected.removeAttr('contenteditable');
    $selected.get(0).blur();
    if ($selected.text() === '') {
      deleteSelected();
    }
  }
}

// navigation commands

function goDown() {
  if ($selected.hasClass('coll')) {
    var $items = $selected.children('.items');
    if (exists($items)) {
      select($items.children().first());
    }
  }
}

function goUp() {
  var $parent = $selected.parent().parent();
  if ($parent.hasClass('coll')) {
    select($parent);
  }
}

function goLeft() {
  if ($selected.hasClass('top')) {
    return;
  }
  var $prev = $selected.prev();
  if (exists($prev)) {
    select($prev);
  } else {
    var $last = $selected.siblings().last();
    if (exists($last)) {
      select($last);
    }
  }
}

function goRight() {
  if ($selected.hasClass('top')) {
    return;
  }
  var $next = $selected.next();
  if (exists($next)) {
    select($next);
  } else {
    var $first = $selected.siblings().first();
    if (exists($first)) {
      select($first);
    }
  }
}

function walkDown() {
  while ($selected.hasClass('coll')) {
    if (!exists($selected.children('.items'))) {
      break;
    }
    goDown();
  }
}

function walkRight() {
  if ($selected.hasClass('top')) {
    return;
  }
  var $next = $selected.next();
  if (exists($next)) {
    select($next);
  } else {
    goUp();
    goRight();
    walkDown();
  }
}

// editing commands

function insertToken() {
  $selected.after('<span class="token"></span>');
  goRight();
  enableEditMode();
}

function deleteSelected() {
  var $deleted = $selected;
  goRight();
  if ($selected === $deleted) {
    goUp();
  }
  $deleted.remove();
}

// key event handling

function getKey(keyEvent) {
  return keyEvent.key || keyEvent.keyCode || keyEvent.which;
}

$(window).keydown(function(ev) {
  var key = getKey(ev);
  if (editMode) {
    switch (key) {
      case Key.ENTER:
      case Key.ESC:
      case Key.UP:
        disableEditMode();
        ev.preventDefault();
        break;
      case Key.TAB:
        disableEditMode();
        ev.preventDefault();
        walkRight();
        enableEditMode();
        break;
    }
  } else {
    switch (key) {
      case Key.LEFT:  goLeft();  break;
      case Key.UP:    goUp();    break;
      case Key.RIGHT: goRight(); break;
      case Key.DOWN:  goDown();  break;
      case Key.DEL:
        ev.preventDefault();
        deleteSelected();
        break;
      case Key.ENTER:
        ev.preventDefault();
        enableEditMode();
        break;
      case Key.SPACE:
        ev.preventDefault();
        insertToken();
        break;
    }
  }
});
