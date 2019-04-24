var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 8</i>';
states['fold000001'] = false;
texts['fold000010'] = '<a href="javascript:fold(\'fold000010\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 10 to line 36</i>';
states['fold000010'] = false;
texts['fold000038'] = '<a href="javascript:fold(\'fold000038\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 38 to line 55</i>';
states['fold000038'] = false;
texts['fold000057'] = '<a href="javascript:fold(\'fold000057\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 57 to line 65</i>';
states['fold000057'] = false;
texts['fold000067'] = '<a href="javascript:fold(\'fold000067\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 67 to line 183</i>';
states['fold000067'] = false;
texts['fold000188'] = '<a href="javascript:fold(\'fold000188\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 188 to line 229</i>';
states['fold000188'] = false;
texts['fold000231'] = '<a href="javascript:fold(\'fold000231\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 231 to line 921</i>';
states['fold000231'] = false;

function fold(id) {
  tmp = document.getElementById(id).innerHTML;
  document.getElementById(id).innerHTML = texts[id];
  texts[id] = tmp;
  states[id] = !(states[id]);
}

function unfoldAll() {
  for (key in states) {
    if (states[key]) {
      fold(key);
    }
  }
}

function foldAll() {
  for (key in states) {
    if (!(states[key])) {
      fold(key);
    }
  }
}
