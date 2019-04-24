var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 21</i>';
states['fold000001'] = false;
texts['fold000023'] = '<a href="javascript:fold(\'fold000023\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 23 to line 29</i>';
states['fold000023'] = false;
texts['fold000031'] = '<a href="javascript:fold(\'fold000031\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 31 to line 40</i>';
states['fold000031'] = false;
texts['fold000043'] = '<a href="javascript:fold(\'fold000043\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 43 to line 43</i>';
states['fold000043'] = false;
texts['fold000045'] = '<a href="javascript:fold(\'fold000045\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 45 to line 47</i>';
states['fold000045'] = false;
texts['fold000049'] = '<a href="javascript:fold(\'fold000049\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 49 to line 58</i>';
states['fold000049'] = false;
texts['fold000061'] = '<a href="javascript:fold(\'fold000061\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 61 to line 61</i>';
states['fold000061'] = false;
texts['fold000063'] = '<a href="javascript:fold(\'fold000063\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 63 to line 73</i>';
states['fold000063'] = false;
texts['fold000075'] = '<a href="javascript:fold(\'fold000075\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 75 to line 89</i>';
states['fold000075'] = false;
texts['fold000091'] = '<a href="javascript:fold(\'fold000091\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 91 to line 120</i>';
states['fold000091'] = false;
texts['fold000122'] = '<a href="javascript:fold(\'fold000122\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 122 to line 123</i>';
states['fold000122'] = false;
texts['fold000129'] = '<a href="javascript:fold(\'fold000129\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 129 to line 133</i>';
states['fold000129'] = false;
texts['fold000135'] = '<a href="javascript:fold(\'fold000135\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 135 to line 141</i>';
states['fold000135'] = false;
texts['fold000145'] = '<a href="javascript:fold(\'fold000145\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 145 to line 158</i>';
states['fold000145'] = false;
texts['fold000164'] = '<a href="javascript:fold(\'fold000164\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 164 to line 169</i>';
states['fold000164'] = false;
texts['fold000171'] = '<a href="javascript:fold(\'fold000171\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 171 to line 172</i>';
states['fold000171'] = false;
texts['fold000177'] = '<a href="javascript:fold(\'fold000177\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 177 to line 183</i>';
states['fold000177'] = false;
texts['fold000187'] = '<a href="javascript:fold(\'fold000187\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 187 to line 188</i>';
states['fold000187'] = false;
texts['fold000192'] = '<a href="javascript:fold(\'fold000192\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 192 to line 194</i>';
states['fold000192'] = false;
texts['fold000196'] = '<a href="javascript:fold(\'fold000196\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 196 to line 198</i>';
states['fold000196'] = false;
texts['fold000200'] = '<a href="javascript:fold(\'fold000200\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 200 to line 200</i>';
states['fold000200'] = false;

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
