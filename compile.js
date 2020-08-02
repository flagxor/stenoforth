#! /usr/bin/env nodejs

var fs = require('fs');
var process = require('process');

const IMMEDIATE_FLAG = 0x01;
const REDIRECT_FLAG = 0x02;
const HIDDEN_LIST_FLAG = 0x10;
const NEWLINE_FLAG = 0x20;
const INDENT_AFTER_FLAG = 0x40;
const UNINDENT_BEFORE_FLAG = 0x80;

// Load opcodes
var opsFile = fs.readFileSync(process.argv[2]).toString().split('\n');
var ops = {};
for (var i = 0; i < opsFile.length; ++i) {
  var parts = opsFile[i].split(' ');
  if (parts.length != 2) {
    continue;
  }
  ops[parts[1]] = parts[0];
}

var heap = new ArrayBuffer(1024 * 1024);
var i32 = new Int32Array(heap);
var u8 = new Uint8Array(heap);
var cp = 0;
var first = undefined;
var last = undefined;
var last_name = undefined;
var tos = undefined;
var dict = {};
var fixups = [];
var deferred = {};

function Aligned() {
  cp = (cp + 3) & ~3;
}

function Comma(n) {
  i32[cp>>2] = n;
  cp += 4;
}

function Target(addr) {
  Comma((addr - cp) >> 2);
}

function Char(ch) {
  u8[cp++] = ch;
}

function Str(s) {
  for (var i = 0; i < s.length; ++i) {
    Char(s.charCodeAt(i));
  }
  Aligned();
}

function Header(name) {
  name = name.toUpperCase();
  if (dict[name] !== undefined) {
    throw Where('Duplicate word');
  }
  Str(name);
  if (last !== undefined) {
    Target(last);  // LINK
  } else {
    Comma(0);  // LINK
  }
  var t = cp;
  Comma(0);  // ADVANCE
  Comma(name.length);
  SetAdvance(last, cp);
  last = cp;
  if (first === undefined) {
    first = cp;
  }
  dict[name] = cp;
  last_name = name;
}

function Find(name) {
  return dict[name.toUpperCase()];
}

function Compile(word) {
  var x = Find(word);
  if (deferred[word.toUpperCase()]) {
    fixups.push([cp, word.toUpperCase()]);
    Comma(0);
  } else if (x !== undefined) {
    Target(x);
  } else {
    var n = parseInt(word);
    if (isNaN(n)) {
      throw Where('Bad compiled word');
    }
    Compile('DOLIT');
    Comma(n);
  }
}

// Load sources
var source_index = 3;
var source_offset = 0;
var source_line = 0;
var source = '';
var last_word = '';

function SetRel(addr, value) {
  i32[addr>>2] = ((value - addr) >> 2);
}

function SetAdvance(xt, value) {
  SetRel(xt - 2 * 4, value);
}

function SetVariable(name, value) {
  SetRel(Find(name) + 4, value);
}

function OrFlags(xt, value) {
  u8[xt - 4 + 1] |= value;
}

function SetOffset(xt, value) {
  u8[xt - 4 + 2] = value;
}

function SetEnslot(xt, value) {
  u8[xt - 4 + 3] |= value;
}

function Gobble(ch) {
  var ret = '';
  while (!source.substr(source_offset, 1).match(ch) && source_offset < source.length) {
    ret += source.substr(source_offset, 1);
    if (source.substr(source_offset, 1) == '\n') {
      ++source_line;
    }
    ++source_offset;
  }
  return ret;
}

function Word(ch) {
  if (ch === undefined) {
    Gobble(/[^ \n\r\t]/);
    ch = /[ \n\r\t]/;
  }
  for (;;) {
    if (source_offset == source.length) {
      if (source_index == process.argv.length) {
        last_word = '(EOF)';
        return undefined;
      }
      source = fs.readFileSync(process.argv[source_index++]).toString();
      source_line = 1;
      source_offset = 0;
    }
    var ret = Gobble(ch);
    if (source_offset < source.length) {
      if (source.substr(source_offset, 1) == '\n') {
        ++source_line;
      }
      ++source_offset;
    }
    if (ret == '') {
      continue;
    }
    last_word = ret;
    return ret;
  }
}

function Where(msg) {
  return msg + ' on ' + last_word + ' at ' + process.argv[source_index - 1] + ':' + source_line;
}

function Build() {
  function FromHere() {
    Target(control.pop());
  }

  function ToHere() {
    var addr = control.pop();
    i32[addr>>2] = (cp - addr) / 4;
  }

  Comma(0);  // Length of the file
  Comma(0);  // Place to start (boot word)
  Comma(0);  // Link to xt of first word

  // Add Opcodes
  for (var word in ops) {
    if (['DOCOL', 'DOLIT'].includes(word)) {
      continue;
    }
    Header(word);
    Comma(ops[word]);
    if (word == '(') {
      SetEnslot(last, -1);
      OrFlags(last, NEWLINE_FLAG);
    } else if (word == 'RUNSCODE>') {
      SetEnslot(last, 2);
    } else if (word.substr(word.length - 1) == '!') {
      OrFlags(last, NEWLINE_FLAG);
    }
  }

  var compiling = false;
  var control = [];
  for (;;) {
    var word = Word();
    if (word === undefined) {
      break;
    }
    word = word.toUpperCase();
    if (word == '(') {
      var s = Word(/[\)]/);
      if (compiling) {
        Compile('(');
        Comma(s.length);
        Str(s);
      }
      continue;
    }
    if (compiling) {
      if (['COMPILE', "[']"].includes(word)) {
        Compile(word);
        Compile(Word());
      } else if (word == '[CHAR]') {
        Compile(word);
        Comma(Word().charCodeAt(0));
      } else if (word == 'LISTS>') {
        Compile(word);
        Comma(ops['DOCOL']);
      } else if (word == ';') {
        Compile(';');
        compiling = false;
      } else if (word.match(/["]$/)) {
        Compile(word);
        var s = Word(/["]/);
        Comma(s.length);
        Str(s);
      } else if (['BEGIN', 'FOR'].includes(word)) {
        Compile(word);
        control.push(cp);
      } else if (['AGAIN', 'UNTIL', 'NEXT'].includes(word)) {
        Compile(word);
        FromHere();
      } else if (word == 'WHILE') {
        Compile('WHILE');
        var k = control.pop();
        control.push(cp);
        Comma(0);
        control.push(k);
      } else if (word == 'REPEAT') {
        Compile('REPEAT');
        FromHere();
        ToHere();
      } else if (word == 'IF') {
        Compile('IF');
        control.push(cp);
        Comma(0);
      } else if (word == 'ELSE') {
        Compile('ELSE');
        var h = cp;
        Comma(0);
        ToHere();
        control.push(h);
      } else if (word == 'THEN') {
        Compile('THEN');
        ToHere();
      } else if (word == 'AFT') {
        control.pop();
        Compile('AFT');
        var h = cp;
        Comma(0);
        control.push(cp);
        control.push(h);
      } else if (['RUNSCODE>', 'RUNS>'].includes(word)) {
        Compile(word);
        var dist = (cp - last) >> 2;
        if (dist < 0 || dist > 255) {
          throw Where('Redirect out of range');
        }
        Comma((REDIRECT_FLAG << 8) | (dist << 16));
        SetOffset(last, dist);
        dict[last_name] = cp;
        if (word == 'RUNSCODE>') {
          Comma(ops[Word()]);
        } else {
          Comma(ops['DORUN']);
        }
      } else {
        // NEEDED?
        if (word.substr(0, 3) == 'OP_' && ops[word.substr(3)] !== undefined) {
          Compile('DOLIT');
          Comma(ops[word.substr(3)]);
          continue;
        }
        Compile(word);
      }
    } else {
      if (word == ':') {
        compiling = true;
        Header(Word());
        Comma(ops['DOCOL']);
      } else if (word == 'FORWARD') {
        deferred[Word()] = 1;
      } else if (word == 'IMMEDIATE') {
        OrFlags(last, IMMEDIATE_FLAG);
      } else if (word == '-TAB') {
        OrFlags(last, UNINDENT_BEFORE_FLAG);
      } else if (word == '+TAB') {
        OrFlags(last, INDENT_AFTER_FLAG);
      } else if (word == 'LIST-NEWLINE') {
        OrFlags(last, NEWLINE_FLAG);
      } else if (word == 'HIDDEN-LIST') {
        OrFlags(last, HIDDEN_LIST_FLAG);
      } else if (word == 'VARISLOT') {
        SetEnslot(last, -1);
      } else if (word == 'ENSLOT') {
        if (tos === undefined) {
          throw Where('Stack underflow');
        }
        SetEnslot(last, tos);
        tos = undefined;
      } else if (word == 'CONSTANT') {
        if (isNaN(tos)) {
          throw Where('Stack underflow');
        }
        Header(Word());
        Comma(ops['DOCON']);
        Comma(tos);
        tos = undefined;
      } else if (word == 'VARIABLE') {
        Header(Word());
        Comma(ops['DOVAR']);
        Comma(0);
        Comma(0);
      } else if (word == 'ALLOT') {
        cp += tos;
        if (tos === undefined) {
          throw Where('Stack underflow');
        }
        tos = undefined;
        Aligned();
      } else {
        var n = parseInt(word);
        if (isNaN(n)) {
          throw Where('Bad interpretered word');
        }
        if (tos !== undefined) {
          throw Where('Stack overflow');
        }
        tos = n;
      }
    }
  }

  for (var i = 0; i < fixups.length; i++) {
    SetRel(fixups[i][0], Find(fixups[i][1]));
  }

  SetRel(0, cp);
  SetRel(4, Find('BOOT'));
  SetRel(8, first);
  SetVariable('FIRST', first);
  SetVariable('CONTEXT', last);
  SetVariable('LAST', last);

  process.stdout.write(u8.slice(0, cp));
}

Build();

