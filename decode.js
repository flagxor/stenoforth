#! /usr/bin/env node

var fs = require('fs');

const IMMEDIATE_FLAG = 0x01;
const REDIRECT_FLAG = 0x02;
const HIDDEN_LIST_FLAG = 0x10;
const NEWLINE_FLAG = 0x20;
const INDENT_AFTER_FLAG = 0x40;
const UNINDENT_BEFORE_FLAG = 0x80;

const DOCOL = 2;

var RAW = false;

var u8 = new Uint8Array(fs.readFileSync(process.argv[2]));
var i32 = new Int32Array(u8.buffer);

function ReadEntry(xt) {
  var link = i32[(xt>>2) - 3];
  var advance = i32[(xt>>2) - 2];
  var count = u8[xt - 4];
  var flags = u8[xt - 4 + 1];
  var offset = u8[xt - 4 + 2];
  var enslot = u8[xt - 4 + 3];
  var padding = (count + 3) & ~3;
  var name_start = xt - 4 * 3 - padding;
  var name = '';
  for (var k = 0; k < count; ++k) {
    name += String.fromCharCode(u8[name_start + k]);
  }
  return {
    count: count,
    flags: flags,
    offset: offset,
    enslot: enslot,
    link: link ? xt - 4 * 3 + link * 4 : 0,
    advance: advance ? xt - 4 * 2 + advance * 4 : 0,
    code: xt,
    opcode: i32[(xt>>2)],
    name: name,
  };
}

function Indent(n) {
  var ret = '';
  for (var i = 0; i < n; ++i) {
    ret += '   ';
  }
  return ret;
}

function Scan() {
  var revdict = {};
  var dict = {};
  var words = [];

  var i = i32[2] * 4 + 8;  // Get first

  for (;;) {
    var entry = ReadEntry(i);
    revdict[entry.code] = entry;
    dict[entry.name] = entry;
    words.push(entry);
    if (!entry.advance) {
      break;
    }
    i = entry.advance;
  }

  for (var j = 0; j < words.length; ++j) {
    var entry = words[j];
    var code = entry.code;
    if (entry.opcode == DOCOL) {
      if (RAW) {
        console.log(entry.name + ' [' + i32[entry.code>>2] +
            '] at ' + entry.code.toString(16));
      }
      var indent = 0;
      var items = '';

      items += ': ' + entry.name + '\n' + Indent(++indent);
      for (;;) {
        code += 4;
        var v = i32[code>>2];
        var addr = code + v * 4;
        if (u8[addr - 4 + 1] & REDIRECT_FLAG) {  // handle redirect
          addr = addr - u8[addr - 4 + 2] * 4 - 4;
        }
        var lookup = revdict[addr];
        if (lookup === undefined) {
          items += '#' + v + ' ';
          continue;
        }

        if (!RAW && lookup.enslot != 255) {
          code += lookup.enslot * 4;
        }

        // Handle indent
        if (lookup.flags & UNINDENT_BEFORE_FLAG) {
          items += '\n' + Indent(--indent);
        } else if (lookup.flags & INDENT_AFTER_FLAG) {
          items += '\n' + Indent(indent);
        }

        if (!RAW && lookup.name == 'DOLIT') {
          items += i32[code>>2] + ' ';
          continue;
        } if (!RAW && lookup.name == 'RUNSCODE>') {
          items += lookup.name + ' ';
          items += i32[code>>2] + ' ';
        } else if (lookup.name == 'COMPILE') {
          items += lookup.name + ' ';
          code += 4;
          v = i32[code>>2];
          addr = code + v * 4;
          lookup = revdict[addr];
          items += lookup.name + ' ';
        } else if (lookup.enslot == 255) {
          code += 4;
          var len = i32[code>>2];
          code += 4;
          var s = '';
          for (var i = 0; i < len; ++i) {
            s += String.fromCharCode(u8[code++]);
          }
          code = (code + 3) & ~3;
          items += lookup.name + ' ' + s + (lookup.name == '(' ? ') ' : '" ');
          code -= 4;
        } else if (lookup.name == ';') {
          items += ';';
          if (entry.flags & IMMEDIATE_FLAG) {
            items += ' IMMEDIATE';
          }
          if (entry.enslot) {
            if (entry.slot == 255) {
              items += ' VARISLOT';
            } else {
              items += ' ' + entry.enslot + ' ENSLOT';
            }
          }
          if (entry.flags & UNINDENT_BEFORE_FLAG) {
            items += ' -TAB';
          }
          if (entry.flags & INDENT_AFTER_FLAG) {
            items += ' +TAB';
          }
          if (entry.flags & NEWLINE_FLAG) {
            items += ' LIST-NEWLINE';
          }
          if (entry.flags & HIDDEN_LIST_FLAG) {
            items += ' HIDDEN-LIST';
          }
          items += '\n';
          break;
        } else {
          items += lookup.name + ' ';
        }

        // Handle indent
        if (lookup.flags & INDENT_AFTER_FLAG) {
          items += '\n' + Indent(++indent);
        } else if (lookup.flags & UNINDENT_BEFORE_FLAG) {
          items += '\n' + Indent(indent);
        } else if (lookup.flags & NEWLINE_FLAG) {
          items += '\n' + Indent(indent);
        }
      }
      items = items.replace(/\n[ ]+\n/g, '\n');
      console.log(items);
    } else if (dict['DOVAR'] && entry.opcode == dict['DOVAR'].opcode && entry.name != 'DOVAR') {
      console.log('VARIABLE ' + entry.name + '\n');
    } else if (dict['DOCON'] && entry.opcode == dict['DOCON'].opcode && entry.name != 'DOCON') {
      console.log(i32[(entry.code>>2) + 1] + ' CONSTANT ' + entry.name + '\n');
    } else {
      console.log(entry.name + ' [' + i32[entry.code>>2] +
          '] at ' + entry.code.toString(16));
    }
  }
}

Scan();

