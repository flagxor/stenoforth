#! /usr/bin/env nodejs

var fs = require('fs');
var ops = fs.readFileSync(process.argv[2]).toString();
var boot = new Uint8Array(fs.readFileSync(process.argv[3]));

var lines = ops.split('\n');

function ReplaceAll(haystack, needle, replacement) {
  for (;;) {
    var old = haystack;
    haystack = haystack.replace(needle, replacement);
    if (old === haystack) {
      return haystack;
    }
  }
}

console.log(`
const HEAP_SIZE = (1024 * 1024);
const DSTACK_SIZE = 4096;
const RSTACK_SIZE = 4096;

function Interpreter(stdlib, foreign, heap) {
  "use asm";

  var imul = stdlib.Math.imul;

  var exit = foreign.exit;
  var emit = foreign.emit;
  var qkey = foreign.qkey;
  var color = foreign.color;
  var print_decimal = foreign.print_decimal;
  var print_hexadecimal = foreign.print_hexadecimal;

  var u8 = new stdlib.Uint8Array(heap);
  var i32 = new stdlib.Int32Array(heap);

  function run(initrp) {
    initrp = initrp | 0;
    var tos = 0;
    var ip = 0;
    var sp = 0;
    var rp = 0;
    var w = 0;
    var t = 0;
    var ir = 0;
    rp = initrp;
    ip = i32[rp>>2]|0; rp = (rp - 4)|0;
    sp = i32[rp>>2]|0; rp = (rp - 4)|0;
    tos = i32[sp>>2]|0; sp = (sp - 4)|0;
    for (;;) {
      w = i32[ip>>2]|0;
      for (;;) {
        ir = i32[((ip + (w<<2))|0)>>2]|0;
        ip = (ip + 4)|0;
        switch (ir & 0xff) {
`);

for (var i = 0; i < lines.length; ++i) {
  var line = lines[i];
  line = ReplaceAll(line, 'tos *= *sp;', 'tos = imul(tos, *sp);');
  line = line.replace(/[>][>]0/g , '>>>0');
  line = ReplaceAll(line, 'ip + w', '(ip + (w << 2))|0');
  line = ReplaceAll(line, 'ip + *ip', '((ip + (i32[ip>>2] << 2))|0)');
  line = ReplaceAll(line, '++ip;', 'ip = (ip + 4)|0;');
  line = ReplaceAll(line, '++sp;', 'sp = (sp + 4)|0;');
  line = ReplaceAll(line, '++rp;', 'rp = (rp + 4)|0;');
  line = ReplaceAll(line, '--ip', 'ip = (ip - 4)|0;');
  line = ReplaceAll(line, '--sp;', 'sp = (sp - 4)|0;');
  line = ReplaceAll(line, '--rp', 'rp = (rp - 4)|0;');
  line = ReplaceAll(line, '*(cell_t *) tos = ', 'i32[tos>>2] = ');
  line = ReplaceAll(line, '*(int32_t *) tos = ', 'i32[tos>>2] = ');
  line = ReplaceAll(line, '*(uint8_t *) tos = ', 'u8[tos] = ');
  line = ReplaceAll(line, '*(cell_t *) tos', '(i32[tos>>2]|0)');
  line = ReplaceAll(line, '*(int32_t *) tos', '(i32[tos>>2]|0)');
  line = ReplaceAll(line, '*(uint8_t *) tos', '(u8[tos]|0)');
  line = ReplaceAll(line, '*(cell_t *) w', '(i32[w>>2]|0)');
  line = ReplaceAll(line, '*sp = ', 'i32[sp>>2] = ');
  line = ReplaceAll(line, '*sp', '(i32[sp>>2]|0)');
  line = ReplaceAll(line, '*ip', '(i32[ip>>2]|0)');
  line = ReplaceAll(line, 'sp[-1] = ', 'i32[(sp - 4)>>2] = ');
  line = ReplaceAll(line, 'sp[-1]', '(i32[(sp - 4)>>2]|0)');
  line = ReplaceAll(line, '*rp = ', 'i32[rp>>2] = ');
  line = ReplaceAll(line, '*rp', '(i32[rp>>2]|0)');
  line = ReplaceAll(line, 'ip[w]', '(i32[((ip + (w<<2))|0)>>2]|0)');
  line = ReplaceAll(line, '(cell_t) ((int32_t *) tos - ip)', '((tos - ip)>>2)');
  line = ReplaceAll(line, '(int32_t *) ', '');
  line = ReplaceAll(line, '(cell_t) ', '');
  line = ReplaceAll(line, '(ucell_t) ', '');
  line = ReplaceAll(line, '(cell_t *) ', '');
  line = ReplaceAll(line, '(int32_t *) ', '');
  line = ReplaceAll(line, 'sizeof(cell_t)', '(4)');
  line = ReplaceAll(line, 'return rp;', 'return rp|0;');
  console.log('        ' + line);
}

console.log(`
          default:
            break;
        }
        break;
      }
    }
    return 0;
  }
  return {
    run: run,
  }
}
`);

var boot_items = 'var boot = [';
for (var i = 0; i < boot.length; i++) {
  boot_items += boot[i] + ',';
}
boot_items += '];';
console.log(boot_items);

console.log(`
var text = document.getElementById('text');
var stdlib = {
  Uint8Array: Uint8Array,
  Int32Array: Int32Array,
  Math: Math,
};
var input = '';
var output = '';
var foreign = {
  exit: function(x) { console.log(x); },
  emit: function(ch) { output += String.fromCharCode(ch); text.innerText = output; },
  qkey: function() {
    if (input.length) {
      var r = input.charCodeAt(0);
      input = input.substr(1);
      return r;
    } else {
      return 0;
    }
  },
  color: function(c) { console.log(c); },
  print_hexadecimal: function(x) { console.log(x.toString(16)); },
  print_decimal: function(x) { console.log(x); },
};
var heap = new ArrayBuffer(Math.max(HEAP_SIZE * 2, HEAP_SIZE + DSTACK_SIZE + RSTACK_SIZE));
var interpreter = Interpreter(stdlib, foreign, heap);
var u8 = new Uint8Array(heap);
var i32 = new Int32Array(heap);
var request = new XMLHttpRequest();
for (var i = 0; i < boot.length; i++) {
  u8[i] = boot[i];
}
var dstack = HEAP_SIZE + 4;
i32[dstack>>2] = 4 * i32[0];
var rp = HEAP_SIZE + DSTACK_SIZE;
rp += 4;
i32[rp>>2] = dstack;
rp += 4;
i32[rp>>2] = 4;
rp = interpreter.run(rp);
setInterval(function() {
  rp = interpreter.run(rp);
}, 1);
window.onkeypress = function(e) {
  input += String.fromCharCode(e.charCode);
};
`);
