#ifndef _stenoforth_h
#define _stenoforth_h

#define DEBUG_MODE 0
#define DEBUG_WORDS 0

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

typedef intptr_t cell_t;
typedef uintptr_t ucell_t;
#if __SIZEOF_POINTER__ == 8
typedef __int128_t dcell_t;
typedef __uint128_t udcell_t;
#elif __SIZEOF_POINTER__ == 4
typedef int64_t dcell_t;
typedef uint64_t udcell_t;
#else
# error "unsupported cell size"
#endif

#define PRIMITIVE_LIST \
  X("NOP", NOP, ) \
  X("YIELD", YIELD, ++sp; *sp = tos; ++rp; *rp = (cell_t) sp; ++rp; *rp = (cell_t) ip; return rp) \
  X("DOCOL", DOCOL, ++rp; *rp = (cell_t) ip; ip = ip + w) \
  X("EXIT", EXIT, ip = (int32_t *) *rp; --rp) \
  X("DORUN", DORUN, ++sp; *sp = tos; tos = (cell_t) ip; ++rp; *rp = (cell_t) ip; ip = ip + w) \
  X("DOCON", DOCON, ++sp; *sp = tos; tos = ip[w]) \
  X("DOLIT", DOLIT, ++sp; *sp = tos; tos = *ip; ++ip) \
  X("DOVAR", DOVAR, ++sp; *sp = tos; tos = (cell_t) (ip + w)) \
  X("EXECUTE", EXECUTE, --ip; w = (cell_t) ((int32_t *) tos - ip); tos = *sp; --sp; continue) \
  X("RUNSCODE>", RUNSCODE, ip = (int32_t *) *rp; --rp) \
  X("DONEXT", DONEXT, if (*rp) { *rp = (*rp - 1)|0; ip = ip + *ip; } else { --rp; ++ip; }) \
  X("BRANCH", BRANCH, ip = ip + *ip) \
  X("0BRANCH", ZBRANCH, if (!tos) ip = ip + *ip; else ++ip; tos = *sp; --sp) \
  X("DUP", DUP, ++sp; *sp = tos) \
  X("CELL", CELL, ++sp; *sp = tos; tos = sizeof(cell_t)) \
  X("2DUP", DDUP, ++sp; *sp = tos; tos = sp[-1]; ++sp; *sp = tos; tos = sp[-1]) \
  X("DROP", DROP, tos = *sp; --sp) \
  X("2DROP", DDROP, tos = *sp; --sp; tos = *sp; --sp) \
  X("OVER", OVER, ++sp; *sp = tos; tos = sp[-1]) \
  X("SWAP", SWAP, w = tos; tos = *sp; *sp = w) \
  X("AND", AND, tos = tos & *sp; --sp) \
  X("OR", OR, tos = tos | *sp; --sp) \
  X("XOR", XOR, tos = tos ^ *sp; --sp) \
  X("0<", ZLESS, tos = (-((tos|0) < 0))|0) \
  X("0=", ZEQUAL, tos = (-(!tos))|0) \
  X("NEGATE", NEGATE, tos = (-tos)|0) \
  X("INVERT", INVERT, tos = (~tos)|0) \
  X("ABS", ABS, if ((tos|0) < 0) tos = (-tos)|0) \
  X("=", EQUAL, tos = (-((*sp|0) == (tos|0)))|0; --sp) \
  X("<>", NEQUAL, tos = (-((*sp|0) != (tos|0)))|0; --sp) \
  X("<", LESS, tos = (-((*sp|0) < (tos|0)))|0; --sp) \
  X("U<", ULESS, tos = (-((ucell_t) ((*sp)>>0) < (ucell_t) (tos>>0)))|0; --sp) \
  X("+", PLUS, tos = (tos + *sp)|0; --sp) \
  X("UM+", UMPLUS, w = (*sp + tos) | 0; tos = ((ucell_t) (w>>0) < (ucell_t) (tos>>0)) | \
                                              ((ucell_t) (w>>0) < (ucell_t) ((*sp)>>0)); *sp = w) \
  X("*", STAR, tos *= *sp; --sp) \
  X("-", SUB, tos = (*sp - tos)|0; --sp) \
  X("/MOD", SLASHMOD, w = *sp; t = tos; *sp = ((w|0) % (t|0))|0; tos = ((w|0) / (t|0))|0) \
  X("/", SLASH, tos = ((*sp|0) / (tos|0))|0; --sp) \
  X("MOD", MOD, tos = ((*sp|0) % (tos|0))|0; --sp) \
  X(">R", PUSH, ++rp; *rp = tos; tos = *sp; --sp) \
  X("R>", POP, ++sp; *sp = tos; tos = *rp; --rp) \
  X("R@", RAT, ++sp; *sp = tos; tos = *rp) \
  X("@", FETCH, tos = *(cell_t *) tos) \
  X("L@", LFETCH, tos = *(int32_t *) tos) \
  X("C@", CFETCH, tos = *(uint8_t *) tos) \
  X("+!", PSTORE, *(cell_t *) tos = (*sp + (*(cell_t *) tos))|0; --sp; tos = *sp; --sp) \
  X("L+!", LPSTORE, *(int32_t *) tos = (*sp + (*(int32_t *) tos))|0; --sp; tos = *sp; --sp) \
  X("!", STORE, *(cell_t *) tos = *sp; --sp; tos = *sp; --sp) \
  X("L!", LSTORE, *(int32_t *) tos = *sp; --sp; tos = *sp; --sp) \
  X("C!", CSTORE, *(uint8_t *) tos = *sp; --sp; tos = *sp; --sp) \
  X("MIN", MIN, if ((tos|0) > (*sp|0)) tos = *sp; --sp) \
  X("MAX", MAX, if ((tos|0) < (*sp|0)) tos = *sp; --sp) \
  X("ROT", ROT, w = sp[-1]; sp[-1] = *sp; *sp = tos; tos = w) \
  X("-ROT", NROT, w = tos; tos = *sp; *sp = sp[-1]; sp[-1] = w) \
  X("SP@", SPFETCH, ++sp; *sp = tos; tos = (cell_t) sp) \
  X("SP!", SPSTORE, sp = (cell_t *) tos; tos = *sp; --sp) \
  X("RP@", RPFETCH, ++sp; *sp = tos; tos = (cell_t) rp) \
  X("RP!", RPSTORE, rp = (cell_t *) tos; tos = *sp; --sp) \
  X("TERMINATE", TERMINATE, exit(tos|0)) \
  X("EMIT", EMIT, emit(tos|0); tos = *sp; --sp) \
  X("?KEY", QKEY, ++sp; *sp = tos; tos = qkey()|0) \
  X("COLOR", COLOR, color(tos|0); tos = *sp; --sp) \

#define PRIMITIVE_LIST_DEBUG \
  X("hex.", HEXDOT, print_hexadecimal(tos|0); tos = *sp; --sp) \
  X("dec.", DECDOT, print_decimal(tos|0); tos = *sp; --sp) \

#define PRIMITIVE_LIST_EXTRA \
  X("*/MOD", SSMOD, d = (dcell_t) tos; \
    m = (dcell_t) *sp; \
    n = (dcell_t) sp[-1]; \
    n *= m; \
    --sp; \
    tos = (cell_t) (n / d); \
    *sp = (cell_t)(n%d)) \
  X("*/", STASL, d = (dcell_t) tos; \
    m = (dcell_t) *sp; \
    n = (dcell_t) sp[-1]; \
    n *= m; \
    sp-=2; \
    tos = (cell_t) (n / d)) \

// Provided by core.
extern cell_t *vm_load(const char *filename);
extern cell_t *vm(cell_t *initrp);
// Provided by platform.
extern void emit(cell_t ch);
extern void color(cell_t ch);
extern cell_t qkey(void);

#endif
