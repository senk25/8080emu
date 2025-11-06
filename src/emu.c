#include <assert.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include "disassmbler.h"
#include "emu.h"

#define CPU_TEST

static void printByte(uint8_t x) {
  int num_bits = 8;
  for (int i = num_bits - 1; i >= 0; i--) {
    if ((x >> i) & 1) {
      printf("1");
    } else {
      printf("0");
    }
  }
}

static void printState(CPUState *state) {
  printf("--Registers--\n");
  printf("A: %d \nB : % d, C : % d \nD : % d, E : % d \nH : % d, L : % d\n",
         state->a, state->b, state->c, state->d, state->e, state->h, state->l);
  printf("--SP/PC--\n");
  printf("PC: %04x\nSP: %04x\n", state->pc, state->sp);
  printf("--Flags--\n");
  printf("z: %d, s: %d, cy: %d, p: %d, ac: %d\n\n", state->cc.z, state->cc.s,
         state->cc.cy, state->cc.p, state->cc.ac);
}

static void unimplementedOpcodeError(uint8_t opcode) {
  printf("ERROR UNIMPLEMENTED OPCODE: ");
  printByte(opcode);
  printf("\n");
  exit(EXIT_FAILURE);
}

static inline uint16_t get16Bit(uint8_t hb, uint8_t lb) {
  return (hb << 8) | lb;
}

static inline uint8_t getMReg(CPUState *state) {
  uint16_t addr = get16Bit(state->h, state->l);
  return state->memory[addr];
}

static inline void setMReg(CPUState *state, uint8_t data) {
  uint16_t addr = get16Bit(state->h, state->l);
  state->memory[addr] = data;
}

static inline uint8_t parity(uint8_t x) {
  uint8_t n_one = 0;
  for (uint8_t i = 0; i < 8; i++) {
    uint8_t shifted = x >> i;
    if (shifted == 0)
      break;
    if (shifted & 1)
      n_one += 1;
  }
  return (n_one & 1) == 0;
}

static inline uint8_t sign(uint8_t x) { return (x & 0x80) != 0; }

static inline void i8080_lda(CPUState *state, uint8_t hb, uint8_t lb) {
  uint16_t addr = get16Bit(hb, lb);
  state->a = state->memory[addr];
  state->pc += 3;
}

static inline uint8_t checkCond(CPUState *state, uint8_t flag) {
  assert(flag <= 7);

  switch (flag) {
  case 0: // not zero
    return state->cc.z == 0;
    break;
  case 1: // zero
    return state->cc.z == 1;
    break;
  case 2: // no carry
    return state->cc.cy == 0;
    break;
  case 3: // carry
    return state->cc.cy == 1;
    break;
  case 4: // parity odd
    return state->cc.p == 0;
    break;
  case 5: // parity even
    return state->cc.p == 1;
    break;
  case 6: // pos
    return state->cc.s == 0;
    break;
  case 7: // neg
    return state->cc.s == 1;
    break;
  default:
    exit(EXIT_FAILURE);
  }
}

static inline void i8080_sta(CPUState *state, uint8_t hb, uint8_t lb) {
  uint16_t addr = get16Bit(hb, lb);
  state->memory[addr] = state->a;
  state->pc += 3;
}

static inline void i8080_lhld(CPUState *state, uint8_t hb, uint8_t lb) {
  uint16_t addr = get16Bit(hb, lb);
  state->l = state->memory[addr];
  state->h = state->memory[addr + 1];
  state->pc += 3;
}

static inline void i8080_shld(CPUState *state, uint8_t hb, uint8_t lb) {
  uint16_t addr = get16Bit(hb, lb);
  state->memory[addr] = state->l;
  state->memory[addr + 1] = state->h;
  state->pc += 3;
}

static inline void i8080_xchg(CPUState *state) {
  uint8_t tmp = state->h;
  state->h = state->d;
  state->d = tmp;
  tmp = state->l;
  state->l = state->e;
  state->e = tmp;
  state->pc += 1;
}

static inline void i8080_adi(CPUState *state, uint8_t db, uint8_t carry) {
  uint16_t val = state->a + db + carry;

  state->cc.z = (val & 0xff) == 0;
  state->cc.s = sign(val);
  state->cc.cy = val > 0xff;
  state->cc.p = parity(val);
  state->cc.ac = ((state->a & 0x08) || (db & 0x08)) && (!(val & 0x08));

  state->a = val & 0xff;
  state->pc += 2;
}

static inline void i8080_sui(CPUState *state, uint8_t db, uint8_t carry) {
  uint8_t val = state->a + ((~(db + carry)) + 1);

  state->cc.cy = (db + carry) > state->a;
  state->cc.s = sign(val);
  state->cc.z = val == 0;
  state->cc.p = parity(val);

  state->a = val;
  state->pc += 2;
}

static inline void i8080_daa(CPUState *state) {
  uint8_t nib1 = state->a & 0x0f;
  uint8_t nib2 = state->a >> 4;

  if (nib1 > 9) {
    nib1 += 6;
    nib2 += 1;
  } else if (state->cc.ac) {
    nib1 += 6;
  }

  if (nib2 > 9 || state->cc.cy)
    nib2 += 6;

  state->cc.ac = (nib1 & 0xf0) != 0;
  state->cc.cy = (nib2 & 0xf0) != 0;
  uint8_t val = (nib1 & 0x0f) + (nib2 << 4);
  state->cc.z = val == 0;
  state->cc.p = parity(val);
  state->cc.s = sign(val);

  state->a = val;
  state->pc += 1;
}

static inline void i8080_mov(CPUState *state, uint8_t opcode,
                             uint8_t *registers[]) {
  uint8_t dest_reg = (opcode >> 3) & 7;
  uint8_t src_reg = opcode & 7;

  if (src_reg == MEM_REGISTER) {
    *registers[dest_reg] = getMReg(state);
  } else if (dest_reg == MEM_REGISTER) {
    setMReg(state, *registers[src_reg]);
  } else {
    *registers[dest_reg] = *registers[src_reg];
  }
  state->pc += 1;
}

static inline void i8080_mvi(CPUState *state, uint8_t opcode, uint8_t db,
                             uint8_t *registers[]) {
  uint8_t dest_reg = (opcode >> 3) & 7;

  if (dest_reg == MEM_REGISTER) {
    setMReg(state, db);
  } else {
    *registers[dest_reg] = db;
  }
  state->pc += 2;
}

static inline void i8080_lxi(CPUState *state, uint8_t *reg1, uint8_t *reg2,
                             uint8_t hb, uint8_t lb) {
  *reg1 = hb;
  *reg2 = lb;
  state->pc += 3;
}

static inline void i8080_lxi_sp(CPUState *state, uint8_t hb, uint8_t lb) {
  state->sp = get16Bit(hb, lb);
  state->pc += 3;
}

static inline void i8080_stax(CPUState *state, uint8_t hb, uint8_t lb) {
  uint16_t addr = get16Bit(hb, lb);
  state->memory[addr] = state->a;
  state->pc += 1;
}

static inline void i8080_ldax(CPUState *state, uint8_t hb, uint8_t lb) {
  uint16_t addr = get16Bit(hb, lb);
  state->a = state->memory[addr];
  state->pc += 1;
}

static inline void i8080_add(CPUState *state, uint8_t opcode,
                             uint8_t *registers[], uint8_t carry) {
  uint8_t reg = opcode & 7;
  uint8_t db;
  if (reg == MEM_REGISTER) {
    db = getMReg(state);
  } else {
    db = *registers[reg];
  }
  uint16_t val = state->a + db + carry;

  state->cc.z = (val & 0xff) == 0;
  state->cc.s = sign(val);
  state->cc.cy = val > 0xff;
  state->cc.p = parity(val);
  state->cc.ac = ((state->a & 0x08) || (db & 0x08)) && ((val & 0x08) == 0);

  state->a = val & 0xff;
  state->pc += 1;
}

static inline void i8080_sub(CPUState *state, uint8_t opcode,
                             uint8_t *registers[], uint8_t carry) {

  uint8_t reg = opcode & 7;
  uint8_t db;
  if (reg == MEM_REGISTER) {
    db = getMReg(state);
  } else {
    db = *registers[reg];
  }
  db += carry;
  uint8_t val = state->a + ((~db) + 1);
  state->cc.cy = db > state->a;
  state->cc.s = sign(val);
  state->cc.z = val == 0;
  state->cc.p = parity(val);

  state->a = val;
  state->pc += 1;
}

static inline void i8080_inr(CPUState *state, uint8_t opcode,
                             uint8_t *registers[]) {
  uint8_t reg = (opcode >> 3) & 7;
  uint8_t val, db;
  if (reg == MEM_REGISTER) {
    db = getMReg(state);
    val = db + 1;
    setMReg(state, val);
  } else {
    db = *registers[reg];
    val = db + 1;
    *registers[reg] = val;
  }

  state->cc.z = val == 0;
  state->cc.s = sign(val);
  state->cc.p = parity(val);
  state->cc.ac = (db & 0x08) && ((val & 0x08) == 0);

  state->pc += 1;
}

static inline void i8080_dcr(CPUState *state, uint8_t opcode,
                             uint8_t *registers[]) {
  uint8_t reg = (opcode >> 3) & 7;
  uint8_t val, db;
  if (reg == MEM_REGISTER) {
    val = getMReg(state) - 1;
    setMReg(state, val);
  } else {
    val = *registers[reg] - 1;
    *registers[reg] = val;
  }

  state->cc.z = val == 0;
  state->cc.s = sign(val);
  state->cc.p = parity(val);

  state->pc += 1;
}

static inline void i8080_inx(CPUState *state, uint8_t *hreg, uint8_t *lreg) {
  *lreg += 1;
  // we overflowed
  if (*lreg == 0) {
    *hreg += 1;
  }

  state->pc += 1;
}
static inline void i8080_dcx(CPUState *state, uint8_t *hreg, uint8_t *lreg) {
  *lreg -= 1;
  // we underflowed
  if (*lreg == 0xff) {
    *hreg -= 1;
  }

  state->pc += 1;
}

static inline void i8080_dad(CPUState *state, uint8_t reg_high,
                             uint8_t reg_low) {
  uint16_t val = state->l + reg_low;
  state->l = val & 0xff;
  uint8_t cy = val > 0xff;
  val = state->h + reg_high + cy;
  state->h = val & 0xff;
  state->cc.cy = val > 0xff;

  state->pc += 1;
}

static inline void i8080_ana(CPUState *state, uint8_t opcode,
                             uint8_t *registers[]) {
  uint8_t reg = opcode & 7;
  uint8_t db;
  if (reg == MEM_REGISTER) {
    db = getMReg(state);
  } else {
    db = *registers[reg];
  }
  state->a = state->a & db;
  state->cc.s = sign(state->a);
  state->cc.p = parity(state->a);
  state->cc.z = state->a == 0;
  state->cc.cy = 0;

  state->pc += 1;
}

static inline void i8080_ani(CPUState *state, uint8_t db) {
  state->a = state->a & db;
  state->cc.s = sign(state->a);
  state->cc.p = parity(state->a);
  state->cc.z = state->a == 0;
  state->cc.cy = 0;
  state->cc.ac = 0;

  state->pc += 2;
}

static inline void i8080_ora(CPUState *state, uint8_t opcode,
                             uint8_t *registers[]) {
  uint8_t reg = opcode & 7;
  uint8_t db;
  if (reg == MEM_REGISTER) {
    db = getMReg(state);
  } else {
    db = *registers[reg];
  }
  state->a = state->a | db;
  state->cc.s = sign(state->a);
  state->cc.p = parity(state->a);
  state->cc.z = state->a == 0;
  state->cc.cy = 0;
  state->cc.ac = 0;

  state->pc += 1;
}

static inline void i8080_ori(CPUState *state, uint8_t db) {
  state->a = state->a | db;
  state->cc.s = sign(state->a);
  state->cc.p = parity(state->a);
  state->cc.z = state->a == 0;
  state->cc.cy = 0;
  state->cc.ac = 0;

  state->pc += 2;
}

static inline void i8080_xra(CPUState *state, uint8_t opcode,
                             uint8_t *registers[]) {
  uint8_t reg = opcode & 7;
  uint8_t db;
  if (reg == MEM_REGISTER) {
    db = getMReg(state);
  } else {
    db = *registers[reg];
  }
  state->a = state->a ^ db;
  state->cc.s = sign(state->a);
  state->cc.p = parity(state->a);
  state->cc.z = state->a == 0;
  state->cc.cy = 0;
  state->cc.ac = 0;

  state->pc += 1;
}

static inline void i8080_xri(CPUState *state, uint8_t db) {
  state->a = state->a ^ db;
  state->cc.s = sign(state->a);
  state->cc.p = parity(state->a);
  state->cc.z = state->a == 0;
  state->cc.cy = 0;
  state->cc.ac = 0;

  state->pc += 2;
}

static inline void i8080_cmp(CPUState *state, uint8_t opcode,
                             uint8_t *registers[]) {
  uint8_t reg = opcode & 7;
  uint8_t db;
  if (reg == MEM_REGISTER) {
    db = getMReg(state);
  } else {
    db = *registers[reg];
  }
  uint8_t val = state->a - db;
  state->cc.s = sign(val);
  state->cc.p = parity(val);
  state->cc.z = state->a == db;
  state->cc.cy = db > state->a;

  state->pc += 1;
}

static inline void i8080_cpi(CPUState *state, uint8_t db) {
  uint8_t val = state->a - db;
  state->cc.s = sign(val);
  state->cc.p = parity(val);
  state->cc.z = state->a == db;
  state->cc.cy = db > state->a;

  state->pc += 2;
}

static inline void i8080_rlc(CPUState *state) {
  uint16_t val = state->a << 1;
  uint8_t cy = val >> 8;

  // set first bit if wrap
  state->a = (0xff & val) | cy;
  state->cc.cy = cy;

  state->pc += 1;
}

static inline void i8080_rrc(CPUState *state) {
  uint8_t val = state->a >> 1;
  uint8_t cy = state->a & 1;
  assert((cy == 1) || (cy == 0));
  // set 8th bit if wrap
  state->a = val | (cy << 7);
  state->cc.cy = cy;

  state->pc += 1;
}

static inline void i8080_ral(CPUState *state) {
  uint16_t val = state->a << 1;
  uint8_t cy = (val >> 8) != 0;
  // set first bit using prev carry
  state->a = (0xff & val) | state->cc.cy;
  state->cc.cy = cy;

  state->pc += 1;
}

static inline void i8080_rar(CPUState *state) {
  uint8_t val = state->a >> 1;
  uint8_t cy = state->a & 1;
  // set 8th bit if wrap
  state->a = val | (state->cc.cy << 7);
  state->cc.cy = cy;

  state->pc += 1;
}

static inline void i8080_cma(CPUState *state) {
  state->a = ~state->a;
  state->pc += 1;
}

static inline void i8080_cmc(CPUState *state) {
  state->cc.cy = ~state->cc.cy;
  state->pc += 1;
}

static inline void i8080_stc(CPUState *state) {
  state->cc.cy = 1;
  state->pc += 1;
}

static inline void i8080_jmp(CPUState *state, uint8_t hb, uint8_t lb) {
  uint16_t addr = get16Bit(hb, lb);
  state->pc = addr;
}

static inline void i8080_jmp_cond(CPUState *state, uint8_t opcode, uint8_t hb,
                                  uint8_t lb) {
  uint8_t cond_flag = (opcode >> 3) & 7;
  if (checkCond(state, cond_flag)) {
    i8080_jmp(state, hb, lb);
  } else {
    state->pc += 3;
  }
}

static inline void i8080_call(CPUState *state, uint8_t hb, uint8_t lb) {
#ifdef CPU_TEST
  uint16_t called_addr = get16Bit(hb, lb);
  if (called_addr == 5) {
    if (state->c == 9) {
      uint16_t addr = get16Bit(state->d, state->e);
      char *c = &state->memory[addr];
      printf("\nPRINTING: ");
      while (*c != '$') {
        printf("%c", *c);
        c += 1;
      }
      printf("\n");
    } else if (state->c == 2) {
      printf("Char output routine called");
    }

    getchar();
    state->pc += 3;
    return;
  } else if (called_addr == 0) {
    printf("Exiting");
    exit(0);
  }

#endif

  uint16_t ret_addr = state->pc + 3;
  state->memory[state->sp - 1] = ret_addr >> 8;
  state->memory[state->sp - 2] = ret_addr & 0xff;
  state->sp -= 2;

  uint16_t subroutine_addr = get16Bit(hb, lb);
  state->pc = subroutine_addr;
}

static inline void i8080_call_cond(CPUState *state, uint8_t opcode, uint8_t hb,
                                   uint8_t lb) {
  uint8_t cond_flag = (opcode >> 3) & 7;
  if (checkCond(state, cond_flag)) {
    i8080_call(state, hb, lb);
  } else {
    state->pc += 3;
  }
}

static inline void i8080_ret(CPUState *state) {
  uint8_t lb = state->memory[state->sp];
  uint8_t hb = state->memory[state->sp + 1];
  state->pc = get16Bit(hb, lb);
  state->sp += 2;
}

static inline void i8080_ret_cond(CPUState *state, uint8_t opcode) {
  uint8_t cond_flag = (opcode >> 3) & 7;
  if (checkCond(state, cond_flag)) {
    i8080_ret(state);
  } else {
    state->pc += 1;
  }
}

static inline void i8080_rst(CPUState *state, uint8_t opcode) {
  uint16_t ret_addr = state->pc + 1;
  state->memory[state->sp - 1] = ret_addr >> 8;
  state->memory[state->sp - 2] = ret_addr & 0xff;
  state->sp -= 2;

  uint8_t rst_num = (opcode >> 3) & 7;
  uint16_t rst_addr = rst_num * 8;
  state->pc = rst_addr;
}

static inline void i8080_pchl(CPUState *state) {
  uint16_t addr = get16Bit(state->h, state->l);
  state->pc = addr;
}

static inline void i8080_push(CPUState *state, uint8_t hr, uint8_t lr) {
  state->memory[state->sp - 1] = hr;
  state->memory[state->sp - 2] = lr;
  state->sp -= 2;

  state->pc += 1;
}

static inline void i8080_pop(CPUState *state, uint8_t *hr, uint8_t *lr) {
  *lr = state->memory[state->sp];
  *hr = state->memory[state->sp + 1];
  state->sp += 2;

  state->pc += 1;
}

static inline uint8_t make_psw_flag(const ConditionCodes *cc) {
  assert((cc->cy == 1) || (cc->cy == 0));
  uint8_t flag = cc->cy & 1;
  flag |= cc->p << 2;
  flag |= cc->ac << 4;
  flag |= cc->z << 6;
  flag |= cc->s << 7;
  return flag;
}

static inline void i8080_push_psw(CPUState *state) {
  state->memory[state->sp - 1] = state->a;
  state->memory[state->sp - 2] = make_psw_flag(&state->cc);
  state->sp -= 2;

  state->pc += 1;
}


// 106
//
static inline void i8080_pop_psw(CPUState *state) {
  uint8_t psw_flag = state->memory[state->sp];
  state->cc.cy = psw_flag & 1;
  state->cc.p = (psw_flag >> 2) & 1;
  state->cc.ac = (psw_flag >> 4) & 1;
  state->cc.z = (psw_flag >> 6) & 1;
  state->cc.s = (psw_flag >> 7) & 1;

  state->a = state->memory[state->sp + 1];
  state->sp += 2;

  state->pc += 1;
}

static inline void i8080_xthl(CPUState *state) {
  uint8_t tmp = state->l;
  state->l = state->memory[state->sp];
  state->memory[state->sp] = tmp;
  tmp = state->h;
  state->h = state->memory[state->sp + 1];
  state->memory[state->sp + 1] = tmp;

  state->pc += 1;
}

static inline void i8080_ei(CPUState *state) {
  state->int_enable = 1;
  state->pc += 1;
}

static inline void i8080_di(CPUState *state) {
  state->int_enable = 0;
  state->pc += 1;
}

static inline void i8080_sphl(CPUState *state) {
  state->sp = get16Bit(state->h, state->l);
  state->pc += 1;
}

static inline void i8080_in(CPUState *state, uint8_t port) {
  state->a = state->in_ports[port];
  state->pc += 2;
}

static inline void i8080_out(CPUState *state, uint8_t port) {
  state->out_ports[port] = state->a;
  state->pc += 2;
}

static inline void i8080_halt() {
  printf("Halting 8080 Emulator");
  exit(EXIT_SUCCESS);
}

void handleOpcode(CPUState *state, uint8_t *registers[]) {
  uint8_t *code = &state->memory[state->pc];

  switch (code[0]) {
  // LDA
  case 0x3a:
    i8080_lda(state, code[2], code[1]);
    break;
  // STA
  case 0x32:
    i8080_sta(state, code[2], code[1]);
    break;
  // LHLD
  case 0x2a:
    i8080_lhld(state, code[2], code[1]);
    break;
  // SHLD
  case 0x22:
    i8080_shld(state, code[2], code[1]);
    break;
  // XCHG
  case 0xeb:
    i8080_xchg(state);
    break;
  // ADI
  case 0xc6:
    i8080_adi(state, code[1], 0);
    break;
  // ACI
  case 0xce:
    i8080_adi(state, code[1], state->cc.cy);
    break;
  // SUI
  case 0xd6:
    i8080_sui(state, code[1], 0);
    break;
  // SBI
  case 0xde:
    i8080_sui(state, code[1], state->cc.cy);
    break;
  // DAA
  case 0x27:
    i8080_daa(state);
    break;
  // STC
  case 0x37:
    i8080_stc(state);
    break;
  // ANI
  case 0xe6:
    i8080_ani(state, code[1]);
    break;
  // ORI
  case 0xf6:
    i8080_ori(state, code[1]);
    break;
  // XRI
  case 0xee:
    i8080_xri(state, code[1]);
    break;
  // RLC
  case 0x07:
    i8080_rlc(state);
    break;
  // RRC
  case 0x0f:
    i8080_rrc(state);
    break;
  // RAL
  case 0x17:
    i8080_ral(state);
    break;
  // RAR
  case 0x1f:
    i8080_rar(state);
    break;
  // CMA
  case 0x2f:
    i8080_cma(state);
    break;
  // CMC
  case 0x3f:
    i8080_cmc(state);
    break;
  // JMP
  case 0xc3:
    i8080_jmp(state, code[2], code[1]);
    break;
  // CALL
  case 0xcd:
    i8080_call(state, code[2], code[1]);
    break;
  // RET
  case 0xc9:
    i8080_ret(state);
    break;
  // PCHL
  case 0xe9:
    i8080_pchl(state);
    break;
  // XTHL
  case 0xe3:
    i8080_xthl(state);
    break;
  // SPHL
  case 0xf9:
    i8080_sphl(state);
    break;
  // EI
  case 0xfb:
    i8080_ei(state);
    break;
  // DI
  case 0xf3:
    i8080_di(state);
    break;
  // HLT
  case 0x76:
    i8080_halt();
    break;
  // IN
  case 0xdb:
    i8080_in(state, code[1]);
    break;
  // OUT
  case 0xd3:
    i8080_out(state, code[1]);
    break;
  // CPI
  case 0xfe:
    i8080_cpi(state, code[1]);
    break;

  // MOV
  case 0x40 ... 0x75:
  case 0x77 ... 0x7f:
    i8080_mov(state, code[0], registers);
    break;

  // MVI
  case 0x06:
  case 0x0e:
  case 0x16:
  case 0x1e:
  case 0x26:
  case 0x2e:
  case 0x36:
  case 0x3e:
    i8080_mvi(state, code[0], code[1], registers);
    break;

  // LXI
  case 0x01: // bc
    i8080_lxi(state, &state->b, &state->c, code[2], code[1]);
    break;
  case 0x11: // de
    i8080_lxi(state, &state->d, &state->e, code[2], code[1]);
    break;
  case 0x21: // hl
    i8080_lxi(state, &state->h, &state->l, code[2], code[1]);
    break;
  case 0x31: // sp
    i8080_lxi_sp(state, code[2], code[1]);
    break;

  // STAX
  case 0x02:
    i8080_stax(state, state->b, state->c);
    break;
  case 0x12:
    i8080_stax(state, state->d, state->e);
    break;

  // LDAX
  case 0x0a:
    i8080_ldax(state, state->b, state->c);
    break;
  case 0x1a:
    i8080_ldax(state, state->d, state->e);
    break;

  // ADD
  case 0x80 ... 0x87: {
    i8080_add(state, code[0], registers, 0);
    break;
  }

  // ADC
  case 0x88 ... 0x8f: {
    i8080_add(state, code[0], registers, state->cc.cy);
    break;
  }

  // SUB
  case 0x90 ... 0x97:
    i8080_sub(state, code[0], registers, 0);
    break;

  // SBB
  case 0x98 ... 0x9f:
    i8080_sub(state, code[0], registers, state->cc.cy);
    break;

  // ANA
  case 0xa0 ... 0xa7:
    i8080_ana(state, code[0], registers);
    break;

  // XRA
  case 0xa8 ... 0xaf:
    i8080_xra(state, code[0], registers);
    break;

  // ORA
  case 0xb0 ... 0xb7:
    i8080_ora(state, code[0], registers);
    break;

  // CMP
  case 0xb8 ... 0xbf:
    i8080_cmp(state, code[0], registers);
    break;

    // INR
  case 0x04:
  case 0x0c:
  case 0x14:
  case 0x1c:
  case 0x24:
  case 0x2c:
  case 0x34:
  case 0x3c:
    i8080_inr(state, code[0], registers);
    break;

  // DCR
  case 0x05:
  case 0x0d:
  case 0x15:
  case 0x1d:
  case 0x25:
  case 0x2d:
  case 0x35:
  case 0x3d:
    i8080_dcr(state, code[0], registers);
    break;

  // INX
  case 0x03:
    i8080_inx(state, &state->b, &state->c);
    break;
  case 0x13:
    i8080_inx(state, &state->d, &state->e);
    break;
  case 0x23:
    i8080_inx(state, &state->h, &state->l);
    break;
  case 0x33:
    state->sp += 1;
    state->pc += 1;
    break;

  // DCX
  case 0x0b:
    i8080_dcx(state, &state->b, &state->c);
    break;
  case 0x1b:
    i8080_dcx(state, &state->d, &state->e);
    break;
  case 0x2b:
    i8080_dcx(state, &state->h, &state->l);
    break;
  case 0x3b:
    state->sp -= 1;
    state->pc += 1;
    break;

  // DAD
  case 0x09:
    i8080_dad(state, state->b, state->c);
    break;
  case 0x19:
    i8080_dad(state, state->d, state->e);
    break;
  case 0x29:
    i8080_dad(state, state->h, state->l);
    break;
  case 0x39: {
    i8080_dad(state, state->sp >> 8, state->sp & 0xff);
    break;
  }

  // JMP conditional
  case 0xc2: // JNZ

  case 0xca: // JZ
  case 0xd2: // JNC
  case 0xda: // JC
  case 0xe2: // JPO
  case 0xea: // JPE
  case 0xf2: // JP
  case 0xfa: // JM
    i8080_jmp_cond(state, code[0], code[2], code[1]);
    break;

  // Cccc
  case 0xc4: // CNZ
  case 0xcc: // CZ
  case 0xd4: // CNC
  case 0xdc: // CC
  case 0xe4: // CPO
  case 0xec: // CPE
  case 0xf4: // CP
  case 0xfc: // CM
    i8080_call_cond(state, code[0], code[2], code[1]);
    break;

  // Rccc
  case 0xc0: // RNZ
  case 0xc8: // RZ
  case 0xd0: // RNC
  case 0xd8: // RC
  case 0xe0: // RPO
  case 0xe8: // RPE
  case 0xf0: // RP
  case 0xf8: // RM
    i8080_ret_cond(state, code[0]);
    break;

  // RST n
  case 0xc7:
  case 0xcf:
  case 0xd7:
  case 0xdf:
  case 0xe7:
  case 0xef:
  case 0xf7:
  case 0xff:
    i8080_rst(state, code[0]);
    break;

  // PUSH
  case 0xc5:
    i8080_push(state, state->b, state->c);
    break;
  case 0xd5:
    i8080_push(state, state->d, state->e);
    break;
  case 0xe5:
    i8080_push(state, state->h, state->l);
    break;

  // PUSH PSW
  case 0xf5:
    i8080_push_psw(state);
    break;

  // POP
  case 0xc1:
    i8080_pop(state, &state->b, &state->c);
    break;
  case 0xd1:
    i8080_pop(state, &state->d, &state->e);
    break;
  case 0xe1:
    i8080_pop(state, &state->h, &state->l);
    break;

  // POP PSW
  case 0xf1:
    i8080_pop_psw(state);
    break;

  // NOP
  case 0x00:
  case 0x08:
  case 0x10:
  case 0x18:
  case 0x20:
  case 0x28:
  case 0x30:
  case 0x38:
  case 0xcb:
  case 0xd9:
  case 0xdd:
  case 0xed:
  case 0xfd:
    state->pc += 1;
    break;

  default:
    unimplementedOpcodeError(code[0]);
    break;
  }

  printState(state);
}

void readFileIntoMemory(char *fpath, CPUState *state, uint32_t offset) {

  FILE *f = fopen(fpath, "rb");

  if (f == NULL) {
    printf("error: Couldn't open file %s\n", fpath);
    exit(1);
  }

  fseek(f, 0, SEEK_END);
  int fsize = ftell(f);
  fseek(f, 0, SEEK_SET);

  fread(&state->memory[offset], fsize, 1, f);
  fclose(f);
}

int main(int argc, char *argv[]) {
  CPUState *state = calloc(sizeof(CPUState), 1);
  state->memory = (uint8_t *)malloc(MEMORY_SIZE);
  state->pc = 0x000;
  state->sp = 0x000;

  uint8_t *registers[8];
  registers[0] = &state->b;
  registers[1] = &state->c;
  registers[2] = &state->d;
  registers[3] = &state->e;
  registers[4] = &state->h;
  registers[5] = &state->l;
  registers[6] = NULL; // mem reg
  registers[7] = &state->a;

  readFileIntoMemory(argv[1], state, 0x100);

#ifdef CPU_TEST
  state->pc = 0x100;
#endif

  while (1) {
    printf("%04x: ", state->pc);
    printOpcode(&state->memory[state->pc]);
    printf("\n");
    handleOpcode(state, registers);
    // getchar();
  }

  return 0;
}
