/* Internal counter and environment tests. Run through memory.zuo, or compile
   with -fsanitize=address,undefined and run each named case separately. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define main zuo_original_main
#include "../zuo.c"
#undef main

#define z zuo_roots.image
#define Z zuo_roots.runtime

static void check(int ok) {
  if (!ok) {
    fprintf(stderr, "memory test failed\n");
    exit(2);
  }
}

int main(int argc, char **argv) {
  check(argc == 2);
  zuo_primitive_init(0);
  zuo_image_init(NULL);

  if (!strcmp(argv[1], "environment")) {
    char missing[] = "NOEQUALS", empty[] = "", valid[] = "OK=value", blank[] = "EMPTY=";
    char *env[] = { missing, empty, valid, blank, NULL };
    zuo_t *vars;
    environ = env;
    vars = zuo_get_envvars();
    check(zuo_length_int(vars) == 2);
    check(!strcmp(ZUO_STRING_PTR(ZUO_CAR(ZUO_CAR(vars))), "OK"));
    check(!strcmp(ZUO_STRING_PTR(ZUO_CDR(ZUO_CAR(vars))), "value"));
    check(!strcmp(ZUO_STRING_PTR(ZUO_CDR(ZUO_CAR(ZUO_CDR(vars)))), ""));
  } else if (!strcmp(argv[1], "probe-parse")) {
    zuo_probe_each = 0;
    check(setenv("ZUO_PROBE_EACH", "9999999999", 1) == 0);
    zuo_configure();
    check(zuo_probe_each == (zuo_uint_t)9999999999ULL);
    zuo_probe_each = 0;
    check(setenv("ZUO_PROBE_EACH", "999999999999999999999999999999999", 1) == 0);
    zuo_configure();
    check(zuo_probe_each == ~(zuo_uint_t)0);
  } else if (!strcmp(argv[1], "probe-counter")) {
    Z.o_interp_e = z.o_true;
    Z.o_interp_k = z.o_done_k;
    Z.o_interp_meta_k = z.o_null;
    Z.o_interp_in_proc = z.o_false;
    zuo_probe_each = (zuo_uint_t)0x80000000;
    zuo_probe_counter = 0x7fffffff;
    interp_step();
    check(zuo_probe_counter == (zuo_uint_t)0x80000000);
    zuo_probe_each = 2;
    zuo_probe_counter = ~(zuo_uint_t)0;
    interp_step();
    check(zuo_probe_counter == 0);
  } else if (!strcmp(argv[1], "symbol-limit")) {
    zuo_t *a, *b, *table;
    a = zuo_make_symbol("a");
    zuo_symbol_count = 0x7ffffffe;
    b = zuo_make_symbol("b");
    table = zuo_trie_extend(z.o_empty_hash, a, z.o_true);
    table = zuo_trie_extend(table, b, z.o_false);
    check(zuo_trie_lookup(table, a) == z.o_true);
    check(zuo_trie_lookup(table, b) == z.o_false);
    (void)zuo_make_symbol("c");
    return 2;
  } else if (!strcmp(argv[1], "handle-limit")) {
    zuo_handle_t *a, *b;
    zuo_handle_count = 0x7ffffffe;
    a = (zuo_handle_t *)zuo_handle(0, zuo_handle_closed_status);
    check(a->id == 0x7ffffffe);
    b = (zuo_handle_t *)zuo_handle(0, zuo_handle_closed_status);
    (void)b;
    return 2;
  } else
    return 2;
  return 0;
}
