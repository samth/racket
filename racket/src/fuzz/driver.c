/* libFuzzer driver for Racket CS.

   Boots Racket CS once, loads "safe-eval.rkt", and calls one of its
   `fuzz-*` entry points on each input. An entry point returns 0 for
   every expected outcome and 1 for a bug, which becomes `abort()` here.

   Compile-time settings (all strings):
     FUZZ_ENTRY     entry point name, such as "fuzz-eval"
   and either
     FUZZ_EMBEDDED  defined: boot files and modules come from
                    "embed-data.S", and no files are needed at run time
   or
     FUZZ_MODULE    absolute path of "safe-eval.rkt"
     BOOT_DIR       directory holding petite.boot, scheme.boot, racket.boot
     COLLECTS_DIR   the "collects" directory
     CONFIG_DIR     the "etc" directory

   With RACKET_COVERAGE_COUNTERS defined (and a Racket CS built with it
   and with PLT_CS_COVERAGE_COUNTERS set), Scheme code also reports
   coverage: the Chez Scheme counters are registered with libFuzzer
   like clang's inline 8-bit counters. Code that a fuzzed program
   compiles gets no counters, since counters placed by hashing the
   program's own code would reward new programs, not new behavior of
   Racket. */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "chezscheme.h"
#include "racketcs.h"

#ifndef __has_feature
# define __has_feature(x) 0
#endif
#if defined(__SANITIZE_ADDRESS__) || __has_feature(address_sanitizer)
# include <sanitizer/lsan_interface.h>
# define FUZZ_LSAN_DISABLE() __lsan_disable()
# define FUZZ_LSAN_ENABLE() __lsan_enable()
#else
# define FUZZ_LSAN_DISABLE() (void)0
# define FUZZ_LSAN_ENABLE() (void)0
#endif

#ifndef FUZZ_ENTRY
# define FUZZ_ENTRY "fuzz-eval"
#endif

static ptr entry_proc;

#ifdef FUZZ_EMBEDDED
extern char fuzz_petite_boot[], fuzz_petite_boot_end[];
extern char fuzz_scheme_boot[], fuzz_scheme_boot_end[];
extern char fuzz_racket_boot[], fuzz_racket_boot_end[];
extern char fuzz_modules[], fuzz_modules_end[];
#endif

#ifdef RACKET_COVERAGE_COUNTERS
void __sanitizer_cov_8bit_counters_init(uint8_t *start, uint8_t *stop);
void __sanitizer_cov_pcs_init(const uintptr_t *pcs_beg, const uintptr_t *pcs_end);

/* libFuzzer wants a PC table entry, a (PC, flags) pair, for each
   counter; the PCs only need to be distinct. */
static void register_scheme_counters(void)
{
  iptr len;
  uint8_t *counters = Scoverage_counters(&len);
  uintptr_t *pcs;
  iptr i;

  if (!counters) return;

  pcs = malloc(2 * len * sizeof(uintptr_t));
  for (i = 0; i < len; i++) {
    pcs[2 * i] = (uintptr_t)(counters + i);
    pcs[2 * i + 1] = 0;
  }

  __sanitizer_cov_8bit_counters_init(counters, counters + len);
  __sanitizer_cov_pcs_init(pcs, pcs + 2 * len);
}
#endif

int LLVMFuzzerInitialize(int *argc, char ***argv)
{
  racket_boot_arguments_t ba;

  memset(&ba, 0, sizeof(ba));
#ifdef FUZZ_EMBEDDED
  ba.boot1_data = fuzz_petite_boot;
  ba.boot1_len = fuzz_petite_boot_end - fuzz_petite_boot;
  ba.boot2_data = fuzz_scheme_boot;
  ba.boot2_len = fuzz_scheme_boot_end - fuzz_scheme_boot;
  ba.boot3_data = fuzz_racket_boot;
  ba.boot3_len = fuzz_racket_boot_end - fuzz_racket_boot;
  ba.collects_dir = "";
  ba.config_dir = NULL;
#else
  ba.boot1_path = BOOT_DIR "/petite.boot";
  ba.boot2_path = BOOT_DIR "/scheme.boot";
  ba.boot3_path = BOOT_DIR "/racket.boot";
  /* A list of paths, each ending in NUL, with an extra NUL at the end;
     `parse_coldirs` in "boot.c" reads past a single NUL. */
  ba.collects_dir = COLLECTS_DIR "\0";
  ba.config_dir = CONFIG_DIR;
#endif
  ba.exec_file = "racket-fuzz";
  ba.exit_after = 0;

  /* Racket keeps what it allocates while booting until the process
     exits, often referenced only from the Scheme heap, which
     LeakSanitizer does not scan; so don't report those as leaks.
     Leaks from running an input are still reported. */
  FUZZ_LSAN_DISABLE();

  racket_boot(&ba);

#ifdef RACKET_COVERAGE_COUNTERS
  register_scheme_counters();
#endif

#ifdef FUZZ_EMBEDDED
  racket_embedded_load_bytes(fuzz_modules, fuzz_modules_end - fuzz_modules, 0);
#endif

  {
#ifdef FUZZ_EMBEDDED
    /* "embed-modules.rkt" declares the module as 'fuzz:safe-eval */
    ptr mod_path = Scons(Sstring_to_symbol("quote"),
                         Scons(Sstring_to_symbol("fuzz:safe-eval"), Snil));
#else
    ptr mod_path = Scons(Sstring_to_symbol("file"),
                         Scons(Sstring(FUZZ_MODULE), Snil));
#endif
    ptr vals = racket_dynamic_require(mod_path, Sstring_to_symbol(FUZZ_ENTRY));
    entry_proc = Scar(vals);
    Slock_object(entry_proc);
  }

  FUZZ_LSAN_ENABLE();

  return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
  ptr bstr = Smake_bytevector(size, 0);
  ptr vals;

  memcpy(Sbytevector_data(bstr), data, size);
  vals = racket_apply(entry_proc, Scons(bstr, Snil));

  if (!Spairp(vals) || !Sfixnump(Scar(vals)) || Sfixnum_value(Scar(vals)) != 0)
    abort();

  return 0;
}
