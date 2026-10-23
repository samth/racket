/* libFuzzer driver for Racket CS.

   Boots Racket CS once, loads "safe-eval.rkt", and calls one of its
   `fuzz-*` entry points on each input. An entry point returns 0 for
   every expected outcome and 1 for a bug, which becomes `abort()` here.

   Compile-time settings (all strings):
     BOOT_DIR       directory holding petite.boot, scheme.boot, racket.boot
     COLLECTS_DIR   the "collects" directory
     CONFIG_DIR     the "etc" directory
     FUZZ_MODULE    absolute path of "safe-eval.rkt"
     FUZZ_ENTRY     entry point name, such as "fuzz-eval" */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "chezscheme.h"
#include "racketcs.h"

#ifndef FUZZ_ENTRY
# define FUZZ_ENTRY "fuzz-eval"
#endif

static ptr entry_proc;

int LLVMFuzzerInitialize(int *argc, char ***argv)
{
  racket_boot_arguments_t ba;

  memset(&ba, 0, sizeof(ba));
  ba.boot1_path = BOOT_DIR "/petite.boot";
  ba.boot2_path = BOOT_DIR "/scheme.boot";
  ba.boot3_path = BOOT_DIR "/racket.boot";
  ba.exec_file = "racket-fuzz";
  ba.collects_dir = COLLECTS_DIR;
  ba.config_dir = CONFIG_DIR;
  ba.exit_after = 0;

  racket_boot(&ba);

  {
    ptr mod_path = Scons(Sstring_to_symbol("file"),
                         Scons(Sstring(FUZZ_MODULE), Snil));
    ptr vals = racket_dynamic_require(mod_path, Sstring_to_symbol(FUZZ_ENTRY));
    entry_proc = Scar(vals);
    Slock_object(entry_proc);
  }

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
