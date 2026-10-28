/* Runs a fuzz target on each file named on the command line, for
   testing a target built without libFuzzer (as for riscv64 under qemu). */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
int LLVMFuzzerInitialize(int *argc, char ***argv);
int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size);
int main(int argc, char **argv) {
  LLVMFuzzerInitialize(&argc, &argv);
  for (int i = 1; i < argc; i++) {
    FILE *f = fopen(argv[i], "rb");
    if (!f) { perror(argv[i]); return 1; }
    fseek(f, 0, SEEK_END); long n = ftell(f); fseek(f, 0, SEEK_SET);
    uint8_t *buf = malloc(n ? n : 1);
    if (fread(buf, 1, n, f) != (size_t)n) return 1;
    fclose(f);
    LLVMFuzzerTestOneInput(buf, n);
    free(buf);
    printf("ok %s\n", argv[i]);
  }
  return 0;
}
