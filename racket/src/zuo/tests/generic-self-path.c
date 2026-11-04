/* Exercise the generic-Unix executable-path fallback on Linux, where
   the normal build uses /proc/self/exe instead. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __linux__
# define ZUO_TEST_GENERIC_SELF_PATH
# undef __linux__
#endif

#define main zuo_original_main
#include "../zuo.c"
#undef main

int main(void) {
#ifdef ZUO_TEST_GENERIC_SELF_PATH
  char *path;
  int ok;

  if (unsetenv("PATH") != 0)
    return 2;
  path = zuo_self_path_c("zuo");
  ok = !strcmp(path, "zuo");
  free(path);
  if (!ok)
    return 1;

  if (setenv("PATH", "", 1) != 0)
    return 2;
  path = zuo_self_path_c("zuo");
  ok = !strcmp(path, "zuo");
  free(path);
  if (!ok)
    return 1;
#endif
  return 0;
}
