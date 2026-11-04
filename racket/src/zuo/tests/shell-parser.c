/* On Windows, put a guard page after each argument-vector allocation
   so an overrun is detected even without a sanitizer. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
# include <windows.h>

static void *guard_malloc(size_t size) {
  SYSTEM_INFO info;
  size_t rounded;
  char *base;
  DWORD old_protect;

  GetSystemInfo(&info);
  rounded = ((size + info.dwPageSize - 1) / info.dwPageSize) * info.dwPageSize;
  base = VirtualAlloc(NULL, rounded + info.dwPageSize, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
  if (base == NULL)
    return NULL;
  if (!VirtualProtect(base + rounded, info.dwPageSize, PAGE_NOACCESS, &old_protect)) {
    VirtualFree(base, 0, MEM_RELEASE);
    return NULL;
  }
  return base + rounded - size;
}

static void guard_free(void *p) {
  MEMORY_BASIC_INFORMATION info;
  if (p == NULL)
    return;
  if (!VirtualQuery(p, &info, sizeof(info)))
    abort();
  VirtualFree(info.AllocationBase, 0, MEM_RELEASE);
}

# define malloc guard_malloc
# define free guard_free
#endif

#define main zuo_original_main
#define WinMain zuo_original_WinMain
#define wmain zuo_original_wmain
#include "../zuo.c"
#undef main
#undef WinMain
#undef wmain

int main(int argc, char **argv) {
  char input[257];
  char **words;
  zuo_intptr_t count;
  int n, i;

  if (argc != 2)
    return 2;
  n = atoi(argv[1]);
  if ((n < 1) || (n > 128))
    return 2;
  for (i = 0; i < n; i++) {
    input[2*i] = 'a';
    input[2*i+1] = ' ';
  }
  input[2*n] = 0;
  words = zuo_shell_to_strings_c(input, 0, &count);
  if ((count != n) || (words[count] != NULL))
    return 1;
  for (i = 0; i < n; i++) {
    if (strcmp(words[i], "a"))
      return 1;
  }
  free(words);
  return 0;
}
