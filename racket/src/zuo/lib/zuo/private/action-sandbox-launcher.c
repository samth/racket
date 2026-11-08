/* Build with cc -static -O2. The sandbox runner starts this trusted shim before
   tracing the action itself. argv[1] supplies the declared stdin file (or
   /dev/null); argv[2...] supplies the action command. Replace bubblewrap's environment (which injects
   PWD) with the exact NUL-delimited name=value records supplied on stdin. */
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
  size_t size = 0, capacity = 4096, count = 0, i;
  char *bytes = malloc(capacity);
  char **env;
  if (argc < 3 || !bytes) return 125;
  for (;;) {
    ssize_t n;
    if (size == capacity) {
      char *next;
      if (capacity >= 16 * 1024 * 1024) return 125;
      next = realloc(bytes, capacity *= 2);
      if (!next) return 125;
      bytes = next;
    }
    n = read(0, bytes + size, capacity - size);
    if (n < 0) { if (errno == EINTR) continue; return 125; }
    if (!n) break;
    size += n;
  }
  if (size && bytes[size - 1]) return 125;
  for (i = 0; i < size; i++) if (!bytes[i]) count++;
  env = calloc(count + 1, sizeof(char *));
  if (!env) return 125;
  for (i = 0, count = 0; i < size; i += strlen(bytes + i) + 1) {
    if (!bytes[i] || bytes[i] == '=' || !strchr(bytes + i, '=')) return 125;
    env[count++] = bytes + i;
  }
  close(0);
  if (open(argv[1], O_RDONLY) != 0) return 125;
  execve(argv[2], argv + 2, env);
  return 126;
}
