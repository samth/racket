/* A static producer exercises real child processes, file probes, and writes. */
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
extern char **environ;

static void probe(const char *file) {
  int fd = open(file, O_RDONLY);
  if (fd >= 0) close(fd); /* Deliberately tolerate a missing input. */
}
int main(int argc, char **argv) {
  if (argc != 5) return 2;
  const char *mode = argv[1];
  if (!environ[0] || strcmp(environ[0], "ACTION_TEST=fixed") || environ[1]) return 6;
  if (!strcmp(mode, "missing")) probe("hidden");
  if (!strcmp(mode, "child")) {
    pid_t p = fork();
    if (!p) { probe("hidden"); _exit(0); }
    waitpid(p, NULL, 0);
  }
  if (!strcmp(mode, "network")) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd >= 0) close(fd);
  }
  if (!strcmp(mode, "write-input")) {
    int fd = open(argv[2], O_WRONLY | O_TRUNC);
    if (fd >= 0) close(fd);
  }
  if (!strcmp(mode, "symlink")) {
    symlink("/etc/passwd", argv[3]);
    return 0;
  }
  if (!strcmp(mode, "optional")) probe("optional");
  FILE *in = fopen(argv[2], "r");
  FILE *out = fopen(argv[3], "w");
  if (!in || !out) return 3;
  int c;
  while ((c = fgetc(in)) != EOF) fputc(c, out);
  const char *value = getenv("ACTION_TEST");
  if (!value || strcmp(value, "fixed")) return 4;
  fclose(in);
  fclose(out);
  if (!strcmp(mode, "incomplete")) return 0;
  out = fopen(argv[4], "w");
  if (!out) return 5;
  fputs("second\n", out);
  fclose(out);
  return !strcmp(mode, "fail");
}
