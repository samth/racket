/* A static producer exercises real child processes, file probes, and writes. */
#define _GNU_SOURCE
#include <fcntl.h>
#include <sched.h>
#include <signal.h>
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
static int exec_child(void *data) {
  char **argv = data;
  char tool[4096];
  snprintf(tool, sizeof(tool), "%s-child", argv[0]);
  execl(tool, tool, "good", argv[2], argv[3], argv[4], (char *)NULL);
  return 12;
}
int main(int argc, char **argv) {
  if (argc != 5) return 2;
  const char *mode = argv[1];
  if (!environ[0] || strcmp(environ[0], "ACTION_TEST=fixed") || environ[1]) return 6;
  if (!strcmp(mode, "abort")) raise(SIGABRT);
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
  if (!strcmp(mode, "clone-exec")) {
    char *stack = malloc(1024 * 1024);
    if (!stack) return 13;
    pid_t child = clone(exec_child, stack + 1024 * 1024, CLONE_FS | SIGCHLD, argv);
    int status;
    if (child < 0 || waitpid(child, &status, 0) < 0 || status) return 14;
    free(stack);
  }
  if (!strcmp(mode, "proc") || !strcmp(mode, "clone-exec")) {
    char pid[64], file[128], exe[4096];
    ssize_t n = readlink("/proc/self", pid, sizeof(pid) - 1);
    if (n <= 0) return 9;
    pid[n] = 0;
    snprintf(file, sizeof(file), "/proc/%s", pid);
    readlink(file, exe, sizeof(exe)); /* realpath's expected EINVAL */
    snprintf(file, sizeof(file), "/proc/%s/exe", pid);
    n = readlink(file, exe, sizeof(exe) - 1);
    if (n <= 0) return 10;
    exe[n] = 0;
    if (strcmp(exe, argv[0])) return 11;
  }
  if (!strcmp(mode, "proc-other")) probe("/proc/1/environ");
  if (!strcmp(mode, "metadata") || !strcmp(mode, "metadata-read")) {
    struct stat s;
    if (stat("optional", &s)) return 7;
    if (!strcmp(mode, "metadata-read")) probe("optional");
  }
  FILE *in = fopen(argv[2], "r");
  FILE *out = fopen(argv[3], "w");
  if (!in || !out) return 3;
  int c;
  while ((c = fgetc(in)) != EOF) fputc(c, out);
  if (!strcmp(mode, "stdin"))
    while ((c = fgetc(stdin)) != EOF) fputc(c, out);
  if (!strcmp(mode, "retained")) {
    FILE *scratch = fopen("unlisted-header.h", "w");
    if (!scratch) return 8;
    fputs("generated header\n", scratch);
    fclose(scratch);
  }
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
