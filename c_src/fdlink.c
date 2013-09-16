#include <sys/select.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <stdlib.h>
#include <assert.h>

#define safe(expr, error) do { if (!(expr)) { perror(error); exit(1); } } while (0)

static sig_atomic_t exited;

static void
sighandler(int sig)
{
	if (sig == SIGCHLD)
		exited = 1;
}

int
main(int argc, char **argv)
{
	pid_t pid;

	if (argc < 2) {
		fprintf(stderr, "usage: fdlink bin args...\n");
		exit(2);
	}

	signal(SIGCHLD, sighandler);

	if ((pid = fork()) == 0) {
		close(0);
		safe(execv(argv[1], argv + 1) != -1, "fdlink execv");
		/* NOTREACHED */
	} else {
		assert(pid != -1);
		safe(fcntl(0, F_SETFL, O_NONBLOCK) != -1, "fdlink fcntl");

		int nfds;

		do {
			if (exited == 1) {
				int status;
				if (waitpid(pid, &status, WNOHANG) != -1) {
					exit(WEXITSTATUS(status));
				};
				exited = 0;
			}

			fd_set fdset_r; FD_ZERO(&fdset_r); FD_SET(0, &fdset_r);
			fd_set fdset_e; FD_ZERO(&fdset_e); FD_SET(0, &fdset_e);

			nfds = select(64, &fdset_r, NULL, &fdset_e, NULL);
			if (nfds == -1 && (errno == EAGAIN || errno == EINTR))
				continue;
			else if (nfds == -1) {
				perror("fdlink select");
				exit(1);
			}

			if (FD_ISSET(0, &fdset_r) || FD_ISSET(0, &fdset_e)) {
				char buf[1024];
				while (1) { 
					int nread = read(0, &buf, sizeof(buf));
					if (nread == -1 && errno == EINTR)
						continue;
					else if (nread == -1 && errno == EAGAIN)
						break;
					else if (nread == -1) {
						perror("fdlink read");
						exit(1);
					} else if (nread == 0) {
						kill(pid, SIGHUP);
						exit(0);
					}
				}
			}

		} while (1);
	}

	return 0;
}
