#define _GNU_SOURCE

#include <unistd.h>

extern void blockUserSignals(void);
extern void unblockUserSignals(void);
extern void stopTimer(void);
extern void startTimer(void);

int forkExecuteFile
	( char *const args[]
	, int path
	, char *workingDirectory
	, char **environment
	, int fdStdIn
	, int fdStdOut
	, int fdStdErr
	)
{
	int pid;

	blockUserSignals();
	stopTimer();

	switch (pid = fork()) {
	case -1:
		unblockUserSignals();
		startTimer();
		if (fdStdIn != -1) close(fdStdIn);
		if (fdStdOut != -1) close(fdStdOut);
		if (fdStdErr != -1) close(fdStdErr);
		return -1;
	case 0:
		unblockUserSignals();
		if (workingDirectory) {
			if (chdir (workingDirectory) < 0) {
				_exit(126);
			}
		}
		if (fdStdIn != -1) {
			dup2(fdStdIn, 0);
			close(fdStdIn);
		}
		if (fdStdOut != -1) {
			dup2(fdStdOut, 1);
			close(fdStdOut);
		}
		if (fdStdErr != -1) {
			dup2(fdStdErr, 2);
			close(fdStdErr);
		}

		// FIXME close file descriptors
		if (environment) {
			if (path) execvpe(args[0], args, environment);
			else      execve (args[0], args, environment);
		} else {
			if (path) execvp(args[0], args);
			else      execv (args[0], args);
		}
		_exit(127);
	default: break;
	}
	unblockUserSignals();
	startTimer();
	return pid;
}
