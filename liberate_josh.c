#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

int fork_exec(char *command) {
	pid_t pid = fork();
	if (pid < 0) {
		perror("invalid system call");
	} else if (pid == 0) {
		system(command);
	} else {
		waitpid(pid, NULL, 0);
	}
	return 0;
}

