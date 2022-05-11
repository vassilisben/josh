#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
void fork_exec(char *command) {
	pid_t pid = fork();
	if (pid < 0) {
		perror("invalid system call");
	} else if (pid == 0) {
		system(command);
	} else {
		waitpid(pid, NULL, 0);
	}
	return;
}

int main() {
	fork_exec("for((i=1;i<=100;i++))\n"
		"do\n"
    "echo $i\n"
		"done");
}
