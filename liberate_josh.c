#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <math.h>
#include <string.h>

int fork_exec (char *command) {
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

float int_to_float (int x) {
	return (float) (x);
}

int float_to_int (float x) {
	return (int) (x);
}

float josh_sqrt (int x) {
	return (float) (sqrt(x));
}

float josh_sqrt2 (float x) {
	return (float) (sqrt(x));
}

int josh_pow (int a, int b) {
	return (int) (pow(a, b));
}

float josh_pow2 (float a, float b) {
	return (float) (pow(a, b));
}

char *josh_concat (char *a, char *b) {
	char *ret = malloc(strlen(a) + strlen(b));
	strcpy(ret, a);
	strcpy(ret + strlen(a), b);
	return ret;
}

char *josh_subset (char *a, int start, int finish) {
	if (start > strlen(a)) {
		return 0;
	} else if (finish < 0) {
		return 0;
	}

	if (start < 0) {
		start = 0;
	} else if (finish > strlen(a)) {
		finish = strlen(a);
	}

	char *ret = malloc(finish - start);
	strncpy(ret, a + start, finish - start);

	return ret;
}

int josh_strcmp (char * a, char *b) {
	return strcmp(a, b);
}
