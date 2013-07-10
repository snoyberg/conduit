#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

struct node {
    pid_t pid;
    struct node *next;
};

static struct node * add_node(pid_t pid, struct node *n) {
    struct node *n2 = malloc(sizeof(struct node));
    n2->pid = pid;
    n2->next = n;
    return n2;
}

static struct node * remove_node(pid_t pid, struct node *n) {
    if (!n) {
        return n;
    }
    else if (n->pid == pid) {
        struct node *n2 = n->next;
        free(n);
        return remove_node(pid, n2);
    }
    else {
        n->next = remove_node(pid, n->next);
        return n;
    }
}

extern void track_process(int fd, pid_t pid, int b) {
    unsigned int buffer[2];

    //printf("Tracking process %d, %d\n", pid, b);

    buffer[0] = pid;
    buffer[1] = b;
    if (! write(fd, buffer, sizeof(unsigned int) * 2)) {
        //printf("Error writing to fd %d\n", fd);
    }
}

// Returns FD to write to, or -1 on failure.
extern int launch_process_tracker(void) {
    int pipes[2];
    pid_t child;

    if (pipe(pipes)) {
        return -1;
    }

    child = fork();

    if (child < 0) {
        return -1;
    }
    else if (child == 0) {
        unsigned int buffer[2];
        struct node *n = 0, *n2;

        close(pipes[1]);

        // Prevent monitoring programs like Upstart from killing this
        // new process along with the parent
        setpgid(0, 0);

        while (read(pipes[0], buffer, sizeof(unsigned int) * 2) > 0) {
            if (buffer[1]) {
                //printf("Adding node %d\n", buffer[0]);
                n = add_node(buffer[0], n);
            }
            else {
                //printf("Removing node %d\n", buffer[0]);
                n = remove_node(buffer[0], n);
            }
        }

        for (n2 = n; n2; n2 = n2->next) {
            //printf("Sending process %d TERM signal\n", n2->pid);
            kill(n2->pid, SIGTERM);
        }

        sleep(2);

        while (n) {
            //printf("Sending process %d KILL signal\n", n->pid);
            kill(n2->pid, SIGKILL);

            n2 = n;
            n = n->next;
            free(n2);
        }
    }
    else {
        close(pipes[0]);
        return pipes[1];
    }
}
