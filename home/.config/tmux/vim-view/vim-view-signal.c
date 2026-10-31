/* pipe-pane's command for vim-view: read a pane's output and drop it, and
 * tell the viewer that the pane changed by writing one byte to its FIFO.
 * The first output after a quiet moment is told at once, then at most one
 * byte every INTERVAL_MS; the last output of a burst is always told.
 *
 * Usage: vim-view-signal FIFO
 * Build: make vim-view-signal */

#define _GNU_SOURCE /* F_SETPIPE_SZ, and clock_gettime under -std=c11. */

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>

#define INTERVAL_MS 20

static long long now_ms(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return (long long)t.tv_sec * 1000 + t.tv_nsec / 1000000;
}

/* Read all the output there is; returns 1 if some came, -1 when pipe-pane
 * closed, -2 on errors. */
static int drain(void) {
    static char buffer[1 << 18];
    int some = 0;
    for (;;) {
        ssize_t n = read(0, buffer, sizeof buffer);
        if (n > 0)
            some = 1;
        else if (n == 0)
            return -1;
        else if (errno == EAGAIN)
            return some;
        else if (errno != EINTR)
            return -2;
    }
}

int main(int argc, char **argv) {
    if (argc != 2)
        return 2;
    /* Without a reader the FIFO refuses to open: no viewer, nothing to do. */
    int fifo = open(argv[1], O_WRONLY | O_NONBLOCK | O_CLOEXEC);
    if (fifo < 0)
        return 1;
    signal(SIGPIPE, SIG_IGN);
    fcntl(0, F_SETFL, fcntl(0, F_GETFL) | O_NONBLOCK);
    /* Room for well over a millisecond of what tmux can produce, so that it
     * never waits for this program between reads. tmux gives pipe-pane a
     * socket pair; a pipe would take the second call. */
    int room = 1 << 20;
    setsockopt(0, SOL_SOCKET, SO_RCVBUF, &room, sizeof room);
    fcntl(0, F_SETPIPE_SZ, room);
    long long told = -INTERVAL_MS;
    int untold = 0;
    for (;;) {
        if (!untold) {
            struct pollfd input = { .fd = 0, .events = POLLIN };
            if (poll(&input, 1, -1) < 0 && errno != EINTR)
                return 1;
        }
        int some = drain();
        if (some < 0)
            return some == -1 ? 0 : 1;
        untold |= some;
        if (untold && now_ms() >= told + INTERVAL_MS) {
            /* A full FIFO already holds news the viewer has yet to read. */
            if (write(fifo, "", 1) < 0 && errno == EPIPE)
                return 0;
            told = now_ms();
            untold = 0;
        } else if (untold) {
            /* Until it is time to tell, let output gather between reads: one
             * read then takes many of tmux's small writes. */
            nanosleep(&(struct timespec){ .tv_nsec = 1000000 }, NULL);
        }
    }
}
