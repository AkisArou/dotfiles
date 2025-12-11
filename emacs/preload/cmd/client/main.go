package main

import (
	"akisarou/emacs-preload/internal"
	"bufio"
	"fmt"
	"net"
	"os"
	"os/exec"
	"os/signal"
	"strings"
	"syscall"
)

func main() {
	conn, err := net.Dial("unix", internal.SocketPath)
	if err != nil {
		panic(err)
	}
	defer conn.Close()

	reader := bufio.NewReader(conn)

	_, _ = conn.Write([]byte("REQUEST_ID\n"))
	id_line, _ := reader.ReadString('\n')
	id := strings.ReplaceAll(id_line, "\n", "")

	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-c
		killDaemon(id)
		os.Exit(1)
	}()

	// Launch emacsclient and block until it exits
    args := []string{"-c", "-s", id}
    args = append(args, os.Args[1:]...)
    cmd := exec.Command("emacsclient", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		panic(err)
	}

	//TODO: do not wait to close. fire and forget
	killDaemon(id)
}

func killDaemon(id string) {
	killCmd := exec.Command("emacsclient", "-s", id, "--eval", "(kill-emacs)")
	killCmd.Stdout = os.Stdout
	killCmd.Stderr = os.Stderr

	if err := killCmd.Run(); err != nil {
		fmt.Println("Failed to kill daemon:", err)
	}
}
