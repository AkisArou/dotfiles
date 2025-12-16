package main

import (
	"bufio"
	"context"
	"fmt"
	"log"
	"net"
	"os"
	"os/exec"
	"os/signal"
	"runtime"
	"strings"
	"syscall"
)

const SocketPath = "/tmp/emacs-preload.sock"

func main() {
	conn, err := net.Dial("unix", SocketPath)
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Close()

	reader := bufio.NewReader(conn)

	_, _ = conn.Write([]byte("REQUEST_ID\n"))
	idLine, err := reader.ReadString('\n')
	if err != nil {
		log.Fatal(err)
	}
	id := strings.TrimSpace(idLine)

	// Context canceled on SIGINT / SIGTERM / SIGHUP (if delivered)
	ctx, stop := signal.NotifyContext(
		context.Background(),
		os.Interrupt,
		syscall.SIGTERM,
		syscall.SIGHUP,
	)
	defer stop()

	pwd, err := os.Getwd()
	if err != nil {
		log.Fatal(err)
	}

	eval := fmt.Sprintf(`(progn
	(when (get-buffer "*Warnings*")
		(delete-windows-on "*Warnings*")
		(kill-buffer "*Warnings*"))
	(let ((buf (generate-new-buffer "*new*")))
		(switch-to-buffer buf)
		(setq default-directory "%s/")
		(display-splash-screen)
		(setq default-directory "%s/")))`, pwd, pwd)

	args := []string{"-c", "-s", id, "--eval", eval}
	args = append(args, os.Args[1:]...)

	cmd := exec.CommandContext(ctx, "emacsclient", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	// Linux-only hardening: child receives SIGTERM if parent dies
	if runtime.GOOS == "linux" {
		cmd.SysProcAttr = &syscall.SysProcAttr{
			Pdeathsig: syscall.SIGTERM,
		}
	}

	// Watchdog: daemon dies when emacsclient exits, regardless of reason
	done := make(chan error, 1)
	go func() {
		done <- cmd.Run()
	}()

	select {
	case <-ctx.Done():
		// Signal received; emacsclient will be canceled by context
		<-done
	case <-done:
		// emacsclient exited normally or due to UI disappearance
	}

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
