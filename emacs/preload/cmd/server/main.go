package main

import (
	"bufio"
	"fmt"
	"net"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"sync"
	"time"
)

const SocketPath = "/tmp/emacs-preload.sock"

type DaemonId string

type State struct {
	nextDaemonId DaemonId
}

func main() {
	if err := os.RemoveAll(SocketPath); err != nil {
		panic(err)
	}

	ln, err := net.Listen("unix", SocketPath)
	if err != nil {
		panic(err)
	}
	defer ln.Close()

	fmt.Println("Server listening on", SocketPath)

	state := State{
		nextDaemonId: createDaemon(),
	}

	for {
		conn, err := ln.Accept()
		if err != nil {
			fmt.Println("accept error:", err)
			continue
		}
		go handleConn(&state, conn)
	}
}

var daemonMu sync.Mutex

func handleConn(s *State, c net.Conn) {
	defer c.Close()

	reader := bufio.NewReader(c)

	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			return
		}

		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		switch line {
		case "REQUEST_ID":
			handleRequestID(s, c)
		default:
			c.Write([]byte("ERR unknown command\n"))
		}
	}
}

func handleRequestID(s *State, c net.Conn) {
	daemonMu.Lock()
	defer daemonMu.Unlock()

	c.Write([]byte(s.nextDaemonId + "\n"))

	s.nextDaemonId = createDaemon()
}

func createDaemon() DaemonId {
	id := DaemonId(strconv.FormatInt(time.Now().UnixNano(), 10))

	cmd := exec.Command("emacs", "--fg-daemon="+string(id))

	if err := cmd.Start(); err != nil {
		panic("No daemon created")
	}

	// Option A: fire-and-reap to avoid zombie processes
	go func() {
		_ = cmd.Wait()
	}()

	return id
}
