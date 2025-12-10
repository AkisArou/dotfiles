package main

import (
	"fmt"
	"os/exec"
	"strconv"
	"time"
)


type State struct {
	Daemons []*Daemon
}

type Daemon struct {
	Id string
}

func NewDaemon() Daemon {
	Id:= strconv.FormatInt( time.Now().UnixNano(), 10)

	return Daemon{
		Id: Id,
	}
}

func (d *Daemon) Init() {
	cmd := exec.Command("emacs", "--daemon=" + d.Id)

    out, err := cmd.CombinedOutput()

    if err != nil {
        fmt.Printf("Failed to start daemon: %v\nOutput: %s\n", err, out)
        return
    }

    fmt.Printf("Daemon with ID [%s] started successfully: %s\n", d.Id, out)
}

func (d *Daemon) Destroy() {
	cmd := exec.Command("emacsclient", "-s",  d.Id, "--eval", "(kill-emacs)")

    out, err := cmd.CombinedOutput()

    if err != nil {
        fmt.Printf("Failed to kill daemon with ID [%s]: %v\nOutput: %s\n", d.Id, err, out)
        return
    }

    fmt.Printf("Daemon with ID [%s] killed successfully: %s\n", d.Id, out)
}

func main() {
	reserved_daemon := NewDaemon()
	reserved_daemon.Init()
}
