package main

import (
	"fmt"
	"os/exec"
	"regexp"

	"github.com/mattn/go-shellwords"
)

func main() {
	InitRuby()
}

type target struct {
	name string
}

func (t *target) match(s string) bool {
	return regexp.MustCompile(s).MatchString(t.name)
}

func cmd(c string) {
	if args, err := shellwords.Parse(c); err != nil {
		fmt.Println(err)
	} else {
		if _, err := exec.Command(args[0], args[1:]...).Output(); err != nil {
			fmt.Println(err)
		}
	}
}
