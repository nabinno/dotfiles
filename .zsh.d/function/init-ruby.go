package main

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"unsafe"
)

// InitRuby ...
func InitRuby() {
	if _, err := exec.Command("type", "-p", "ruby").Output(); err == nil {
		getRuby()
	} else {
		// rm -f ~/.ruby-version
		// rbenv global $REQUIRED_RUBY_VERSION
		// _REQUIRED_RUBY_VERSION=$(echo $REQUIRED_RUBY_VERSION | sed 's/\(.*\..*\)\..*/\1/')
		// _CURRENT_RUBY_VERSION=$(ruby -v | cut -f 2 -d " " | sed 's/^\([0-9]\{1,\}\.[0-9]\{1,\}\)\..*/\1/')
		// if [[ $_REQUIRED_RUBY_VERSION > $_CURRENT_RUBY_VERSION ]]; then get-ruby; fi
	}
}

func addAsdfPluginRuby() {
	cmd("asdf plugin-add ruby")
}

func getRbenv() {
	res, err := exec.Command("rbenv", "root").Output()
	if err != nil {
		fmt.Println(err)
	}
	rbenvRoot := *(*string)(unsafe.Pointer(&res))

	cmd("anyenv install rbenv")
	cmd(fmt.Sprintf("git clone https://github.com/amatsuda/gem-src.git %s/plugins/gem-src", rbenvRoot))
	cmd("exec -l zsh")
}

func upgradeRbenvByRubyBuild() {
	res, err := exec.Command("rbenv", "root").Output()
	if err != nil {
		fmt.Println(err)
	}
	rbenvRoot := *(*string)(unsafe.Pointer(&res))

	cmd(fmt.Sprintf("rm -fr %s/plugins/ruby-build", rbenvRoot))
	cmd(fmt.Sprintf("git clone https://github.com/rbenv/ruby-build.git %s/plugins/ruby-build", rbenvRoot))
}

func getRuby() {
	getRubyByRbenv()
}

func getRubyByAsdf() {
	t := target{os.Getenv("OSTYPE")}
	switch {
	case t.match("cygwin"):
		cmd("apt-cyg install ruby")
	case t.match("freebsd.*"), t.match("darwin.*"), t.match("linux.*"):
		if _, err := exec.Command("asdf", "plugin-add", "ruby").Output(); err == nil {
			cmd(fmt.Sprintf("asfd install ruby %s", os.Getenv("REQUIRED_RUBY_VERSION")))
			cmd(fmt.Sprintf("asdf global ruby %s", os.Getenv("REQUIRED_RUBY_VERSION")))
			getGlobalGemPackages()
		} else {
			addAsdfPluginRuby()
		}
	}
}

func getRubyByRbenv() {
	var err error
	t := target{os.Getenv("OSTYPE")}
	switch {
	case t.match("cygwin"):
		cmd("apt-cyg install ruby")
	case t.match("freebsd.*"), t.match("darwin.*"), t.match("linux.*"):
		if _, err = exec.Command("type", "-p", "rbenv").Output(); err == nil {
			cmd(fmt.Sprintf("rbenv install %s", os.Getenv("REQUIRED_RUBY_VERSION")))
			cmd("rbenv rehash")
			cmd(fmt.Sprintf("rbenv global %s", os.Getenv("REQUIRED_RUBY_VERSION")))
			getGlobalGemPackages()
		} else {
			getRbenv()
		}
	}
}

func getGlobalGemPackages() {
	gems := []string{
		"benchmark-ips",
		"bundler",
		"compass",
		"git-trend",
		"haml",
		"html2slim",
		"peek-rblineprof",
		"rack-lineprof",
		"rails",
		"rblineprof",
		"rubygems-bundler",
		"sidekiq",
		"slim",
		"stackprof",
		"unicorn",
	}
	cmd(fmt.Sprintf("gem install %s", strings.Join(gems, " ")))
	cmd("gem install rufo -v 0.1.0")
}
