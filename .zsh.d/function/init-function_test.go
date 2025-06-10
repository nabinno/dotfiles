package main

import "testing"

func TestTargetMatch(t *testing.T) {
	t1 := &target{name: "darwin19"}
	if !t1.match("darwin.*") {
		t.Errorf("expected darwin match")
	}
	if t1.match("linux.*") {
		t.Errorf("unexpected linux match")
	}
}

func TestCmdEcho(t *testing.T) {
	defer func() {
		if r := recover(); r != nil {
			t.Fatalf("cmd panicked: %v", r)
		}
	}()
	cmd("echo test")
}
