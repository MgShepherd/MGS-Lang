package test

import (
	"bytes"
	"errors"
	"fmt"
	"michael/mgstests/internal/utils"
	"os"
	"os/exec"
)

type Test struct {
	Name     *string
	ExitCode *int
	Body     string
}

const buildPath = "./build/"

func (t Test) IsHeaderFilled() bool {
	return t.Name != nil && t.ExitCode != nil
}

func (t Test) IsHeaderEmpty() bool {
	return t.Name == nil && t.ExitCode == nil
}

func (t Test) String() string {
	return fmt.Sprintf("TEST\nName: %s\nExpected Exit Code: %d\nBody:\n%s\n", *t.Name, *t.ExitCode, t.Body)
}

func (t Test) Run(outFile *os.File) error {
	_, err := outFile.WriteString(t.Body)
	if err != nil {
		return errors.Join(errors.New("Failed to write test data to file\n"), err)
	}

	compileCmd := exec.Command(fmt.Sprintf("%s%s", buildPath, "Compiler"), outFile.Name())
	var stderr bytes.Buffer
	compileCmd.Stderr = &stderr

	if err := compileCmd.Run(); err != nil {
		_, ok := err.(*exec.ExitError)
		if !ok {
			return errors.New("Failed to run compilation command\n")
		}
		return errors.New(stderr.String())
	}

	fileNameNoExt := utils.GetFileNameWithoutExtension(outFile.Name())
	defer os.Remove(fmt.Sprintf("%s%s.o", buildPath, fileNameNoExt))
	defer os.Remove(fmt.Sprintf("%s%s", buildPath, fileNameNoExt))

	runCmd := exec.Command(fmt.Sprintf("%s%s", buildPath, fileNameNoExt))
	stderr = bytes.Buffer{}
	runCmd.Stderr = &stderr

	if err := runCmd.Run(); err != nil {
		exitErr, ok := err.(*exec.ExitError)
		if !ok {
			return errors.New("Failed to run executable\n")
		}

		if *t.ExitCode != exitErr.ExitCode() {
			return fmt.Errorf("[ERROR]:\tExpected program exit code %d, but got %d\n", *t.ExitCode, exitErr.ExitCode())
		}
	}

	return nil
}
