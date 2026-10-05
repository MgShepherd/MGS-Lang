package test

import (
	"bytes"
	"errors"
	"fmt"
	"michael/mgstests/internal/utils"
	"os"
	"os/exec"
	"strings"
)

type Test struct {
	Name             *string
	ExitCode         *int
	CompilationError *string
	Body             string
}

const buildPath = "./build/"
const compilerExecName = "mgs"

func (t Test) IsHeaderFilled() bool {
	return t.Name != nil && (t.ExitCode != nil || t.CompilationError != nil)
}

func (t Test) IsHeaderEmpty() bool {
	return t.Name == nil && t.ExitCode == nil && t.CompilationError == nil
}

func (t Test) String() string {
	return fmt.Sprintf("TEST\nName: %s\nExpected Exit Code: %d\nBody:\n%s\n", *t.Name, *t.ExitCode, t.Body)
}

func (t Test) Run(outFile *os.File) error {
	_, err := outFile.WriteString(t.Body)
	if err != nil {
		return errors.Join(errors.New("Failed to write test data to file\n"), err)
	}

	compileCmd := exec.Command(fmt.Sprintf("%s%s", buildPath, compilerExecName), outFile.Name(), fmt.Sprintf("--output-folder=%s", buildPath))
	var stderr bytes.Buffer
	compileCmd.Stderr = &stderr

	if err := compileCmd.Run(); err != nil {
		_, ok := err.(*exec.ExitError)
		if !ok {
			return errors.New("Failed to run compilation command\n")
		}

		if t.CompilationError != nil {
			if strings.Contains(stderr.String(), *t.CompilationError) {
				return nil
			}

			return fmt.Errorf("[ERROR]:\tExpected compilation error to include: \"%s\", but error was:\n%s", *t.CompilationError, stderr.String())
		}
		return errors.New(stderr.String())
	}

	if t.CompilationError != nil {
		return fmt.Errorf("[ERROR]:\tExpected compilation error, but code compiled successfully\n")
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
