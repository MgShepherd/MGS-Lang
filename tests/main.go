package main

import (
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type test struct {
	name     *string
	exitCode *int
	body     string
}

func (t test) isHeaderFilled() bool {
	return t.name != nil && t.exitCode != nil
}

func (t test) isHeaderEmpty() bool {
	return t.name == nil && t.exitCode == nil
}

func (t test) String() string {
	return fmt.Sprintf("TEST\nName: %s\nExpected Exit Code: %d\nBody:\n%s\n", *t.name, *t.exitCode, t.body)
}

const (
	testPath     string = "./tests"
	mgsExtension string = "mgs"
)

func main() {
	files, err := os.ReadDir(testPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to read test directory: %v\n", err)
		os.Exit(1)
	}

	for _, file := range files {
		nameComponents := strings.Split(file.Name(), ".")
		if file.IsDir() || len(nameComponents) == 1 || nameComponents[len(nameComponents)-1] != mgsExtension {
			continue
		}

		tests, err := processTestFile(file.Name())
		if err != nil {
			fmt.Fprintf(os.Stderr, "%v\n", err)
			os.Exit(1)
		}

		for _, t := range tests {
			fmt.Println(t)
		}
	}
}

func processTestFile(name string) ([]test, error) {
	data, err := os.ReadFile(fmt.Sprintf("%s/%s", testPath, name))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to read file: %s/%s\n", testPath, name)
		return []test{}, err
	}

	tests := []test{}
	var currentTest test
	var currentTestBody strings.Builder

	for lineNum, nextLine := range strings.Split(string(data), "\n") {
		nextLine := strings.TrimSpace(nextLine)

		// If we have the header filled for a test, but reach a directive line, we have reached the end of a test block
		if currentTest.isHeaderFilled() && len(nextLine) >= 2 && nextLine[:2] == "--" {
			currentTest.body = currentTestBody.String()
			if len(currentTest.body) == 0 {
				return []test{}, fmt.Errorf("Invalid empty body provided for test")
			}

			tests = append(tests, currentTest)
			currentTestBody.Reset()
			currentTest = test{}
		}

		// If we have not filled test header, we expect more directives
		if !currentTest.isHeaderFilled() {
			if err := fillTestHeaderFromDirective(&currentTest, nextLine); err != nil {
				return []test{}, errors.Join(fmt.Errorf("Invalid test structure in file %s at line %d", name, lineNum+1), err)
			}
			continue
		}

		if len(nextLine) > 0 {
			if _, err := currentTestBody.WriteString(nextLine); err != nil {
				return []test{}, errors.Join(fmt.Errorf("Failed to append line to test body"), err)
			}
			if _, err = currentTestBody.WriteRune('\n'); err != nil {
				return []test{}, errors.Join(fmt.Errorf("Failed to append newline to test body"), err)
			}
		}
	}

	// Process last test in the file
	if currentTest.isHeaderFilled() && len(currentTestBody.String()) != 0 {
		currentTest.body = currentTestBody.String()
		tests = append(tests, currentTest)
		currentTest = test{}
	}

	if !currentTest.isHeaderEmpty() {
		return []test{}, fmt.Errorf("partially completed test block at end of file")
	}

	return tests, nil
}

func fillTestHeaderFromDirective(t *test, directiveLine string) error {
	if len(directiveLine) == 0 {
		return nil
	}

	elements := strings.Fields(directiveLine)
	if len(elements) < 1 || elements[0] != "--" {
		return fmt.Errorf("All directive lines in test header must begin with --")
	}

	if len(elements) < 2 {
		return fmt.Errorf("No directive provided after -- in test header")
	}

	switch elements[1] {
	case "TEST":
		if err := processTestDirective(t, elements); err != nil {
			return err
		}
	case "EXPECTED_STATUS_CODE":
		if err := processExpectedStatusCodeDirective(t, elements); err != nil {
			return err
		}
	default:
		return fmt.Errorf("Unknown directive: %s\n", elements[1])
	}

	return nil
}

func processTestDirective(t *test, elements []string) error {
	if t.name != nil {
		return fmt.Errorf("Multiple TEST directives provided in same test header")
	}

	nameStartIdx := 2
	if len(elements) <= nameStartIdx {
		return fmt.Errorf("Empty test name provided in TEST directive")
	}

	t.name = new(strings.Join(elements[nameStartIdx:], " "))

	return nil
}

func processExpectedStatusCodeDirective(t *test, elements []string) error {
	if t.exitCode != nil {
		return fmt.Errorf("Multiple EXPECTED_STATUS_CODE directives provided in same test header")
	}

	exitCodeIdx := 2
	if len(elements) != exitCodeIdx+1 {
		return fmt.Errorf("Invalid EXPECTED_STATUS_CODE directive")
	}

	exitCode, err := strconv.Atoi(elements[exitCodeIdx])
	if err != nil {
		return fmt.Errorf("Failed to convert EXPECTED_STATUS_CODE value into integer")
	}

	t.exitCode = new(exitCode)

	return nil
}
