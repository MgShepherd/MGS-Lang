package file

import (
	"errors"
	"fmt"
	"michael/mgstests/internal/test"
	"os"
	"strconv"
	"strings"
)

const mgsExtension = "mgs"

func ProcessTestDirectory(testDir string) ([]test.Test, error) {
	files, err := os.ReadDir(testDir)
	if err != nil {
		return []test.Test{}, errors.Join(errors.New("failed to read test directory"), err)
	}

	tests := []test.Test{}
	for _, file := range files {
		nameComponents := strings.Split(file.Name(), ".")
		if file.IsDir() || len(nameComponents) == 1 || nameComponents[len(nameComponents)-1] != mgsExtension {
			continue
		}

		fileTests, err := ProcessTestFile(testDir, file.Name())
		if err != nil {
			return []test.Test{}, errors.Join(errors.New("failed to process test file"), err)
		}

		tests = append(tests, fileTests...)
	}

	return tests, nil
}

func ProcessTestFile(baseDir, fileName string) ([]test.Test, error) {
	data, err := os.ReadFile(fmt.Sprintf("%s/%s", baseDir, fileName))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to read file: %s/%s\n", baseDir, fileName)
		return []test.Test{}, err
	}

	tests := []test.Test{}
	var currentTest test.Test
	var currentTestBody strings.Builder

	for lineNum, nextLine := range strings.Split(string(data), "\n") {
		nextLine := strings.TrimSpace(nextLine)

		// If we have the header filled for a test, but reach a directive line, we have reached the end of a test block
		if currentTest.IsHeaderFilled() && len(nextLine) >= 2 && nextLine[:2] == "--" {
			currentTest.Body = currentTestBody.String()
			if len(currentTest.Body) == 0 {
				return []test.Test{}, fmt.Errorf("Invalid empty body provided for test")
			}

			tests = append(tests, currentTest)
			currentTestBody.Reset()
			currentTest = test.Test{}
		}

		// If we have not filled test header, we expect more directives
		if !currentTest.IsHeaderFilled() {
			if err := fillTestHeaderFromDirective(&currentTest, nextLine, fileName); err != nil {
				return []test.Test{}, errors.Join(fmt.Errorf("Invalid test structure in file %s at line %d", fileName, lineNum+1), err)
			}
			continue
		}

		if len(nextLine) > 0 {
			if _, err := currentTestBody.WriteString(nextLine); err != nil {
				return []test.Test{}, errors.Join(fmt.Errorf("Failed to append line to test body"), err)
			}
			if _, err = currentTestBody.WriteRune('\n'); err != nil {
				return []test.Test{}, errors.Join(fmt.Errorf("Failed to append newline to test body"), err)
			}
		}
	}

	// Process last test in the file
	if currentTest.IsHeaderFilled() && len(currentTestBody.String()) != 0 {
		currentTest.Body = currentTestBody.String()
		tests = append(tests, currentTest)
		currentTest = test.Test{}
	}

	if !currentTest.IsHeaderEmpty() {
		return []test.Test{}, fmt.Errorf("partially completed test block at end of file")
	}

	return tests, nil
}

func fillTestHeaderFromDirective(t *test.Test, directiveLine, fileName string) error {
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
		if err := processTestDirective(t, elements, fileName); err != nil {
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

func processTestDirective(t *test.Test, elements []string, fileName string) error {
	if t.Name != nil {
		return fmt.Errorf("Multiple TEST directives provided in same test header")
	}

	nameStartIdx := 2
	if len(elements) <= nameStartIdx {
		return fmt.Errorf("Empty test name provided in TEST directive")
	}

	testName := strings.Join(elements[nameStartIdx:], " ")
	t.Name = new(fmt.Sprintf("%s: %s", fileName, testName))

	return nil
}

func processExpectedStatusCodeDirective(t *test.Test, elements []string) error {
	if t.ExitCode != nil {
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

	t.ExitCode = new(exitCode)

	return nil
}
