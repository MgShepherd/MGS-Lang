package main

import (
	"fmt"
	"michael/mgstests/internal/file"
	"os"
)

const (
	colorRed   = "\033[0;31m"
	colorGreen = "\033[32m"
	colorNone  = "\033[0m"
)

func main() {
	//TODO: Ensure that tests are run from the root of the project and that the compiler executable exits

	tests, err := file.ProcessTestDirectory("./tests/inputs")
	if err != nil {
		fmt.Printf("Failed to process tests: %v\n", err)
		os.Exit(1)
	}

	failedTests, totalTests := 0, 0
	for _, test := range tests {
		tempFile, err := os.CreateTemp("", "*.mgs")
		if err != nil {
			fmt.Printf("Failed to create temporary file required for running tests\n")
			os.Exit(1)
		}
		defer os.Remove(tempFile.Name())

		totalTests += 1
		if err := test.Run(tempFile); err != nil {
			fmt.Printf("%s[FAILED]:\t%s\n", colorRed, *test.Name)
			fmt.Printf("%v%s\n", err, colorNone)
			failedTests += 1
		} else {
			fmt.Printf("%s[PASSED]:\t%s%s\n", colorGreen, *test.Name, colorNone)
		}
	}

	if failedTests != 0 {
		fmt.Printf("Not all tests passed, %d out of %d failed\n", failedTests, totalTests)
		os.Exit(1)
	}

	fmt.Printf("All tests passed!\n")
}
