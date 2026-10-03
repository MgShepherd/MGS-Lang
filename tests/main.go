package main

import (
	"errors"
	"fmt"
	"michael/mgstests/internal/file"
	"michael/mgstests/internal/test"
	"os"
	"runtime"
	"sync"
	"sync/atomic"
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

	var waitGroup sync.WaitGroup
	threadLimiter := make(chan struct{}, runtime.NumCPU())

	var failedTests atomic.Int32
	for _, t := range tests {
		// Write to the threadLimiter channel so that this will block when the channel is full
		threadLimiter <- struct{}{}

		waitGroup.Go(func() {
			// Once this func has finished, free one on the threadLimiter slots
			defer func() { <-threadLimiter }()

			if err := setupAndRunTest(&t); err != nil {
				fmt.Printf("%s[FAILED]:\t%s\n%v%s\n", colorRed, *t.Name, err, colorNone)
				failedTests.Add(1)
				return
			}
			fmt.Printf("%s[PASSED]:\t%s%s\n", colorGreen, *t.Name, colorNone)
		})
	}
	waitGroup.Wait()

	if numFailed := failedTests.Load(); numFailed != 0 {
		fmt.Printf("Not all tests passed, %d out of %d failed\n", numFailed, len(tests))
		os.Exit(1)
	}

	fmt.Printf("All tests passed!\n")
}

func setupAndRunTest(t *test.Test) error {
	tempFile, err := os.CreateTemp("", "*.mgs")
	if err != nil {
		return errors.Join(errors.New("Failed to create temporary file for test\n"), err)
	}
	defer tempFile.Close()
	defer os.Remove(tempFile.Name())

	return t.Run(tempFile)
}
