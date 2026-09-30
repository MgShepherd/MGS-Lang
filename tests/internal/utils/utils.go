package utils

import "strings"

func GetFileNameWithoutExtension(filePath string) string {
	pathComponents := strings.Split(filePath, "/")
	fullName := pathComponents[len(pathComponents)-1]
	nameComponents := strings.Split(fullName, ".")
	return nameComponents[0]
}
