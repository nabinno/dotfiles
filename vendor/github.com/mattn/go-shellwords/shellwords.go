package shellwords

import "strings"

func Parse(s string) ([]string, error) {
	return strings.Fields(s), nil
}
