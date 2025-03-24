package tests

import (
	"encoding/csv"
	"fmt"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// Executes an external script (Octave/MATLAB or R)
func runScriptParglmVS(script string, args ...string) error {
	cmd := exec.Command(script, args...)

	// Capture standard output and error output
	var out, errOut strings.Builder
	cmd.Stdout = &out
	cmd.Stderr = &errOut

	err := cmd.Run()
	if err != nil {
		fmt.Printf("Standard output of %s:\n%s\n", script, out.String())
		fmt.Printf("Error output of %s:\n%s\n", script, errOut.String())
		return fmt.Errorf("error executing script %s: %v", script, err)
	}
	return nil
}

func readResultsCSV(filePath string) ([][]float64, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	reader := csv.NewReader(file)
	reader.TrimLeadingSpace = true
	reader.ReuseRecord = true
	reader.LazyQuotes = true
	allData, err := reader.ReadAll()
	if err != nil {
		return nil, err
	}

	var numericData [][]float64
	for _, record := range allData {
		var numericRow []float64
		for _, value := range record {
			value = strings.TrimSpace(strings.Trim(value, "\""))
			if value == "NA" || value == "NaN" {
				numericRow = append(numericRow, math.NaN())
				continue
			}
			if num, err := strconv.ParseFloat(value, 64); err == nil {
				numericRow = append(numericRow, num)
			}
		}
		if len(numericRow) > 0 {
			numericData = append(numericData, numericRow)
		}
	}
	return numericData, nil
}

func transpose(matrix [][]float64) [][]float64 {
	if len(matrix) == 0 {
		return nil
	}
	numRows := len(matrix)
	numCols := len(matrix[0])
	transposed := make([][]float64, numCols)
	for i := range transposed {
		transposed[i] = make([]float64, numRows)
	}
	for i := 0; i < numRows; i++ {
		for j := 0; j < numCols; j++ {
			transposed[j][i] = matrix[i][j]
		}
	}
	return transposed
}

func compareNumericSlices(matlabValues, rValues [][]float64, tolerance float64) bool {
	if len(matlabValues) != len(rValues) {
		return false
	}
	for i := 0; i < len(matlabValues); i++ {
		if len(matlabValues[i]) != len(rValues[i]) {
			return false
		}
		for j := 0; j < len(matlabValues[i]); j++ {
			if math.IsNaN(matlabValues[i][j]) && math.IsNaN(rValues[i][j]) {
				continue
			}
			if math.Abs(matlabValues[i][j]-rValues[i][j]) > tolerance {
				return false
			}
		}
	}
	return true
}

func TestCompareMATLABAndROutput(t *testing.T) {
	datasets := []struct {
		X, F string
	}{
		{"../datasets/tests_datasets/X1.csv", "../datasets/tests_datasets/F1.csv"},         // One factor
		{"../datasets/tests_datasets/X_test.csv", "../datasets/tests_datasets/F_test.csv"}, // Three factors
		// {"../datasets/tests_datasets/X2.csv", "../datasets/tests_datasets/F2.csv"},
	}
	tolerance := 1e-6
	models := []string{"linear", "interaction", "full"}
	preprocessing := []string{"0", "1", "2"}
	ts := []string{"0", "1", "2"}
	ordinal := []string{"0", "1"}
	fmtc := []string{"0", "1", "2", "3", "4"}
	coding := []string{"0", "1"}

	for _, dataset := range datasets {
		for _, model := range models {
			for _, prep := range preprocessing {
				for _, s := range ts {
					for _, ord := range ordinal {
						for _, f := range fmtc {
							for _, cod := range coding {
								testName := fmt.Sprintf("%s_%s_%s_%s_%s_%s_%s", filepath.Base(dataset.X), model, prep, s, ord, f, cod)
								t.Run(testName, func(t *testing.T) {
									args := []string{dataset.X, dataset.F, "Model", model, "Preprocessing", prep, "Permutations", "1000", "Ts", s, "Fmtc", f, "Ordinal", ord, "Coding", cod}

									// Run Octave
									if err := runScriptParglmVS("octave", append([]string{"--no-gui", "-q", "./parglmVS_runners/parglmVS_run.m"}, args...)...); err != nil {
										t.Fatalf("error executing Octave: %v", err)
									}

									// Run R
									if err := runScriptParglmVS("Rscript", append([]string{"./parglmVS_runners/parglmVS_run.R"}, args...)...); err != nil {
										t.Fatalf("error executing R: %v", err)
									}

									// Read results
									matlabValues, err := readResultsCSV("parglmVS_matlab.csv")
									if err != nil {
										t.Fatalf("error reading MATLAB: %v", err)
									}

									rValues, err := readResultsCSV("parglmVS_r.csv")
									if err != nil {
										t.Fatalf("error reading R: %v", err)
									}

									if !compareNumericSlices(matlabValues, transpose(rValues), tolerance) {
										t.Errorf("Differences between MATLAB and R in %s", testName)
									}
								})
							}
						}
					}
				}
			}
		}
	}
	os.Remove("parglmVS_matlab.csv")
	os.Remove("parglmVS_r.csv")
}
