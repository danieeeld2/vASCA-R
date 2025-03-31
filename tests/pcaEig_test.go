package tests

import (
	"encoding/csv"
	"fmt"
	"math"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"testing"
)

// Executes an external script (Octave/MATLAB or R)
func runScriptpcaEig(script string, args ...string) error {
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

// Read a CSV file and return its content as a 2D slice of floats
func readCSVFile(filepath string) ([][]float64, error) {
	file, err := os.Open(filepath)
	if err != nil {
		return nil, fmt.Errorf("error opening CSV file %s: %v", filepath, err)
	}
	defer file.Close()

	reader := csv.NewReader(file)
	var result [][]float64

	for {
		record, err := reader.Read()
		if err != nil {
			break
		}

		var row []float64
		for _, value := range record {
			f, err := strconv.ParseFloat(value, 64)
			if err != nil {
				return nil, fmt.Errorf("error parsing value %s in CSV: %v", value, err)
			}
			row = append(row, f)
		}
		result = append(result, row)
	}

	return result, nil
}

// Compare two 2D slices of floats (with a tolerance)
func compareMatrices_pcaEig(mat1, mat2 [][]float64, tolerance float64) bool {
	if len(mat1) != len(mat2) {
		return false
	}

	for i := 0; i < len(mat1); i++ {
		if len(mat1[i]) != len(mat2[i]) {
			return false
		}
		for j := 0; j < len(mat1[i]); j++ {
			if math.Abs(math.Abs(mat1[i][j])-math.Abs(mat2[i][j])) > tolerance { // The sign of vector elements can be inverted
				return false
			}
		}
	}
	return true
}

// testPcaEigCompareCSVOutput tests the comparison between the outputs of the PCA scripts (MATLAB/R)
func TestPcaEigCompareCSVOutput(t *testing.T) {
	tolerance := 1e-6
	dataset := "../datasets/tests_datasets/dataset_pcaEig.csv"
	M := 10 // Number of columns of the dataset (min(size(dataset)))

	// Loop through different PCA components (1 through M)
	for i := 1; i <= M; i++ {
		testName := fmt.Sprintf("pcaEig_PCAs-length:%d", i)
		t.Run(testName, func(t *testing.T) {
			// Create the PCs range for MATLAB and R: "1:i" (e.g., "1:3")
			pcsRange := fmt.Sprintf("1:%d", i)

			// Run Octave for the specific number of PCA components
			if err := runScriptpcaEig("octave", "--no-gui", "-q", "./pcaEig_runners/pcaEig_run.m", dataset, "PCs", pcsRange); err != nil {
				t.Fatalf("Error running Octave script: %v", err)
			}

			// Read the output CSV file (for Octave)
			octaveData, err := readCSVFile("pcaEig_matlab.csv")
			if err != nil {
				t.Fatalf("Error reading Octave output CSV: %v", err)
			}

			// Run R for the same number of PCA components
			if err := runScriptpcaEig("Rscript", "./pcaEig_runners/pcaEig_run.R", dataset, "PCs", pcsRange); err != nil {
				t.Fatalf("Error running R script: %v", err)
			}

			// Read the output CSV file (for R)
			rData, err := readCSVFile("pcaEig_r.csv")
			if err != nil {
				t.Fatalf("Error reading R output CSV: %v", err)
			}

			// Compare Octave and R results
			if !compareMatrices_pcaEig(octaveData, rData, tolerance) {
				t.Errorf("Results for PCA components %d do not match between Octave and R outputs", i)
			}

			t.Logf("PCA component length %d output comparison successful (within tolerance %.2e)", i, tolerance)
		})
	}

	// Clean up generated files
	os.Remove("pcaEig_matlab.csv")
	os.Remove("pcaEig_r.csv")
}
