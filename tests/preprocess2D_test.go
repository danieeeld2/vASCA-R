package tests

import (
	"bytes"
	"encoding/csv"
	"fmt"
	"math"
	"os"
	"os/exec"
	"strconv"
	"testing"
)

// Function to run Octave script and capture the output
func runOctave(script, dataset, preprocessing, weights string) (string, error) {
	var cmd *exec.Cmd
	if weights == "" {
		cmd = exec.Command("octave", "--silent", "--no-gui", "-q", script, dataset, preprocessing)
	} else {
		cmd = exec.Command("octave", "--silent", "--no-gui", "-q", script, dataset, preprocessing, weights)
		// print the command
		fmt.Println(cmd)
	}
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	err := cmd.Run()
	return out.String(), err
}

// Function to run R script and capture the output
func runR(script, dataset, preprocessing, weights string) (string, error) {
	var cmd *exec.Cmd
	if weights == "" {
		cmd = exec.Command("Rscript", script, dataset, preprocessing)
	} else {
		cmd = exec.Command("Rscript", script, dataset, preprocessing, weights)
		// print the command
		fmt.Println(cmd)
	}
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	err := cmd.Run()
	return out.String(), err
}

// Function to check if a file exists
func fileExists(filename string) bool {
	_, err := os.Stat(filename)
	return err == nil
}

// Function to read CSV and return a 2D slice of float64
func readCSV(filename string) ([][]float64, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, fmt.Errorf("failed to open file %s: %v", filename, err)
	}
	defer file.Close()

	reader := csv.NewReader(file)
	var data [][]float64

	// Read all lines from the CSV
	records, err := reader.ReadAll()
	if err != nil {
		return nil, fmt.Errorf("failed to read CSV data from %s: %v", filename, err)
	}

	// Convert the records to a 2D slice of float64
	for _, record := range records {
		var row []float64
		for _, value := range record {
			// Parse each CSV value as a float64
			val, err := strconv.ParseFloat(value, 64)
			if err != nil {
				return nil, fmt.Errorf("failed to convert value %s to float64: %v", value, err)
			}
			row = append(row, val)
		}
		data = append(data, row)
	}

	return data, nil
}

// Function to compare two datasets with a specified tolerance and return a boolean result
func compareResults(dataset1, dataset2 string, tolerance float64) bool {
	// Read the datasets
	data1, err := readCSV(dataset1)
	if err != nil {
		fmt.Printf("Failed to read dataset %s: %v\n", dataset1, err)
		return false
	}
	data2, err := readCSV(dataset2)
	if err != nil {
		fmt.Printf("Failed to read dataset %s: %v\n", dataset2, err)
		return false
	}

	// Ensure the dimensions of both datasets are the same
	if len(data1) != len(data2) || len(data1[0]) != len(data2[0]) {
		fmt.Printf("Datasets have different dimensions: %s and %s\n", dataset1, dataset2)
		return false
	}

	// Compare data element by element with the specified tolerance
	for i := range data1 {
		for j := range data1[i] {
			if math.Abs(data1[i][j]-data2[i][j]) > tolerance {
				fmt.Printf("Data mismatch at [%d][%d]: %s = %v, %s = %v (tolerance: %v)\n", i, j, dataset1, data1[i][j], dataset2, data2[i][j], tolerance)
				return false
			}
		}
	}
	return true
}

// Function for cleaning up the processed files
func cleanupFiles() {
	os.Remove("preprocess2D_matlab.csv")
	os.Remove("average_matlab.csv")
	os.Remove("scale_matlab.csv")
	os.Remove("preprocess2D_r.csv")
	os.Remove("average_r.csv")
	os.Remove("scale_r.csv")
}

// Test function to verify that the output from Octave and R are the same
func TestProcessData(t *testing.T) {
	// List of datasets to test with
	datasets := []string{
		"../datasets/tests_datasets/dataset1.csv",
		"../datasets/tests_datasets/dataset2.csv",
		"../datasets/tests_datasets/dataset1.csv",
	}

	// Iterate through each dataset
	for _, dataset := range datasets {
		t.Run(fmt.Sprintf("Testing with %s", dataset), func(t *testing.T) {

			// Testing with different preprocessing methods
			for i := range 3 {
				preprocessing := fmt.Sprintf("%d", i)

				// Generating the processed datasets using Octave
				_, err := runOctave("./preprocess2D_runners/preprocess2D_run.m", dataset, preprocessing, "")
				if err != nil {
					t.Errorf("Failed to execute Octave script for %s: %v", dataset, err)
				}

				// Generating the processed datasets using R
				_, err = runR("./preprocess2D_runners/preprocess2D_run.R", dataset, preprocessing, "")
				if err != nil {
					t.Errorf("Failed to execute R script for %s: %v", dataset, err)
				}

				// Check that the processed files exist
				if !fileExists("preprocess2D_matlab.csv") || !fileExists("average_matlab.csv") || !fileExists("scale_matlab.csv") {
					t.Errorf("Octave processed files not found for %s", dataset)
				}
				if !fileExists("preprocess2D_r.csv") || !fileExists("average_r.csv") || !fileExists("scale_r.csv") {
					t.Errorf("R processed files not found for %s", dataset)
				}

				// Compare the three outputs with the specified tolerance
				tolerance := 1e-6
				if !compareResults("preprocess2D_matlab.csv", "preprocess2D_r.csv", tolerance) {
					t.Errorf("The 'xcs' data from Octave and R are different for %s", dataset)
				}
				if !compareResults("average_matlab.csv", "average_r.csv", tolerance) {
					t.Errorf("The 'average' data from Octave and R are different for %s", dataset)
				}
				if !compareResults("scale_matlab.csv", "scale_r.csv", tolerance) {
					t.Errorf("The 'scale' data from Octave and R are different for %s", dataset)
				}

				// Clean up the processed files
				cleanupFiles()
			}

			// Generate new weights in string format
			data, _ := readCSV(dataset)
			weights_octave, weights_r := "", ""
			for i := range data[0] {
				weights_octave += fmt.Sprintf("%f", 1.0/float64(len(data[0])))
				weights_r += fmt.Sprintf("%f", 1.0/float64(len(data[0])))
				if i < len(data[0])-1 {
					// R weights are comma separated
					weights_r += ","
					// Octave weights are space separated
					weights_octave += " "
				}
			}

			// Testing with weights
			// Generating the processed datasets using Octave
			_, err := runOctave("./preprocess2D_runners/preprocess2D_run.m", dataset, "2", fmt.Sprintf("[%s]", weights_octave))
			if err != nil {
				t.Errorf("Failed to execute Octave script for %s: %v", dataset, err)
			}

			// Generating the processed datasets using R
			_, err = runR("./preprocess2D_runners/preprocess2D_run.R", dataset, "2", weights_r)
			if err != nil {
				t.Errorf("Failed to execute R script for %s: %v", dataset, err)
			}

			// Check that the processed files exist
			if !fileExists("preprocess2D_matlab.csv") || !fileExists("average_matlab.csv") || !fileExists("scale_matlab.csv") {
				t.Errorf("Octave processed files not found for %s", dataset)
			}
			if !fileExists("preprocess2D_r.csv") || !fileExists("average_r.csv") || !fileExists("scale_r.csv") {
				t.Errorf("R processed files not found for %s", dataset)
			}

			// Compare the three outputs with the specified tolerance
			tolerance := 1e-6
			if !compareResults("preprocess2D_matlab.csv", "preprocess2D_r.csv", tolerance) {
				t.Errorf("The 'xcs' data from Octave and R are different for %s", dataset)
			}
			if !compareResults("average_matlab.csv", "average_r.csv", tolerance) {
				t.Errorf("The 'average' data from Octave and R are different for %s", dataset)
			}
			if !compareResults("scale_matlab.csv", "scale_r.csv", tolerance) {
				t.Errorf("The 'scale' data from Octave and R are different for %s", dataset)
			}
		})
	}

	// Clean up the processed files
	cleanupFiles()
}
