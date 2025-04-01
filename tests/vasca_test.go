package tests

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"math"
	"os"
	"os/exec"
	"testing"
)

type Result struct {
	Values map[string]float64 `json:"values"`
}

// compareJSON checks if two JSON results are equal within a tolerance.
func compareJSON(result1, result2 Result, tolerance float64) bool {
	for key, val1 := range result1.Values {
		if val2, exists := result2.Values[key]; exists {
			if math.Abs(val1-val2) > tolerance {
				return false
			}
		} else {
			return false
		}
	}
	return true
}

func runCommand(command string, args ...string) error {
	cmd := exec.Command(command, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func loadResult(filename string) (Result, error) {
	var result Result
	data, err := ioutil.ReadFile(filename)
	if err != nil {
		return result, err
	}
	if err := json.Unmarshal(data, &result); err != nil {
		return result, err
	}
	return result, nil
}

func TestVascaComparison(t *testing.T) {
	datasets := []string{"../datasets/tests_datasets/parglmVS_1.json", "../datasets/tests_datasets/parglmVS_2.json"}
	siglevs := []string{"0.01", "0.05"}
	tolerance := 1e-6

	for _, dataset := range datasets {
		for _, siglev := range siglevs {
			t.Run(fmt.Sprintf("Dataset: %s, Siglev: %s", dataset, siglev), func(t *testing.T) {
				// Run MATLAB script
				if err := runCommand("octave", "--no-gui", "-q", "./vasca_runners/vasca_run.m", dataset, siglev); err != nil {
					t.Fatalf("Error running MATLAB script: %v", err)
				}

				// Run R script
				if err := runCommand("Rscript", "./vasca_runners/vasca_run.R", dataset, siglev); err != nil {
					t.Fatalf("Error running R script: %v", err)
				}

				// Load results
				matlabResult, err1 := loadResult("vasca_matlab.json")
				rResult, err2 := loadResult("vasca_r.json")
				if err1 != nil || err2 != nil {
					t.Fatalf("Error loading results: %v, %v", err1, err2)
				}

				// Compare results
				if !compareJSON(matlabResult, rResult, tolerance) {
					t.Errorf("Results do not match within tolerance")
				}
			})
		}
	}
}
