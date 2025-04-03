package tests

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// Executes an external script (Octave/MATLAB or R)
func runPlotScatter(script string, args ...string) error {
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

func generateComparisonHTML(testCases []struct {
	name       string
	rData      string
	octaveData string
	rArgs      []string
	octaveArgs []string
	skipR      bool
	skipOctave bool
}, outputDir string) error {
	htmlContent := `
	<!DOCTYPE html>
	<html>
	<head>
		<title>MATLAB vs R Scatter Plot Comparison</title>
		<style>
			body { font-family: Arial, sans-serif; margin: 20px; }
			h1 { color: #333; text-align: center; }
			.test-case { 
				margin-bottom: 40px; 
				padding: 20px; 
				background: #f9f9f9; 
				border-radius: 8px;
				box-shadow: 0 2px 4px rgba(0,0,0,0.1);
			}
			h2 { color: #444; border-bottom: 1px solid #ddd; padding-bottom: 8px; }
			.comparison { 
				display: flex; 
				justify-content: center;
				flex-wrap: wrap;
				gap: 20px;
				margin-top: 15px;
			}
			.image-container { 
				text-align: center; 
				padding: 10px;
				background: white;
				border-radius: 5px;
				box-shadow: 0 1px 3px rgba(0,0,0,0.1);
			}
			img { 
				max-width: 450px; 
				height: auto;
				border: 1px solid #eee;
			}
			.caption { 
				font-weight: bold; 
				margin-top: 8px;
				color: #555;
			}
			.metadata {
				background: #f0f0f0;
				padding: 10px;
				border-radius: 5px;
				margin-top: 15px;
				font-family: monospace;
				font-size: 0.9em;
			}
			footer {
				text-align: center;
				margin-top: 30px;
				color: #777;
				font-size: 0.9em;
			}
		</style>
	</head>
	<body>
		<h1>Scatter Plot Comparison: MATLAB vs R</h1>
	`

	for _, tc := range testCases {
		if tc.skipR || tc.skipOctave {
			continue
		}

		safeName := strings.ReplaceAll(tc.name, " ", "_")
		rCmd := strings.Join(append([]string{"Rscript", "./plotScatter_runners/plotScatter_run.R", tc.rData}, tc.rArgs...), " ")
		octaveCmd := strings.Join(append([]string{"octave", "--no-gui", "-q", "./plotScatter_runners/plotScatter_run.m", tc.octaveData}, tc.octaveArgs...), " ")

		htmlContent += fmt.Sprintf(`
		<div class="test-case">
			<h2>%s</h2>
			<div class="comparison">
				<div class="image-container">
					<img src="plotScatter_matlab_%s.png" alt="MATLAB output">
					<div class="caption">MATLAB</div>
				</div>
				<div class="image-container">
					<img src="plotScatter_r_%s.png" alt="R output">
					<div class="caption">R</div>
				</div>
			</div>
			<div class="metadata">
				<p><strong>R Command:</strong> %s</p>
				<p><strong>MATLAB Command:</strong> %s</p>
			</div>
		</div>
		`, tc.name, safeName, safeName, rCmd, octaveCmd)
	}

	htmlContent += fmt.Sprintf(`
		<footer>
			Generated at %s | plotScatter_test_results
		</footer>
	</body>
	</html>
	`, time.Now().Format("2006-01-02 15:04:05"))

	// Create HTML file in the output directory
	return os.WriteFile(filepath.Join(outputDir, "plotScatter_comparison.html"), []byte(htmlContent), 0644)
}

func TestPlotScatterBasic(t *testing.T) {
	tests := []struct {
		name       string
		rData      string
		octaveData string
		rArgs      []string
		octaveArgs []string
		skipR      bool
		skipOctave bool
	}{
		{
			name:       "basic_random_data",
			rData:      "cbind(runif(100), runif(100))",
			octaveData: "[rand(100,1), rand(100,1)]",
		},
		{
			name:       "with_labels_and_classes",
			rData:      "cbind(rnorm(5), rnorm(5))",
			octaveData: "rand(5,2)",
			rArgs:      []string{"EleLabel", "c('A','B','C','D','E')", "ObsClass", "c(1,1,2,2,3)", "XYLabel", "c('X-axis','Y-axis')"},
			octaveArgs: []string{"EleLabel", "['A';'B';'C';'D';'E']", "ObsClass", "[1;1;2;2;3]", "XYLabel", "['X-axis';'Y-axis']"},
		},
		{
			name:       "filled_markers",
			rData:      "cbind(rnorm(50), rnorm(50))",
			octaveData: "randn(50,2)",
			rArgs:      []string{"FilledMarkers", "true"},
			octaveArgs: []string{"FilledMarkers", "true"},
		},
		{
			name:       "multiplicity_size",
			rData:      "cbind(runif(15), runif(15))",
			octaveData: "rand(15,2)",
			rArgs:      []string{"FilledMarkers", "TRUE", "ObsClass", "c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5)", "Multiplicity", "c(1,5,10,1,5,10,1,5,10,1,5,10,1,5,10)", "PlotMult", "size"},
			octaveArgs: []string{"FilledMarkers", "true", "ObsClass", "[1;1;1;2;2;2;3;3;3;4;4;4;5;5;5]", "Multiplicity", "[1;5;10;1;5;10;1;5;10;1;5;10;1;5;10]", "PlotMult", "'size'"},
		},
		{
			name:       "multiplicity_shape",
			rData:      "cbind(rnorm(15), rnorm(15))",
			octaveData: "randn(15,2)",
			rArgs:      []string{"FilledMarkers", "TRUE", "ObsClass", "c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5)", "Multiplicity", "c(1,5,10,1,5,10,1,5,10,1,5,10,1,5,10)", "PlotMult", "shape"},
			octaveArgs: []string{"FilledMarkers", "true", "ObsClass", "[1;1;1;2;2;2;3;3;3;4;4;4;5;5;5]", "Multiplicity", "[1;5;10;1;5;10;1;5;10;1;5;10;1;5;10]", "PlotMult", "'shape'"},
		},
		{
			name:       "numerical_classes",
			rData:      "cbind(runif(20), runif(20))",
			octaveData: "rand(20,2)",
			rArgs:      []string{"ClassType", "Numerical", "ObsClass", "c(1:20)", "Color", "hsv"},
			octaveArgs: []string{"ClassType", "'Numerical'", "ObsClass", "(1:20)'", "Color", "'hsv'"},
		},
		{
			name:       "categorical_classes",
			rData:      "cbind(rnorm(15), rnorm(15))",
			octaveData: "randn(15,2)",
			rArgs:      []string{"ClassType", "Categorical", "ObsClass", "c(1:15)", "Color", "hsv"},
			octaveArgs: []string{"ClassType", "'Categorical'", "ObsClass", "(1:15)'", "Color", "'hsv'"},
		},
	}

	// Create single output directory
	outputDir := "plotScatter_test_results"
	if err := os.MkdirAll(outputDir, 0777); err != nil {
		t.Fatalf("Failed to create output directory: %v", err)
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			safeName := strings.ReplaceAll(tt.name, " ", "_")

			// Test R version
			if !tt.skipR {
				t.Run("R", func(t *testing.T) {
					args := append([]string{tt.rData}, tt.rArgs...)
					if err := runPlotScatter("Rscript", append([]string{"./plotScatter_runners/plotScatter_run.R"}, args...)...); err != nil {
						t.Fatalf("R version failed: %v", err)
					}
					// Move and rename R output
					newPath := filepath.Join(outputDir, fmt.Sprintf("plotScatter_r_%s.png", safeName))
					if err := os.Rename("plotScatter_r.png", newPath); err != nil {
						t.Logf("Warning: could not rename R output file: %v", err)
					}
				})
			}

			// Test Octave/MATLAB version
			if !tt.skipOctave {
				t.Run("Octave", func(t *testing.T) {
					octaveArgs := append([]string{"--no-gui", "-q", "./plotScatter_runners/plotScatter_run.m", tt.octaveData}, tt.octaveArgs...)
					if err := runPlotScatter("octave", octaveArgs...); err != nil {
						t.Fatalf("Octave version failed: %v", err)
					}
					// Move and rename MATLAB output
					newPath := filepath.Join(outputDir, fmt.Sprintf("plotScatter_matlab_%s.png", safeName))
					if err := os.Rename("plotScatter_matlab.png", newPath); err != nil {
						t.Logf("Warning: could not rename MATLAB output file: %v", err)
					}
				})
			}
		})
	}

	// Generate comparison report in the same directory
	if err := generateComparisonHTML(tests, outputDir); err != nil {
		t.Errorf("Failed to generate comparison report: %v", err)
	} else {
		t.Logf("Comparison report generated: %s/plotScatter_comparison.html", outputDir)
	}
}
