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
func runPlotVec(script string, args ...string) error {
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

func generateComparisonMarkdownPlotVec(testCases []struct {
	name       string
	rData      string
	octaveData string
	rArgs      []string
	octaveArgs []string
	skipR      bool
	skipOctave bool
}, outputDir string) error {
	mdContent := "# Vector Plot Comparison: MATLAB vs R\n\n"
	mdContent += fmt.Sprintf("**Generated at**: %s  \n", time.Now().Format("2006-01-02 15:04:05"))
	mdContent += fmt.Sprintf("**Test Results**  \n\n")

	for _, tc := range testCases {
		if tc.skipR || tc.skipOctave {
			continue
		}

		safeName := strings.ReplaceAll(tc.name, " ", "_")
		rCmd := strings.Join(append([]string{"Rscript", "./plotVec_runners/plotVec_run.R", tc.rData}, tc.rArgs...), " ")
		octaveCmd := strings.Join(append([]string{"octave", "--no-gui", "-q", "./plotVec_runners/plotVec_run.m", tc.octaveData}, tc.octaveArgs...), " ")

		mdContent += fmt.Sprintf("## %s\n\n", tc.name)
		mdContent += "| MATLAB | R |\n"
		mdContent += "|--------|---|\n"
		mdContent += fmt.Sprintf("| ![MATLAB Plot](plotVec_matlab_%s.png) | ![R Plot](plotVec_r_%s.png) |\n\n", safeName, safeName)

		mdContent += "### Commands\n"
		mdContent += "```bash\n"
		mdContent += fmt.Sprintf("# R Command\n%s\n\n", rCmd)
		mdContent += fmt.Sprintf("# MATLAB Command\n%s\n", octaveCmd)
		mdContent += "```\n\n"
		mdContent += "---\n\n"
	}

	mdPath := filepath.Join(outputDir, "RESULTS.md")
	return os.WriteFile(mdPath, []byte(mdContent), 0644)
}

func TestPlotVec(t *testing.T) {
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
			name:       "basic_bar_plot",
			rData:      "matrix(rnorm(100), ncol=1)",
			octaveData: "randn(100,1)",
			rArgs:      []string{"XYLabel", "c('Index','Value')"},
			octaveArgs: []string{"XYLabel", "['Index';'Value']"},
		},
		{
			name:       "basic_line_plot",
			rData:      "matrix(rnorm(100), ncol=1)",
			octaveData: "randn(100,1)",
			rArgs:      []string{"PlotType", "'Lines'", "XYLabel", "c('Index','Value')"},
			octaveArgs: []string{"PlotType", "'Lines'", "XYLabel", "['Index';'Value']"},
		},
		{
			name:       "multi_vector_bar_plot",
			rData:      "matrix(rnorm(300), ncol=3)",
			octaveData: "randn(100,3)",
			rArgs:      []string{"XYLabel", "c('Index','Value')"},
			octaveArgs: []string{"XYLabel", "['Index';'Value']"},
		},
		{
			name:       "multi_vector_line_plot",
			rData:      "matrix(rnorm(300), ncol=3)",
			octaveData: "randn(100,3)",
			rArgs:      []string{"PlotType", "'Lines'", "XYLabel", "c('Index','Value')"},
			octaveArgs: []string{"PlotType", "'Lines'", "XYLabel", "['Index';'Value']"},
		},
		{
			name:       "with_labels_and_classes",
			rData:      "matrix(rnorm(15), ncol=3)",
			octaveData: "randn(5,3)",
			rArgs:      []string{"EleLabel", "c('one','two','three','four','five')", "ObsClass", "c(1,1,1,2,2)", "XYLabel", "c('Elements','Values')"},
			octaveArgs: []string{"EleLabel", "['one';'two';'three';'four';'five']", "ObsClass", "[1;1;1;2;2]", "XYLabel", "['Elements';'Values']"},
		},
		{
			name:       "with_control_limits",
			rData:      "matrix(rnorm(100), ncol=1)",
			octaveData: "randn(100,1)",
			rArgs:      []string{"LimCont", "c(1, -1, 3)", "XYLabel", "c('Functions','Time')"},
			octaveArgs: []string{"LimCont", "[1, -1, 3]", "XYLabel", "['Functions';'Time']"},
		},
		{
			name:       "with_variable_control_limits",
			rData:      "matrix(rnorm(10), ncol=1)",
			octaveData: "randn(10,1)",
			rArgs:      []string{"LimCont", "rnorm(10)", "XYLabel", "c('Elements','Values')"},
			octaveArgs: []string{"LimCont", "randn(10,1)", "XYLabel", "['Elements';'Values']"},
		},
		{
			name:       "numerical_classes",
			rData:      "matrix(rnorm(30), ncol=3)",
			octaveData: "randn(10,3)",
			rArgs:      []string{"ObsClass", "1:10", "ClassType", "'Numerical'", "XYLabel", "c('Index','Value')"},
			octaveArgs: []string{"ObsClass", "(1:10)'", "ClassType", "'Numerical'", "XYLabel", "['Index';'Value']"},
		},
		{
			name:       "with_multiplicity",
			rData:      "matrix(rnorm(15), ncol=3)",
			octaveData: "randn(5,3)",
			rArgs:      []string{"EleLabel", "c('one','two','three','four','five')", "ObsClass", "c(1,1,1,2,2)", "Multiplicity", "runif(5,1,100)", "Markers", "c(20,50,100)", "XYLabel", "c('Elements','Values')"},
			octaveArgs: []string{"EleLabel", "['one';'two';'three';'four';'five']", "ObsClass", "[1;1;1;2;2]", "Multiplicity", "100*rand(5,1)", "Markers", "[20,50,100]", "XYLabel", "['Elements';'Values']"},
		},
		{
			name:       "different_color_palettes",
			rData:      "matrix(rnorm(30), ncol=3)",
			octaveData: "randn(10,3)",
			rArgs:      []string{"Color", "'hsv'", "XYLabel", "c('Index','Value')"},
			octaveArgs: []string{"Color", "'hsv'", "XYLabel", "['Index';'Value']"},
		},
	}

	// Create output directory
	outputDir := "plotVec_test_results"
	if err := os.MkdirAll(outputDir, 0755); err != nil {
		t.Fatalf("Failed to create output directory: %v", err)
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			safeName := strings.ReplaceAll(tt.name, " ", "_")

			// Test R version
			if !tt.skipR {
				t.Run("R", func(t *testing.T) {
					args := append([]string{tt.rData}, tt.rArgs...)
					if err := runPlotVec("Rscript", append([]string{"./plotVec_runners/plotVec_run.R"}, args...)...); err != nil {
						t.Fatalf("R version failed: %v", err)
					}
					// Move and rename R output
					newPath := filepath.Join(outputDir, fmt.Sprintf("plotVec_r_%s.png", safeName))
					if err := os.Rename("plotVec_r.png", newPath); err != nil {
						t.Logf("Warning: could not rename R output file: %v", err)
					}
				})
			}

			// Test Octave/MATLAB version
			if !tt.skipOctave {
				t.Run("Octave", func(t *testing.T) {
					octaveArgs := append([]string{"--no-gui", "-q", "./plotVec_runners/plotVec_run.m", tt.octaveData}, tt.octaveArgs...)
					if err := runPlotVec("octave", octaveArgs...); err != nil {
						t.Fatalf("Octave version failed: %v", err)
					}
					// Move and rename MATLAB output
					newPath := filepath.Join(outputDir, fmt.Sprintf("plotVec_matlab_%s.png", safeName))
					if err := os.Rename("plotVec_matlab.png", newPath); err != nil {
						t.Logf("Warning: could not rename MATLAB output file: %v", err)
					}
				})
			}
		})
	}

	// Generate comparison markdown report
	if err := generateComparisonMarkdownPlotVec(tests, outputDir); err != nil {
		t.Errorf("Failed to generate comparison report: %v", err)
	} else {
		t.Logf("Markdown report generated: %s/RESULTS.md", outputDir)
		t.Log("GitHub will automatically render this Markdown file with images")
	}
}
