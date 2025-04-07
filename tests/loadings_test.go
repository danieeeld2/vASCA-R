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
func runLoadings(script string, args ...string) error {
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

func generateComparisonMarkdownLoadings(testCases []struct {
	name       string
	dataset    string
	rArgs      []string
	octaveArgs []string
	skipR      bool
	skipOctave bool
}, outputDir string) error {
	mdContent := "# Loadings Plot Comparison: MATLAB vs R\n\n"
	mdContent += fmt.Sprintf("**Generated at**: %s  \n", time.Now().Format("2006-01-02 15:04:05"))
	mdContent += fmt.Sprintf("**Test Results**  \n\n")

	for _, tc := range testCases {
		if tc.skipR || tc.skipOctave {
			continue
		}

		safeName := strings.ReplaceAll(tc.name, " ", "_")
		datasetPath := filepath.Join("../datasets/tests_datasets", tc.dataset)

		rCmd := strings.Join(append([]string{"Rscript", "./loadings_runners/loadings_run.R", datasetPath}, tc.rArgs...), " ")
		octaveCmd := strings.Join(append([]string{"octave", "--no-gui", "-q", "./loadings_runners/loadings_run.m", datasetPath}, tc.octaveArgs...), " ")

		mdContent += fmt.Sprintf("## %s\n\n", tc.name)
		mdContent += fmt.Sprintf("**Dataset**: %s\n\n", tc.dataset)
		mdContent += "| MATLAB | R |\n"
		mdContent += "|--------|---|\n"

		// Include all generated images (there might be multiple plots per test case)
		matlabImgs := []string{}
		rImgs := []string{}

		// Find all MATLAB images
		for i := 1; i <= 3; i++ {
			imgPath := filepath.Join(outputDir, fmt.Sprintf("loadings_%d_matlab_%s.png", i, safeName))
			if _, err := os.Stat(imgPath); err == nil {
				matlabImgs = append(matlabImgs, fmt.Sprintf("![MATLAB Plot %d](loadings_%d_matlab_%s.png)", i, i, safeName))
			}
		}

		// Find all R images
		for i := 1; i <= 3; i++ {
			imgPath := filepath.Join(outputDir, fmt.Sprintf("loadings_%d_r_%s.png", i, safeName))
			if _, err := os.Stat(imgPath); err == nil {
				rImgs = append(rImgs, fmt.Sprintf("![R Plot %d](loadings_%d_r_%s.png)", i, i, safeName))
			}
		}

		// Make sure we have the same number of images for both
		maxPlots := len(matlabImgs)
		if len(rImgs) > maxPlots {
			maxPlots = len(rImgs)
		}

		for i := 0; i < maxPlots; i++ {
			matlabImg := ""
			if i < len(matlabImgs) {
				matlabImg = matlabImgs[i]
			}

			rImg := ""
			if i < len(rImgs) {
				rImg = rImgs[i]
			}

			mdContent += fmt.Sprintf("| %s | %s |\n", matlabImg, rImg)
		}

		mdContent += "\n### Commands\n"
		mdContent += "```bash\n"
		mdContent += fmt.Sprintf("# R Command\n%s\n\n", rCmd)
		mdContent += fmt.Sprintf("# MATLAB Command\n%s\n", octaveCmd)
		mdContent += "```\n\n"
		mdContent += "---\n\n"
	}

	mdPath := filepath.Join(outputDir, "RESULTS.md")
	return os.WriteFile(mdPath, []byte(mdContent), 0644)
}

func TestLoadings(t *testing.T) {
	tests := []struct {
		name       string
		dataset    string
		rArgs      []string
		octaveArgs []string
		skipR      bool
		skipOctave bool
	}{
		{
			name:    "basic loadings",
			dataset: "model_1.json",
		},
		{
			name:       "loadings with scatter plot",
			dataset:    "model_2.json",
			rArgs:      []string{"PlotType", "Scatter"},
			octaveArgs: []string{"PlotType", "Scatter"},
		},
		{
			name:       "loadings with bar plot",
			dataset:    "model_3.json",
			rArgs:      []string{"PlotType", "Bars"},
			octaveArgs: []string{"PlotType", "Bars"},
		},
		{
			name:       "loadings with custom labels",
			dataset:    "model_1.json",
			rArgs:      []string{"VarsLabel", "c('Var1','Var2','Var3','Var4','Var5','Var6','Var7','Var8','Var9','Var10')", "Title", "Custom Labels"},
			octaveArgs: []string{"VarsLabel", "{'Var1';'Var2';'Var3';'Var4';'Var5';'Var6';'Var7';'Var8';'Var9';'Var10'}", "Title", "'Custom Labels'"},
		},
		{
			name:       "loadings with classes",
			dataset:    "model_2.json",
			rArgs:      []string{"classes", "c(1,1,1,1,2,2,2,2,3,3)", "Color", "hsv"},
			octaveArgs: []string{"VarsClass", "[1;1;1;1;2;2;2;2;3;3]", "Color", "'hsv'"},
		},
	}

	// Create output directory
	outputDir := "loadings_test_results"
	if err := os.MkdirAll(outputDir, 0755); err != nil {
		t.Fatalf("Failed to create output directory: %v", err)
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			safeName := strings.ReplaceAll(tt.name, " ", "_")
			datasetPath := filepath.Join("../datasets/tests_datasets", tt.dataset)

			// Test R version
			if !tt.skipR {
				t.Run("R", func(t *testing.T) {
					args := append([]string{datasetPath}, tt.rArgs...)
					if err := runLoadings("Rscript", append([]string{"./loadings_runners/loadings_run.R"}, args...)...); err != nil {
						t.Fatalf("R version failed: %v", err)
					}

					// Move and rename R output (there might be multiple plots)
					for i := 1; i <= 3; i++ {
						src := fmt.Sprintf("loadings_%d_r.png", i)
						if _, err := os.Stat(src); err == nil {
							newPath := filepath.Join(outputDir, fmt.Sprintf("loadings_%d_r_%s.png", i, safeName))
							if err := os.Rename(src, newPath); err != nil {
								t.Logf("Warning: could not rename R output file %s: %v", src, err)
							}
						}
					}
				})
			}

			// Test Octave/MATLAB version
			if !tt.skipOctave {
				t.Run("Octave", func(t *testing.T) {
					octaveArgs := append([]string{"--no-gui", "-q", "./loadings_runners/loadings_run.m", datasetPath}, tt.octaveArgs...)
					if err := runLoadings("octave", octaveArgs...); err != nil {
						t.Fatalf("Octave version failed: %v", err)
					}

					// Move and rename MATLAB output (there might be multiple plots)
					for i := 1; i <= 3; i++ {
						src := fmt.Sprintf("loadings_%d_matlab.png", i)
						if _, err := os.Stat(src); err == nil {
							newPath := filepath.Join(outputDir, fmt.Sprintf("loadings_%d_matlab_%s.png", i, safeName))
							if err := os.Rename(src, newPath); err != nil {
								t.Logf("Warning: could not rename MATLAB output file %s: %v", src, err)
							}
						}
					}
				})
			}
		})
	}

	// Generate comparison markdown report
	if err := generateComparisonMarkdownLoadings(tests, outputDir); err != nil {
		t.Errorf("Failed to generate comparison report: %v", err)
	} else {
		t.Logf("Markdown report generated: %s/RESULTS.md", outputDir)
		t.Log("GitHub will automatically render this Markdown file with images")
	}
}
