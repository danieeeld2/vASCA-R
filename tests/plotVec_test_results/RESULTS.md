# Vector Plot Comparison: MATLAB vs R

**Generated at**: 2025-04-07 15:21:42  
**Test Results**  

## basic_bar_plot

| MATLAB | R |
|--------|---|
| ![MATLAB Plot](plotVec_matlab_basic_bar_plot.png) | ![R Plot](plotVec_r_basic_bar_plot.png) |

### Commands
```bash
# R Command
Rscript ./plotVec_runners/plotVec_run.R matrix(rnorm(100), ncol=1) XYLabel c('Index','Value')

# MATLAB Command
octave --no-gui -q ./plotVec_runners/plotVec_run.m randn(100,1) XYLabel ['Index';'Value']
```

---

## basic_line_plot

| MATLAB | R |
|--------|---|
| ![MATLAB Plot](plotVec_matlab_basic_line_plot.png) | ![R Plot](plotVec_r_basic_line_plot.png) |

### Commands
```bash
# R Command
Rscript ./plotVec_runners/plotVec_run.R matrix(rnorm(100), ncol=1) PlotType 'Lines' XYLabel c('Index','Value')

# MATLAB Command
octave --no-gui -q ./plotVec_runners/plotVec_run.m randn(100,1) PlotType 'Lines' XYLabel ['Index';'Value']
```

---

## multi_vector_bar_plot

| MATLAB | R |
|--------|---|
| ![MATLAB Plot](plotVec_matlab_multi_vector_bar_plot.png) | ![R Plot](plotVec_r_multi_vector_bar_plot.png) |

### Commands
```bash
# R Command
Rscript ./plotVec_runners/plotVec_run.R matrix(rnorm(300), ncol=3) XYLabel c('Index','Value')

# MATLAB Command
octave --no-gui -q ./plotVec_runners/plotVec_run.m randn(100,3) XYLabel ['Index';'Value']
```

---

## multi_vector_line_plot

| MATLAB | R |
|--------|---|
| ![MATLAB Plot](plotVec_matlab_multi_vector_line_plot.png) | ![R Plot](plotVec_r_multi_vector_line_plot.png) |

### Commands
```bash
# R Command
Rscript ./plotVec_runners/plotVec_run.R matrix(rnorm(300), ncol=3) PlotType 'Lines' XYLabel c('Index','Value')

# MATLAB Command
octave --no-gui -q ./plotVec_runners/plotVec_run.m randn(100,3) PlotType 'Lines' XYLabel ['Index';'Value']
```

---

## with_labels_and_classes

| MATLAB | R |
|--------|---|
| ![MATLAB Plot](plotVec_matlab_with_labels_and_classes.png) | ![R Plot](plotVec_r_with_labels_and_classes.png) |

### Commands
```bash
# R Command
Rscript ./plotVec_runners/plotVec_run.R matrix(rnorm(15), ncol=3) EleLabel c('one','two','three','four','five') ObsClass c(1,1,1,2,2) XYLabel c('Elements','Values')

# MATLAB Command
octave --no-gui -q ./plotVec_runners/plotVec_run.m randn(5,3) EleLabel ['one';'two';'three';'four';'five'] ObsClass [1;1;1;2;2] XYLabel ['Elements';'Values']
```

---

## with_control_limits

| MATLAB | R |
|--------|---|
| ![MATLAB Plot](plotVec_matlab_with_control_limits.png) | ![R Plot](plotVec_r_with_control_limits.png) |

### Commands
```bash
# R Command
Rscript ./plotVec_runners/plotVec_run.R matrix(rnorm(100), ncol=1) LimCont c(1, -1, 3) XYLabel c('Functions','Time')

# MATLAB Command
octave --no-gui -q ./plotVec_runners/plotVec_run.m randn(100,1) LimCont [1, -1, 3] XYLabel ['Functions';'Time']
```

---

## with_variable_control_limits

| MATLAB | R |
|--------|---|
| ![MATLAB Plot](plotVec_matlab_with_variable_control_limits.png) | ![R Plot](plotVec_r_with_variable_control_limits.png) |

### Commands
```bash
# R Command
Rscript ./plotVec_runners/plotVec_run.R matrix(rnorm(10), ncol=1) LimCont rnorm(10) XYLabel c('Elements','Values')

# MATLAB Command
octave --no-gui -q ./plotVec_runners/plotVec_run.m randn(10,1) LimCont randn(10,1) XYLabel ['Elements';'Values']
```

---

## numerical_classes

| MATLAB | R |
|--------|---|
| ![MATLAB Plot](plotVec_matlab_numerical_classes.png) | ![R Plot](plotVec_r_numerical_classes.png) |

### Commands
```bash
# R Command
Rscript ./plotVec_runners/plotVec_run.R matrix(rnorm(30), ncol=3) ObsClass 1:10 ClassType 'Numerical' XYLabel c('Index','Value')

# MATLAB Command
octave --no-gui -q ./plotVec_runners/plotVec_run.m randn(10,3) ObsClass (1:10)' ClassType 'Numerical' XYLabel ['Index';'Value']
```

---

## with_multiplicity

| MATLAB | R |
|--------|---|
| ![MATLAB Plot](plotVec_matlab_with_multiplicity.png) | ![R Plot](plotVec_r_with_multiplicity.png) |

### Commands
```bash
# R Command
Rscript ./plotVec_runners/plotVec_run.R matrix(rnorm(15), ncol=3) EleLabel c('one','two','three','four','five') ObsClass c(1,1,1,2,2) Multiplicity runif(5,1,100) Markers c(20,50,100) XYLabel c('Elements','Values')

# MATLAB Command
octave --no-gui -q ./plotVec_runners/plotVec_run.m randn(5,3) EleLabel ['one';'two';'three';'four';'five'] ObsClass [1;1;1;2;2] Multiplicity 100*rand(5,1) Markers [20,50,100] XYLabel ['Elements';'Values']
```

---

## different_color_palettes

| MATLAB | R |
|--------|---|
| ![MATLAB Plot](plotVec_matlab_different_color_palettes.png) | ![R Plot](plotVec_r_different_color_palettes.png) |

### Commands
```bash
# R Command
Rscript ./plotVec_runners/plotVec_run.R matrix(rnorm(30), ncol=3) Color 'hsv' XYLabel c('Index','Value')

# MATLAB Command
octave --no-gui -q ./plotVec_runners/plotVec_run.m randn(10,3) Color 'hsv' XYLabel ['Index';'Value']
```

---

