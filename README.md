# EPGplotR  
***A Package for Plotting Insect Electrical Penetration Graph Signals in R!***

<div align="center">
  <img src="https://github.com/dankunk/EPGplotR/raw/main/EPGplotR.png" alt="EPGplotR Logo" width="80%"/>
</div>

<div align="center">
  <a href="https://github.com/dankunk/EPGplotR/blob/main/LICENSE.md">
    <img src="https://img.shields.io/badge/License-MIT-blue.svg" alt="License: MIT">
  </a>
  <a href="https://github.com/dankunk/EPGplotR/releases">
    <img src="https://img.shields.io/github/v/release/dankunk/EPGplotR" alt="GitHub Release">
  </a>
  <a href="https://cran.r-project.org/package=EPGplotR">
    <img src="https://www.r-pkg.org/badges/version/EPGplotR" alt="CRAN Status">
  </a>
  <a href="https://github.com/dankunk/EPGplotR/blob/main/CONTRIBUTING.md">
    <img src="https://img.shields.io/badge/contributions-welcome-brightgreen.svg" alt="Contributions Welcome">
  </a>
</div>

---

## 📋 Table of Contents
- [Project Description](#beetle-project-description)
- [Installing EPGplotR](#-installing-epgplotr)
- [Using EPGplotR](#computer-using-epgplotr)
  - [Step 1: Load a `.DXX` Recording](#step-1-load-a-dxx-recording)
  - [Step 2: Load the Corresponding `.ANA` File](#step-2-load-the-corresponding-ana-file)
  - [Step 3: Annotate the Recordings](#step-3-annotate-the-recordings)
  - [Step 4: Plot the Data](#step-4-plot-the-data)
    - [Option 1: Create Interactive Plots](#option-1-create-interactive-plots)
    - [Option 2: Create Static Plots](#option-2-create-static-plots)
- [Example Figure Generated by EPGplotR](#-example-figure-generated-by-epgplotr)
- [Key Features](#wrench-key-features)
- [Getting Started](#books-getting-started-coming-soon)
- [Contributing](#-contributing-coming-soon)
- [License](#-license)

---

## :beetle: Project Description

**EPGplotR** is a powerful R package designed to simplify the analysis and visualization of insect electrical penetration graph (EPG) data. It enables users to:

- **Load EPG Files**: Automatically load **`.DXX`** file formats (**Stylet+ DAQ output files**).
- **Load ANA Files**: Import corresponding **`.ANA`** files for plot annotations.
- **Visualize Signals**: Generate **high-quality** plots of EPG signals (both interactive and static).
- **Annotate and Analyze**: Label recordings with **behavioral annotations** and **segment key areas of interest**.
- **Customize Plots**: Built on the robust frameworks of **`ggplot2`** and **`plotly`**, allowing **extensive user customizations**.

With just four commands, users can produce **publication-ready figures**. EPGplotR makes it easy to generate beautiful visualizations while offering significant flexibility for advanced users.

---

## 🚀 Installing EPGplotR

Before installing **`EPGplotR`**, ensure you have the following tools installed:

1. **R**: Download from [CRAN](https://cran.r-project.org/).
2. **RStudio**: Download the desktop version from [Posit](https://posit.co/download/rstudio-desktop/).

Once these are installed, you can install **EPGplotR** using the **`install_github()`** function from the `devtools` package.

### Installation Steps

1. **Install `devtools` Package** (if not already installed):

    ```R
    install.packages("devtools")
    ```

2. **Load `devtools` into Your Environment**:

    ```R
    library(devtools)
    ```

3. **Install EPGplotR from GitHub**:

    ```R
    install_github("dankunk/EPGplotR")
    ```

4. **Load EPGplotR**:

    ```R
    library(EPGplotR)
    ```

---

## :computer: Using EPGplotR

Once you've loaded **`EPGplotR`** into your environment using `library(EPGplotR)`, you're ready to start using EPGplotR.

### Step 1: Load a `.DXX` Recording

No need to supply a path manually. Simply run `load_d0x_files()`, select the desired file, and EPGplotR will handle the rest.

```R
# Load EPGplotR
library(EPGplotR)

# Load a recording by selecting the file via the file explorer
rec_dt <- load_d0x_files()

# Alternatively, provide a direct path using the filepath argument:
rec_dt <- load_d0x_files(filepath = "C:/Users/user/My_EPG_file_folder/recording1_ch1.D01")
```


### Step 2: Load the Corresponding `.ANA` File


After loading the recordings, load the corresponding `.ANA` file for annotations.

```R
# Load the corresponding ANA file via the file explorer:
ana_dt <- load_ana_file()

# Alternatively, specify the file path directly:
ana_dt <- load_ana_file(filepath = "C:/Users/user/My_ANA_file_folder/recording1_ch1.ANA")
```


### Step 3: Annotate the Recordings

Use the `assign_behavior_codes()` function to add behavioral annotations from the `.ANA` file to your recording data.

```R
# Annotate the rec_dt datatable with behavioral annotations:
assign_behavior_codes(rec_dt, ana_dt)
```

Voila! Now you're ready to plot your data. EPGplotR offers two main plotting options:

1. **Interactive Plots**: Utilize **`plotly`** with the `plot_signal()` function.

2. **Static Plots**: Utilize **`ggplot2`** with the `gg_plot_signal()` function.


### Step 4: Plot the Data

#### Option 1: Create Interactive Plots

This method will use `plotly` via the `plot_signal()` function.

To expedite plotting, EPGplotR offers a downsampling capability that defaults to downsampling the signal by a factor of 10Hz. To preserve proper waveform characteristics, set `downsample_rate = 1`.

```R
# Interactive plots with plotly using plot_signal()
plot_signal(rec_dt)

# Ensure waveform characteristics and behavioral transitions remain intact by setting downsample_rate = 1
plot_signal(rec_dt, downsample_rate = 1)

# Change axis units from hours to seconds using the time_unit argument (default is "hours")
plot_signal(rec_dt, downsample_rate = 1, time_unit = "seconds")

# Modify transparency and add a custom plot title
plot_signal(rec_dt, downsample_rate = 1, time_unit = "seconds", alpha = 1, plot_title = "Recording 1 - Channel 1 - Alpha = 1")
```

#### Option 2: Create Static Plots

This method will use ggplot2 via the `gg_plot_signal()` function.

The `downsample_rate` argument works exactly the same as in the **interactive plot**.

```R
# Create a static plot and display it in the plot window
gg_plot_signal(rec_dt, downsample_rate = 1, alpha = 0.5)

# Assign the plot to a variable for further customization
p <- gg_plot_signal(rec_dt, downsample_rate = 1, alpha = 0.5)

# Segment plots to specific ranges by adding range and range_unit arguments ("hours" or "seconds")
# Plot hours 5-8 only
gg_plot_signal(rec_dt, downsample_rate = 1, alpha = 1, range = c(5, 8), range_unit = "hours")

# Plot from 12603.5 seconds to 12605.85 seconds
gg_plot_signal(rec_dt, alpha = 1, downsample_rate = 1, range = c(12603.5, 12605.85), range_unit = "seconds")

# Save plots to disk using the output_file argument. If left blank, it defaults to NULL and displays the plot in the plot window
gg_plot_signal(
  rec_dt, 
  alpha = 1, 
  downsample_rate = 1, 
  range = c(12603.5, 12605.85), 
  range_unit = "seconds", 
  plot_title = "12604-12606 seconds - Rec1 - Ch1", 
  output_file = "C:/Users/user/My_plot_file_folder/segmented_recording1_ch1_ggplot.svg"
)
```

---

## 📊 Example Figure Generated by EPGplotR
<div align="center"> <img src="https://github.com/dankunk/EPGplotR/blob/main/example_EPGplotR_plot.png" alt="Example EPGplotR Plot" width="80%"/> </div>

---

## :wrench: Key Features
- Load and Process: Quickly import `.DXX` files and prepare them for analysis.
- Interactive Exploration: Zoom, pan, and explore waveforms for in-depth insights.
- Annotation: Add behavior-specific annotations to your plots.
- Customizable Outputs: Export high-quality, presentation-ready figures with vast control over appearance via ggplot2 and plotly.

---

## :books: Getting Started (coming soon)
For more details, visit the `[EPGplotR Documentation](https://github.com/dankunk/EPGplotR/wiki)` to explore tutorials, examples, and detailed instructions.

---

## 🔗 Contributing (coming soon)
Want to contribute to EPGplotR? We welcome contributions! Check out our `[Contributing Guide](https://github.com/dankunk/EPGplotR/main/CONTRIBUTING.md)` for details on how to get started.

---

## 📝 License
This project is licensed under the MIT License. See the `[LICENSE](https://github.com/dankunk/EPGplotR/main/LICENSE.md)` file for details.

---
