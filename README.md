# EPGplotR  
## A Package for Plotting Insect Electrical Penetration Graph Signals in R

<div align="center">
  <img src="https://github.com/dankunk/EPGplotR/raw/main/EPGplotR.png" alt="EPGplotR Logo" width="80%"/>
</div>

---

## :beetle: Project Description

**EPGplotR** is a powerful R package designed to simplify the analysis and visualization of insect electrical penetration graph (EPG) data. It enables users to:
- **Load EPG Files**: Auto loading support for **`.DXX`** file formats (**Stylet+ DAQ output files**).
- **Load ANA Files**: Load corresponding **`.ANA`** files for plot annotations. 
- **Visualize Signals**: Generate **high-quality** plots of EPG signals (interactive and static plots!).
- **Annotate and Analyze**: Label recordings with **behavioral annotations** and **segment key areas of interest**.
- **Customize Plots**: Built on the robust frameworks of **`ggplot2`** and **`plotly`**, allowing **extensive user customizations**.

With just four commands, users can produce **publication-ready figures**. EPGplotR makes it easy to generate beautiful visualizations while offering lots of flexibility for advanced users.

---

## 🚀 Installing EPGplotR

Before installing **`EPGplotR`**, ensure you have the following tools installed:
- **R**: Download from [CRAN](https://cran.r-project.org/).
- **RStudio**: Download the desktop version from [Posit](https://posit.co/download/rstudio-desktop/).

Once these are installed, you can install EPGplotR using the **`install.github()`** function from the `devtools` package.

### Installation Steps
```R
# If you don't already have devtools installed:
install.packages("devtools")

# Load devtools into your environment:
library(devtools)

# Install EPGplotR from GitHub:
install_github("dankunk/EPGplotR")

# Load EPGplotR:
library(EPGplotR)
```

---

## :bar_chart: Using EPGplotR

Once you have loaded **`EPGplotR`** into your environment using `library()` you are ready to use EPGplotR.

The first step is to load a .D## file of interest. No need to supply a path, run `load_d0x_files()`, choose the first hour of recording, and EPGplotR will do all the work!

```R
# Load EPGplotR
library(EPGplotR)

# Load in a recording by selecting the file in the file explorer after running load_d0x_files()
rec_dt = load_d0x_files()

# Or alternatively, you can provide a direct path with the filepath argument (it defaults to FALSE).
rec_dt = load_d0x_files(filepath = "C:/Users/user/My_EPG_file_folder/recording1_ch1.D01")

```

After loading in the recordings, you can now load in the corresponding .ANA file.

```R
# Load the corresponding ANA file
ana_dt = load_ana_file()

# Or you can specify the file path.
ana_dt = load_ana_file(filepath = "C:/Users/user/My_ANA_file_folder/recording1_ch1.ANA")

```

Now that you have both the full recording and the corresponding ANA file loaded, you can use the `assign_behavior_codes()` function to add behavioral annotations to your `rec_dt` datatable.

```R
# Annotate the rec_dt datatable with the behavioral annotations.
assign_behavior_codes(rec_dt, ana_dt)

```




