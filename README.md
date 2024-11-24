# EPGplotR  
## A Package for Plotting Insect Electrical Penetration Graph Signals in R

<div align="center">
  <img src="https://github.com/dankunk/EPGplotR/raw/main/EPGplotR.png" alt="EPGplotR Logo" width="80%"/>
</div>

---

## 🐞 Project Description

**EPGplotR** is a powerful R package designed to simplify the analysis and visualization of insect electrical penetration graph (EPG) data. It enables users to:
- **Load EPG Files**: Auto loading support for `.DXX` file formats (Stylet+ DAQ output files).
- **Load ANA Files**: Load corresponding ANA files for plot annotations. 
- **Visualize Signals**: Generate high-quality plots of EPG signals (interactive and static plots!).
- **Annotate and Analyze**: Label recordings with behavioral annotations and segment key areas of interest.
- **Customize Plots**: Built on the robust frameworks of **ggplot2** and **plotly**, allowing extensive user customizations.

With just four commands, users can produce **publication-ready figures**. EPGplotR makes it easy to generate beautiful visualizations while offering lots of flexibility for advanced users.

---

## 🚀 Installing EPGplotR

Before installing **EPGplotR**, ensure you have the following tools installed:
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
