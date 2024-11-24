# EPGplotR 
## A Package for Plotting Insect Electrical Penetration Graph Signals in R

<div align="center">
  <img src="https://github.com/dankunk/EPGplotR/raw/main/EPGplotR.png" alt="EPGplotR Logo" width="80%"/>
</div>

---
## Project Description
EPGplotR allows users to load insect electrical penetration graph (EPG) 
 files (with a .DXX extension) into R, plot the signals, and annotate them by behavior. 
 EPGplotR also allows users to interactively explore EPG recordings, segment and inspect 
 waveforms of interest, and save high-quality figures to disc. With just four commands, users can generate publication ready figures. EPGplotR also integrates
 well with ggplot2 and plotly, allowing for extensive user customizations by adding well-known arguments for each of those respective packages.
---
## Installing EPGplotR
Before you start installing EPGplotR make sure that you have R and Rstudio installed. To do this please follow the instructions below. 
R and R studio installation instructions: https://posit.co/download/rstudio-desktop/

Once you have these tools installed, you can install the EPGplotR package with the install.github() function in the devtools package.

```R
# if you don't already have dev tools installed.
install.packages(devtools)

# load devtools into your environment.
library(devtools)

# next, download and install the EPGplotR package from the github repository URL.
install.github("https://github.com/dankunk/EPGplotR")

# now you can load EPGplotR with the library() function.
library(EPGplotR)
```
---
## 
