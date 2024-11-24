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

The first step is to load a `.D##` file of interest. No need to supply a path, run `load_d0x_files()`, choose the first hour of recording, and **EPGplotR will do all the work*!

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

Viola! Now we are ready to plot. EPGplotR gives you two options: you can make interactive plots with **`plotly`** using the `plot_signal()` function 
```R
# Interactive plots with plotly using plot_signal()
plot_signal(rec_dt)

# To ensure waveform characteristics and behavioral transitions remain intact use downsample_rate = 1
plot_signal(rec_dt, downsample_rate = 1)

# To change axis units from hours or seconds use the time_unit argument (default is "hours"). This can be useful for finding the durations in seconds of regions/waveforms of interest.
plot_signal(rec_dt, downsample_rate = 1, time_unit = "seconds")

# The default alpha for all plots is 0.5. This can be changed with the alpha argument.
plot_signal(rec_dt, downsample_rate = 1, time_unit = "seconds", alpha = 1, plot_title = "Recording 1 - Channel 1 - Alpha = 1")

```

or you can make more traditional static plots in **`ggplot2`** using the `gg_plot_signal()` function. To make plotting faster, EPGplotR has a downsampling capability that will downsample the signal by a factor of 10HZ by default. To ensure proper waveform characteristics be sure to set the downsample_rate = 1. 

```R
# Make a static plot with the gg_plot_signal() function and display in the plot window.
gg_plot_signal(rec_dt, downsample_rate = 1, alpha = 0.5)

# or assign to a variable
p <- gg_plot_signal(rec_dt, downsample_rate = 1, alpha = 0.5)

# You can also segment plots to specific ranges. If you add an argument for range you must also add an argument for range_units ("hours" or "seconds").

# Plotting hours 5-8 only.
gg_plot_signal(rec_dt, downsample_rate = 1, alpha = 1, range = c(5-8), range_unit = "hours")

# Plotting from 12603.5 seconds to 12605.85 seconds.
gg_plot_signal(rec_dt, alpha =1, downsample_rate = 1 , range = c(12603.5,12605.85), range_unit = "seconds")

# You can also save plots to disc with the output_file argument, if it's blank then it will just default to NULL and show the plot in the plot window.
gg_plot_signal(rec_dt, alpha =1, downsample_rate = 1 , range = c(12603.5,12605.85), range_unit = "seconds", plot_title = "12604-12606 seconds - Rec1 - Ch1", output_file = "C:/Users/user/My_plot_file_folder/segmented_recording1_ch1_ggplot.svg")

```






