# MasterFAA: Setup Instructions

This document provides detailed setup instructions for the MasterFAA project, which analyzes the relationship between Economic Policy Uncertainty (EPU) and exchange rates in Norway and the Eurozone.

## System Requirements

- R version 4.5.0 or higher
- RStudio (latest version)

### Platform-Specific Requirements

- **Windows**:
  - Rtools (for compiling packages)
  - Visual C++ Build Tools (if needed)

- **macOS (Intel)**:
  - Xcode Command Line Tools
  - `xcode-select --install` in Terminal

- **macOS (ARM/M-Chip)**:
  - Xcode Command Line Tools
  - Rosetta 2 (for some packages)

## First-Time Setup

1. Install R 4.5.0 or higher and RStudio
2. Install platform-specific build tools (see System Requirements)
3. Clone the repository:
   ```bash
   git clone https://github.com/olavhegg/MasterFAARepo.git
   cd MasterFAARepo
   ```
4. Open the project in RStudio by double-clicking `MasterFAA.Rproj`
5. Restore the R environment:
   ```r
   renv::restore()
   ```
   - This might take some time on first run
   - Some packages might need additional system libraries

## Project Structure

```
.
├── R/                          # R scripts for analysis
│   ├── utils.R                 # Helper functions and utilities
│   ├── 01_data_cleaning.R     # Data preprocessing and cleaning
│   ├── 02_analysis.R          # Main analysis and calculations
│   ├── 03_models.R            # Statistical modeling (VAR)
│   └── 04_visualization.R     # Plotting and visualization
├── data/                       # Data directory
│   ├── raw/                   # Original data files
│   └── processed/             # Cleaned and transformed data
├── output/                    # Analysis outputs
│   ├── figures/              # Generated plots and visualizations
│   ├── tables/               # Statistical tables and results
│   └── models/               # Saved model objects
├── results/                  # Analysis logs and session info
├── analysis.Rmd             # R Markdown analysis document
├── main.R                   # Main execution script
├── MasterFAA.Rproj          # RStudio project file
├── renv.lock                # Package dependencies
├── README.md                # Project documentation
└── SETUP.md                # Setup instructions (this file)
```

## Running the Analysis

1. Ensure all setup steps are completed
2. Open the project in RStudio
3. Run the analysis:
   ```r
   source("main.R")
   ```

The analysis will execute all steps in sequence and generate:
- Processed datasets in `data/processed/`
- Plots in `output/figures/`
- Statistical tables in `output/tables/`
- Model results in `output/models/`
- Analysis log in `results/analysis_log.txt`

## Troubleshooting

### Common Issues

1. **Package Installation Failures**
   - Check if all system requirements are met
   - Try installing problematic packages individually
   - Check for missing system libraries

2. **Data Loading Issues**
   - Ensure all data files are in the correct location
   - Check file permissions
   - Verify file formats match expected structure

3. **Memory Issues**
   - Close other memory-intensive applications
   - Consider increasing R's memory limit
   - Process data in smaller chunks if needed

### Getting Help

If you encounter issues not covered in this document:
1. Check the analysis log for detailed error messages
2. Review the project documentation in README.md
3. Contact the project maintainers

## License

This project is licensed under the MIT License - see the LICENSE file for details. 