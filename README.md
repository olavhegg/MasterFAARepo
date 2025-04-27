# MasterFAA: Analysis of Economic Policy Uncertainty and Exchange Rates

This project analyzes the relationship between Economic Policy Uncertainty (EPU) and exchange rates in Norway and the Eurozone, using a Vector Autoregression (VAR) framework. The analysis focuses on understanding how economic policy uncertainty affects exchange rate dynamics and how these relationships differ between Norway and the Eurozone.

## Project Overview

The project consists of several key components:
1. Data collection and processing of EPU indices for Norway and the Eurozone
2. Construction of control variables (VIX, oil prices, interest rates, inflation)
3. Statistical analysis using VAR models
4. Visualization of results and key findings

## Project Structure

```
MasterFAA/
├── data/
│   ├── raw/             # Original, unmodified data files
│   ├── processed/       # Cleaned and transformed data
│   └── external/        # Data from external sources
├── R/
│   ├── 01_data_cleaning.R    # Data import and initial processing
│   ├── 02_analysis.R         # Data transformation and preparation
│   ├── 03_models.R           # VAR model estimation and diagnostics
│   ├── 04_visualization.R    # Generation of plots and tables
│   └── utils.R               # Helper functions
├── output/
│   ├── figures/         # Plots and charts
│   ├── tables/          # Result tables
│   └── models/          # Saved model objects
├── doc/                 # Documentation
│   ├── thesis/          # Thesis chapters
│   └── notes/           # Research notes
├── results/             # Final outputs
├── MasterFAA.Rproj      # RStudio project file
├── main.R               # Master script
└── renv.lock            # Package dependencies
```

## Data Sources

### Primary Data
- **Norwegian EPU**: Constructed from major Norwegian newspapers (VG, Dagbladet, NRK, Aftenposten, DN)
- **Eurozone EPU**: GDP-weighted average of EPU indices from Eurozone countries
- **Exchange Rates**: EUR/NOK daily spot rates

### Control Variables
- **Market Volatility**: VIX index from CBOE
- **Commodity Prices**: Brent crude oil prices
- **Monetary Policy**: 
  - ECB deposit facility rate
  - Norges Bank policy rate
- **Inflation**: 
  - Norwegian CPI from Statistics Norway
  - Eurozone HICP from Eurostat

## Required R Packages

```r
install.packages(c(
  "readxl",      # Excel file import
  "dplyr",       # Data manipulation
  "stringr",     # String operations
  "lubridate",   # Date handling
  "ggplot2",     # Plotting
  "tidyr",       # Data tidying
  "xts",         # Time series objects
  "zoo",         # Time series operations
  "tseries",     # Time series analysis
  "vars",        # VAR modeling
  "here"         # Project directory management
))
```

## How to Run the Analysis

1. **Setup**:
   ```bash
   # Clone the repository
   git clone https://github.com/olavhegg/MasterFAARepo.git
   cd MasterFAARepo
   ```

2. **Install Dependencies**:
   ```r
   # Install required packages
   install.packages(c("readxl", "dplyr", "stringr", "lubridate", "ggplot2", 
                     "tidyr", "xts", "zoo", "tseries", "vars", "here"))
   ```

3. **Prepare Data**:
   - Place raw data files in `data/raw/`:
     - Newspaper EPU counts
     - VIX data
     - Oil price data
     - Exchange rate data
     - Interest rate data
     - Inflation data

4. **Run Analysis**:
   ```r
   # Open RStudio and load the project
   # Then run the main script
   source("main.R")
   ```

## Output

The analysis generates several types of output:

### Data Files (`data/processed/`)
- Cleaned and transformed time series
- Standardized EPU indices
- Derived variables (returns, volatility measures)

### Statistical Results (`output/tables/`)
- ADF test results
- VAR model diagnostics
- Summary statistics
- Lag selection criteria

### Visualizations (`output/figures/`)
- EPU index comparisons
- Impulse response functions
- Forecast error variance decomposition
- Time series plots

### Model Objects (`output/models/`)
- Estimated VAR models
- IRF results
- FEVD results

## Code Organization

- `utils.R`: Contains helper functions for data processing
- `01_data_cleaning.R`: Data import and initial processing
- `02_analysis.R`: Data transformation and preparation
- `03_models.R`: VAR model estimation and diagnostics
- `04_visualization.R`: Generation of plots and tables
- `main.R`: Orchestrates the entire analysis pipeline

## Contributing

1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contact

For questions or suggestions, please contact:
- Olav Heggelund
- Email: olav.heggelund@soprasteria.com
- GitHub: [@olavhegg](https://github.com/olavhegg)
