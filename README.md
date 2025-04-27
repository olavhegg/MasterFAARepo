# MasterFAA: Analysis of Economic Policy Uncertainty and Exchange Rates

This project analyzes the relationship between Economic Policy Uncertainty (EPU) and exchange rates in Norway and the Eurozone, using a Vector Autoregression (VAR) framework. The analysis focuses on understanding how economic policy uncertainty affects exchange rate dynamics and how these relationships differ between Norway and the Eurozone.

## Project Overview

MasterFAA is a comprehensive economic analysis project that:

1. Constructs EPU indices for Norway and the Eurozone
2. Analyzes the relationship between EPU and exchange rates
3. Investigates the impact of various control variables
4. Provides visualizations and statistical insights

For setup instructions, please see [SETUP.md](SETUP.md).

## Interactive Analysis

The project includes an interactive R Markdown document (`analysis.Rmd`) that provides a step-by-step walkthrough of the analysis. This document:

1. Explains each component of the analysis in detail
2. Allows for interactive exploration of the results
3. Provides visualizations and interpretations
4. Can be run either as a complete analysis or step-by-step

To use the interactive analysis:
1. Open `analysis.Rmd` in RStudio
2. Click "Knit" to generate the HTML report
3. Follow the step-by-step instructions

The R Markdown document is organized into four main sections:
- Data Cleaning and Preparation
- Analysis and Transformation
- Statistical Modeling
- Visualization and Results

Each section can be run independently, allowing for focused exploration of specific aspects of the analysis.

## Code Structure and Functionality

### Core Components

#### 1. Data Processing (`R/01_data_cleaning.R`)
- **Norwegian EPU Data Processing**
  - Processes newspaper articles from multiple sources
  - Standardizes EPU counts across different newspapers
  - Creates a weighted EPU index for Norway

- **Eurozone EPU Data Processing**
  - Processes EPU data from multiple Eurozone countries
  - Creates a GDP-weighted EPU index for the Eurozone
  - Handles missing data and standardization

- **Financial Data Processing**
  - Processes VIX, oil prices, and exchange rate data
  - Calculates returns and volatility measures
  - Handles date alignment and missing values

#### 2. Analysis (`R/02_analysis.R`)
- **Relative EPU Calculation**
  - Computes the difference between Norwegian and Eurozone EPU
  - Handles temporal alignment of indices
  - Creates standardized measures for comparison

- **Financial Market Analysis**
  - Processes VIX returns and oil price volatility
  - Calculates exchange rate returns
  - Computes interest rate differentials

- **Inflation Analysis**
  - Processes Norwegian and Eurozone inflation data
  - Calculates inflation volatility measures
  - Creates relative inflation volatility series

#### 3. Modeling (`R/03_models.R`)
- **VAR Model Implementation**
  - Performs stationarity tests
  - Selects optimal lag length
  - Estimates VAR model parameters

- **Impulse Response Analysis**
  - Generates impulse response functions
  - Computes forecast error variance decomposition
  - Analyzes shock propagation

- **Model Diagnostics**
  - Performs stability tests
  - Checks for autocorrelation
  - Validates model assumptions

#### 4. Visualization (`R/04_visualization.R`)
- **EPU Index Visualization**
  - Creates comparative plots of EPU indices
  - Visualizes relative EPU measures
  - Generates time series plots

- **Model Results Visualization**
  - Plots impulse response functions
  - Creates variance decomposition charts
  - Generates diagnostic plots

- **Summary Statistics**
  - Creates descriptive statistics tables
  - Generates correlation matrices
  - Produces summary plots

### Utility Functions (`R/utils.R`)
- **Logging System**
  - Implements robust logging with UTF-8 encoding
  - Provides error handling and debugging
  - Creates structured log output

- **Data Processing Utilities**
  - Handles Norwegian date formats
  - Processes newspaper data
  - Manages file operations

## Key Features

1. **Robust Data Processing**
   - Handles multiple data sources
   - Implements standardized cleaning procedures
   - Manages missing data effectively

2. **Advanced Statistical Analysis**
   - Implements VAR modeling
   - Performs comprehensive diagnostics
   - Generates detailed statistical outputs

3. **Professional Visualization**
   - Creates publication-quality plots
   - Implements consistent styling
   - Generates comprehensive visual summaries

4. **Error Handling and Logging**
   - Implements robust error catching
   - Provides detailed logging
   - Maintains analysis traceability

## Output Files

### Figures
- `epu_indices.png`: Norwegian and Eurozone EPU comparison
- `relative_epu.png`: Relative EPU time series
- `irf_epu.png`: Impulse response functions
- `fevd.png`: Forecast error variance decomposition

### Tables
- `summary_statistics.csv`: Descriptive statistics
- `adf_test_results.rds`: Stationarity test results
- `lag_selection.rds`: VAR lag selection criteria

### Models
- `var_model.rds`: Estimated VAR model
- `irf_epu.rds`: Impulse response results
- `irf_exch.rds`: Exchange rate responses
- `fevd_results.rds`: Variance decomposition results

## Dependencies

The project uses `renv` for package management. Key packages include:

- `dplyr` for data manipulation
- `ggplot2` for plotting
- `xts` and `zoo` for time series handling
- `vars` for VAR modeling
- `tseries` for time series analysis
- `readxl` for Excel file reading
- `tidyr` for data tidying
- `lubridate` for date handling
- `stringr` for string manipulation

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
