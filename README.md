# Replication: "Risk, Return, and Equilibrium: Empirical Tests" (Fama & MacBeth, 1973)

This repository provides R scripts that fully replicate the 1973 study by Eugene Fama and James MacBeth in the Journal of Political Economy. The project downloads and cleans the required CRSP data, forms portfolios, and runs the two pass cross sectional regressions to reproduce Tables 1, 2, and 3 from the original paper.

##  Abstract

The Fama and MacBeth (1973) paper provides a foundational empirical test of the Capital Asset Pricing Model (CAPM). It investigates the relationship between the average return on a portfolio and its risk. The study introduces a two-pass regression methodology to test whether stock returns are systematically related to market beta, non-linearities in beta (beta-squared), and non-market (idiosyncratic) risk. This replication exercise follows their methodology closely to validate their findings using modern tools.

##  Data Requirements

This replication relies on data from the Wharton Research Data Services (WRDS) database. A valid WRDS subscription is required to run the data extraction script.

Specifically, the following datasets are used:

### CRSP (Center for Research in Security Prices)
- `crsp_a_stock.msf`: Monthly stock file for returns
- `crsp_a_stock.msenames`: Names history file for share codes (shrcd) and exchange codes (exchcd)
- `crsp_a_stock.dsedelist`: Delisting data for adjusting returns

### Fama/French Factors
- `ff.factors_monthly`: Monthly risk-free rates

The data extraction script (`Code for A3.r`) is configured to pull NYSE common stocks (share codes 10 and 11) for the period from January 1926 to June 1968.

##  Setup and Dependencies

### R Packages

The project requires several R packages for data manipulation, database connection, and table generation. The replicators suggest running the below code to avoid  any unexpected errors during the replication.

```
install.packages(c("DBI", "RPostgres", "broom", "dplyr", "fredr", "gt", 
                   "lubridate", "openxlsx", "purrr", "readr", "stringr", 
                   "tibble", "tidyr", "writexl", "zoo"))
```



### WRDS Connection

You must have valid WRDS credentials. The script `Code for A3.r` connects to the WRDS PostgreSQL database. You will need to amend username  included in wrds code block in the file to establish the connection.
##  How to Run the Replication

### Step 1: Environment Setup

1. Install R and RStudio
2. Install the required R packages listed above
3. Place all the provided R scripts (`Code for A3.r`, `Table1.R`, `Table2.R`, `Table 3.R`) in the same project directory

### Step 2: Script Execution Order

The scripts must be executed sequentially, as each subsequent script depends on the data frames created by the previous one.

1. **Run `Code for A3.r`**: This script connects to WRDS, downloads all necessary data, cleans it, and creates the foundational data frames (`crsp_clean` and `mkt_rf`) in your R environment
2. **Run `Table1.R`**: This script uses the `crsp_clean` object to generate the statistics for Table 1
3. **Run `Table2.R`**: This script also depends on `crsp_clean` and generates the portfolio statistics for Table 2
4. **Run `Table 3.R`**: This is the final and most import script. It uses `crsp_clean` and `mkt_rf` to perform the full Fama-MacBeth two-pass regression and generates the results for Table 3

##  Script Descriptions

### 1. Code for A3.r (Data Preparation)

This is the foundational script that prepares all data for the analysis.

- Connects to the WRDS database
- Extracts CRSP monthly stock data, including returns, share codes, and delisting returns from 1926 to 1968
- Cleans the data by creating a delisting-adjusted return column (`ret_adj`)
- Calculates the equal-weighted NYSE market return (Fisher Index)
- Fetches the risk-free rate from the Fama/French factors dataset
- Creates and saves two key data frames for the subsequent scripts: `crsp_clean` and `mkt_rf`

### 2. Table1.R (Period Definitions)

This script replicates Table 1 from the paper.

- Defines the nine overlapping periods for portfolio formation, estimation, and testing
- For each period, it calculates:
  - The number of securities available at the start of the testing period
  - The number of securities that meet the data requirements (e.g., sufficient non-missing returns in the formation and estimation periods)
- Exports the final table to `Table1.pdf` and an Excel file

### 3. Table2.R (Portfolio Sample Statistics)

This script replicates Table 2 from the paper.

- Selects four specific estimation periods as shown in the original paper
- Forms 20 portfolios based on formation-period betas
- Calculates key sample statistics for each of the 20 portfolios, including:
  - Portfolio beta (β̂_p,t-1)
  - Standard error of portfolio beta (s(β̂_p,t-1))
  - Market model R-squared (r(R_p, R_m)²)
  - Standard deviation of portfolio returns (s(R_p))
- Exports the results to `Table2.pdf` and an Excel file

### 4. Table 3.R (Main Fama-MacBeth Regressions)

This script is the core of the replication, implementing the two-pass regression methodology to replicate Table 3.

- **Portfolio Formation**: Sorts securities into 20 portfolios based on pre-ranking betas
- **Risk Estimation**: Estimates portfolio beta (β_p), squared beta (β_p²), and average residual standard deviation (s̄_p(ε_i)) for the 20 portfolios
- **Monthly Regressions**: For each month in the testing period (1935-1968), it runs a cross-sectional regression of portfolio returns on the estimated risk measures
- **Final Statistics**: It calculates the time-series averages of the coefficients (γ_jt) from the monthly regressions, along with their t-statistics and serial correlations, for 10 different subperiods
- Exports the final, formatted table to `Table3_FamaMacBeth_Corrected.xlsx`

##  Expected Outputs

After running all scripts successfully, the following files will be generated in your project directory:

- `Table1.pdf`
- `Table1 (continued).pdf`
- `Table1_FamaMacBeth_Corrected.xlsx`
- `Table2.pdf`
- `Table2 (continued).pdf`
- `Table 2_FamaMacBeth_Corrected.xlsx`
- `Table3_FamaMacBeth_Corrected.xlsx`

##  License
This code is freely available for anyone to use, replicate, and modify for research, educational, or commercial purposes under the MIT License. 

**If you use this replication in your work, please provide appropriate credit by:**

- Citing the original Fama & MacBeth (1973) paper
- Acknowledging this replication repository in your work.

## Citation

Fama, E. F., & MacBeth, J. D. (1973). Risk, Return, and Equilibrium: Empirical Tests. *Journal of Political Economy*, 81(3), 607–636. [https://doi.org/10.1086/260061](https://doi.org/10.1086/260061)
