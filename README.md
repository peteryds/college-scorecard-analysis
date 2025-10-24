# college-scorecard-analysis

This project analyzes the **Return of Investment of higher education in the United States**.

## Analysis Pipeline 

1.  **Data Loading: API-first, but Cached Locally**
    -   The script automatically detects if a local CSV already exists.
    -   If so, it **skips the API call** and loads the disk cached file
    -   If not, **fetch live data from the official API** and **saved as a CSV file**, so subsequent runs can reuse the same data
2.  **Data Quality Check**
    -   Identify and log issues in `data_quality_issues.csv`
3.  **Data Cleaning & Transformation**
    -   Handle missing and extreme values
    -   Standardize variable names and types
4.  **Define Comparison Groups**
    -   Create a binary variable for Ivy League vs. Non-Ivy League schools
5.  **Data Analysis**
    -   Summary statistics
    -   Visualizations using `ggplot2`

## Project Structure 

-   `R/`: Contains all custom R functions used in the project.
    -   `load_data.R`: Handles fetching data from the API and local caching.
    -   `data_assessment.R`: Performs data quality checks on the raw data.
    -   `clean_data.R`: Cleans and transforms the raw data for analysis.
    -   `define_groups.R`: Defines the comparison groups (e.g., Ivy League).
    -   `visualizations.R`: Contains all `ggplot2` plotting functions.
-   `data/`: Stores the cached raw data (`.csv`) downloaded from the API.
-   `output/`: Stores all generated files, including CSV reports, plots, and final HTML reports. This folder is also ignored by Git.
-   `.env`: A local file for storing your private API key. **This file is not tracked by Git.**
-   `.gitignore`: Specifies files and folders for Git to ignore.
-   `college-scorecard-analysis.Rproj`: The RStudio project file, which sets the correct working directory.
-   `main.R`: The master script that runs the entire pipeline from start to finish.
-   `README.md`: Overview of the project.
-   `report.Rmd`: Final analysis report.

## Setup and Installation 

Follow these steps to get the project running on your local machine.

### 1. Prerequisites

-   [R](https://www.r-project.org/) installed on your system.
-   [RStudio IDE](https://www.rstudio.com/products/rstudio/download/) is highly recommended.
-   [Git](https://git-scm.com/) for cloning the repository.

### 2. Clone the Repository

Open your terminal or command prompt and clone the repository:

``` bash
git clone https://github.com/peteryds/college-scorecard-analysis.git

cd college-scorecard-analysis
```

### 3. Install Required R Packages

Open R or RStudio and run the following commands to install the necessary packages:

``` r
install.packages(c(
  "tidyverse", "ggplot2", "dplyr", "tidyr", "scales",
  "cluster", "broom", "knitr", "rmarkdown"
))
```

### 4. Set Up Environment Variables

An API key from api.data.gov is required to fetch the data. Create a `.env` file in the root directory of the project to store your API key securely. Add the following line to the `.env` file:

```         
API_KEY=your_actual_api_key_here
```

Make sure to replace `your_actual_api_key_here` with your actual API key. **Note:** The `.env` file is included in the `.gitignore` to prevent it from being tracked by Git.

### 5. Run the Main Script In R or RStudio

``` r
source("main.R")
```

### 6. View the Final Report

After running the main script, open the `report.Rmd` file in RStudio and knit it to HTML to view the final analysis report.
