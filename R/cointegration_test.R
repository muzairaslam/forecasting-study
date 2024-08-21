# Load necessary libraries
library("tidyverse")
library("tseries")
library("fpp3")
library("here")
library(urca)
library(writexl)

# Read and arrange data
data <- read_csv(here("data/data-consolidated-2.csv"))

# Renaming columns for easier access
data_arranged <- data |>
  rename(
    no = `Serial No`,
    fis_yr = `Fiscal Year`,
    nit_vol_k_ton = `Nitrogen Volume (N K tonnes)`,
    nit_price_per_ton = `Nitrogen Price (per tonne)`,
    water_maf = `Water Availability (MAF)`,
    cr_area_mn_hec = `Cropped Area (million hectares)`,
    agric_gdp_rs_bn = `Agricultural GDP (Rs in billions)`,
    agric_gdp_usd_bn_cy = `Agricultural GDP (current BN US$) C-Y`,
    credit_dis_rs_mn = `Total Credit Disbursed (Rs million)`,
    inp_out_ratio = `Input Output Ratio`,
    n_p_ratio = `N / P`,
    tech_proxy = `New Tech Proxy(per Ha)`
  )

# Subsetting the data to the required range
data_arranged <- data_arranged[1:29,]

# Convert specific columns to integer as needed
data_arranged <- data_arranged |>
  mutate(
    fis_yr = as.integer(fis_yr),
    no = as.integer(no),
    nit_vol_k_ton = as.integer(nit_vol_k_ton)
  )

# Convert to tsibble for time series analysis
data_tsibble <- data_arranged |> 
  as_tsibble(index = fis_yr)

# Convert the data to a matrix or data frame for Johansen test (including y-variable)
data_matrix <- as.matrix(data_tsibble[,-1])  # Remove fis_yr column

# Check structure of the data prepared for the Johansen test
str(data_matrix)

# Perform the Johansen cointegration test (if needed)
johansen_test <- ca.jo(data_matrix, type = "trace", ecdet = "none", K = 2)
summary(johansen_test)

# Engle-Granger Two-Step Cointegration Test for each pair of variables
variables <- colnames(data_tsibble)[-1]  # Exclude fis_yr from column names

# Function to perform Engle-Granger test for each pair
engle_granger_test <- function(y, x) {
  model <- lm(y ~ x)
  residuals <- model$residuals
  adf_test <- adf.test(residuals)
  return(adf_test$p.value)
}

# Create an empty results matrix
results_matrix <- matrix(NA, nrow = length(variables), ncol = length(variables))
rownames(results_matrix) <- variables
colnames(results_matrix) <- variables

# Populate the results matrix with p-values from Engle-Granger test
for (i in 1:length(variables)) {
  for (j in i:length(variables)) {
    if (i != j) {
      p_value <- engle_granger_test(data_tsibble[[variables[i]]], data_tsibble[[variables[j]]])
      results_matrix[i, j] <- p_value
      results_matrix[j, i] <- p_value
    }
  }
}

# Convert the matrix to a data frame for easier interpretation and add variable names as the first column
results_df <- as.data.frame(results_matrix)
results_df <- cbind(Variable = rownames(results_df), results_df)

# Save the results to an Excel file
write_xlsx(results_df, path = "engle_granger_results.xlsx")
