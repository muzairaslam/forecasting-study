# Load necessary libraries
library("tidyverse")
library("tseries")
library("fpp3")
library("here")
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

# Identify the dependent variable (Y) and independent variables (X)
y_variable <- data_tsibble$nit_vol_k_ton
x_variables <- data_tsibble |> 
  select(-nit_vol_k_ton, -no, -fis_yr)  # Remove Y variable, serial number, and year

# Function to perform Engle-Granger test for Y with each X
engle_granger_test <- function(y, x) {
  model <- lm(y ~ x)
  residuals <- model$residuals
  
  # Check for NA in residuals
  if (any(is.na(residuals))) {
    return(NA)
  }
  
  adf_test <- tryCatch({
    adf.test(residuals)
  }, error = function(e) {
    NA  # Return NA if ADF test fails
  })
  
  return(adf_test$p.value)
}

# Create an empty results list
results_list <- list()

# Perform Engle-Granger test for Nitrogen Volume with each independent variable
for (variable in colnames(x_variables)) {
  p_value <- engle_granger_test(y_variable, x_variables[[variable]])
  results_list[[variable]] <- p_value
}

# Convert the results list to a data frame with variable names
results_df <- as.data.frame(do.call(rbind, results_list))
colnames(results_df) <- "P-Value"
results_df <- tibble::rownames_to_column(results_df, var = "Independent Variable")

# Save the results to an Excel file
write_xlsx(results_df, path = "engle_granger_nit_vol_results.xlsx")

# Display the results
print(results_df)
