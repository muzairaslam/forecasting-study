library("tidyverse")
library("ggpubr")
library("fpp3")
library("here")
library("gt")
library("gtsummary")
library("car")  
library("tseries")

# Ensure the directories exist
if (!dir.exists("visualization")) {
  dir.create("visualization")
}

if (!dir.exists("adf_test_results")) {
  dir.create("adf_test_results")
}

data <- read_csv(here("data/data-consolidated-2.csv"))

# Year = Index
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

data_arranged <- data_arranged[1:29,]

data_arranged$fis_yr <- as.integer(data_arranged$fis_yr)
data_arranged$no <- as.integer(data_arranged$no)
data_arranged$nit_vol_k_ton <- as.integer(data_arranged$nit_vol_k_ton)

data_tsibble <- data_arranged |> 
  as_tsibble(index = fis_yr)

# Iterate through each variable, perform first difference, log transformation, plotting, and ADF test
variables <- c("nit_vol_k_ton", "nit_price_per_ton", "water_maf", "cr_area_mn_hec", 
               "agric_gdp_rs_bn", "agric_gdp_usd_bn_cy", "credit_dis_rs_mn", 
               "inp_out_ratio", "n_p_ratio", "tech_proxy")

variable_labels <- c("Nitrogen Volume (N K tonnes)", "Nitrogen Price (per tonne)", 
                     "Water Availability (MAF)", "Cropped Area (million hectares)", 
                     "Agricultural GDP (Rs in billions)", "Agricultural GDP (current BN US$) C-Y", 
                     "Total Credit Disbursed (Rs million)", "Input Output Ratio", 
                     "N_P_Ratio", "Technology Proxy")

for (i in seq_along(variables)) {
  variable_name <- variable_labels[i]
  
  # Compute the first difference
  diff_variable <- diff(data_tsibble[[variables[i]]])
  
  # Remove any NA values resulting from differencing
  diff_variable <- na.omit(diff_variable)
  
  # Take the log of the absolute values of the first differences
  log_diff_variable <- log(abs(diff_variable))
  
  # Remove any NA, NaN, or Inf values resulting from the log transformation
  log_diff_variable <- log_diff_variable[is.finite(log_diff_variable)]
  
  # Align fis_yr to the length of log_diff_variable by removing the first year
  aligned_fis_yr <- data_tsibble$fis_yr[-1]
  
  # Adjust aligned_fis_yr to match the length of log_diff_variable after removing non-finite values
  aligned_fis_yr <- aligned_fis_yr[1:length(log_diff_variable)]
  
  # Combine the aligned_fis_yr and log_diff_variable into a data frame for plotting
  log_diff_data <- data.frame(fis_yr = aligned_fis_yr, log_diff_variable = log_diff_variable)
  
  # Create and save the plot for the log-transformed first-differenced variable
  p <- ggplot(log_diff_data, aes(x = fis_yr, y = log_diff_variable)) +
    geom_line(color = "#900C3F", size = 1.5) +
    labs(title = paste("Log of First-Differenced Time Series Plot of", variable_name), 
         x = "Fiscal Year", 
         y = paste("Log of First Difference of", variable_name)) +
    theme_light()
  
  # Save the plot using a cleaned file name
  ggsave(paste0("visualization/log_diff_", gsub(" ", "_", gsub("/", "_", tolower(variable_name))), ".png"), plot = p, width = 11.69, height = 8.27, units = "in")
  
  # ADF test and save results
  adf_result <- adf.test(log_diff_variable)
  sink(paste0("adf_test_results/log_diff_adf_", gsub(" ", "_", gsub("/", "_", tolower(variable_name))), ".txt"))
  print(adf_result)
  sink()
}
