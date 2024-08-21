  #Code for checking stationarity:
  #ADF test H0: The data has unit root or data is not stationary.
  # if p-value>0.05: Do not reject H0.
library("tidyverse")
library("ggpubr")
library("fpp3")
library("here")
library("gt")
library("gtsummary")
library("car")  
library("tseries")
  
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

# Define the variable name
variable_name <- "Nitrogen Price (per tonne)"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = nit_price_per_ton)) +
  geom_line(color = "#900C3F", size = 1.5) +
  labs(title = paste("Time Series Plot of", variable_name), 
       x = "Fiscal Year", 
       y = variable_name) +
  theme_light()

ggsave(paste0("visualization/", gsub(" ", "_", tolower(variable_name)), ".png"), plot = p, width = 11.69, height = 8.27, units = "in")

# ADF test and save results
adf_result <- adf.test(na.omit(data_tsibble$nit_price_per_ton))
sink(paste0("adf_test_results/adf_", gsub(" ", "_", tolower(variable_name)), ".txt"))
print(adf_result)
sink()


# Define the variable name
variable_name <- "Nitrogen Volume (N K tonnes)"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = nit_vol_k_ton)) +
  geom_line(color = "#900C3F", size = 1.5) +
  labs(title = paste("Time Series Plot of", variable_name), 
       x = "Fiscal Year", 
       y = variable_name) +
  theme_light()

ggsave(paste0("visualization/", gsub(" ", "_", tolower(variable_name)), ".png"), plot = p, width = 11.69, height = 8.27, units = "in")

# ADF test and save results
adf_result <- adf.test(na.omit(data_tsibble$nit_vol_k_ton))
sink(paste0("adf_test_results/adf_", gsub(" ", "_", tolower(variable_name)), ".txt"))
print(adf_result)
sink()

# Define the variable name
variable_name <- "Water Availability (MAF)"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = water_maf)) +
  geom_line(color = "#900C3F", size = 1.5) +
  labs(title = paste("Time Series Plot of", variable_name), 
       x = "Fiscal Year", 
       y = variable_name) +
  theme_light()

ggsave(paste0("visualization/", gsub(" ", "_", tolower(variable_name)), ".png"), plot = p, width = 11.69, height = 8.27, units = "in")

# ADF test and save results
adf_result <- adf.test(na.omit(data_tsibble$water_maf))
sink(paste0("adf_test_results/adf_", gsub(" ", "_", tolower(variable_name)), ".txt"))
print(adf_result)
sink()

# Define the variable name
variable_name <- "Cropped Area (million hectares)"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = cr_area_mn_hec)) +
  geom_line(color = "#900C3F", size = 1.5) +
  labs(title = paste("Time Series Plot of", variable_name), 
       x = "Fiscal Year", 
       y = variable_name) +
  theme_light()

ggsave(paste0("visualization/", gsub(" ", "_", tolower(variable_name)), ".png"), plot = p, width = 11.69, height = 8.27, units = "in")

# ADF test and save results
adf_result <- adf.test(na.omit(data_tsibble$cr_area_mn_hec))
sink(paste0("adf_test_results/adf_", gsub(" ", "_", tolower(variable_name)), ".txt"))
print(adf_result)
sink()

# Define the variable name
variable_name <- "Agricultural GDP (Rs in billions)"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = agric_gdp_rs_bn)) +
  geom_line(color = "#900C3F", size = 1.5) +
  labs(title = paste("Time Series Plot of", variable_name), 
       x = "Fiscal Year", 
       y = variable_name) +
  theme_light()

ggsave(paste0("visualization/", gsub(" ", "_", tolower(variable_name)), ".png"), plot = p, width = 11.69, height = 8.27, units = "in")

# ADF test and save results
adf_result <- adf.test(na.omit(data_tsibble$agric_gdp_rs_bn))
sink(paste0("adf_test_results/adf_", gsub(" ", "_", tolower(variable_name)), ".txt"))
print(adf_result)
sink()

# Define the variable name
variable_name <- "Agricultural GDP (current BN US$) C-Y"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = agric_gdp_usd_bn_cy)) +
  geom_line(color = "#900C3F", size = 1.5) +
  labs(title = paste("Time Series Plot of", variable_name), 
       x = "Fiscal Year", 
       y = variable_name) +
  theme_light()

ggsave(paste0("visualization/", gsub(" ", "_", tolower(variable_name)), ".png"), plot = p, width = 11.69, height = 8.27, units = "in")

# ADF test and save results
adf_result <- adf.test(na.omit(data_tsibble$agric_gdp_usd_bn_cy))
sink(paste0("adf_test_results/adf_", gsub(" ", "_", tolower(variable_name)), ".txt"))
print(adf_result)
sink()


# Define the variable name
variable_name <- "Total Credit Disbursed (Rs million)"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = credit_dis_rs_mn)) +
  geom_line(color = "#900C3F", size = 1.5) +
  labs(title = paste("Time Series Plot of", variable_name), 
       x = "Fiscal Year", 
       y = variable_name) +
  theme_light()

ggsave(paste0("visualization/", gsub(" ", "_", tolower(variable_name)), ".png"), plot = p, width = 11.69, height = 8.27, units = "in")

# ADF test and save results
adf_result <- adf.test(na.omit(data_tsibble$credit_dis_rs_mn))
sink(paste0("adf_test_results/adf_", gsub(" ", "_", tolower(variable_name)), ".txt"))
print(adf_result)
sink()


# Define the variable name
variable_name <- "Input Output Ratio"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = inp_out_ratio)) +
  geom_line(color = "#900C3F", size = 1.5) +
  labs(title = paste("Time Series Plot of", variable_name), 
       x = "Fiscal Year", 
       y = variable_name) +
  theme_light()

ggsave(paste0("visualization/", gsub(" ", "_", tolower(variable_name)), ".png"), plot = p, width = 11.69, height = 8.27, units = "in")

# ADF test and save results
adf_result <- adf.test(na.omit(data_tsibble$inp_out_ratio))
sink(paste0("adf_test_results/adf_", gsub(" ", "_", tolower(variable_name)), ".txt"))
print(adf_result)
sink()

# Define the variable name
variable_name <- "N / P Ratio"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = n_p_ratio)) +
  geom_line(color = "#900C3F", size = 1.5) +
  labs(title = paste("Time Series Plot of", variable_name), 
       x = "Fiscal Year", 
       y = variable_name) +
  theme_light()

ggsave(paste0("visualization/", gsub(" ", "_", tolower(variable_name)), ".png"), plot = p, width = 11.69, height = 8.27, units = "in")

# ADF test and save results
adf_result <- adf.test(na.omit(data_tsibble$n_p_ratio))
sink(paste0("adf_test_results/adf_", gsub(" ", "_", tolower(variable_name)), ".txt"))
print(adf_result)
sink()

# Define the variable name
variable_name <- "Technology Proxy"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = tech_proxy)) +
  geom_line(color = "#900C3F", size = 1.5) +
  labs(title = paste("Time Series Plot of", variable_name), 
       x = "Fiscal Year", 
       y = variable_name) +
  theme_light()

ggsave(paste0("visualization/", gsub(" ", "_", tolower(variable_name)), ".png"), plot = p, width = 11.69, height = 8.27, units = "in")

# ADF test and save results
adf_result <- adf.test(na.omit(data_tsibble$tech_proxy))
sink(paste0("adf_test_results/adf_", gsub(" ", "_", tolower(variable_name)), ".txt"))
print(adf_result)
sink()


