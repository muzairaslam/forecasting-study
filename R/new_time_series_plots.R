library("tidyverse")
library("ggpubr")
library("fpp3")
library("here")
library("gt")
library("gtsummary")
library("car")  
library("tseries")

data <- read_csv(here("data/data-consolidated-2.csv"))


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

# N Price
nit_price <- "Nitrogen Price (per ton)"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = nit_price_per_ton)) +
  geom_line(color = "#900C3F", size = 3) +
  labs(title = paste(nit_price), 
       x = "Fiscal Year", 
       y = nit_price) +
  scale_x_continuous(breaks = seq(min(data_tsibble$fis_yr-1), max(data_tsibble$fis_yr), by = 5)) +
  theme_light()  +
  theme(
    axis.title = element_text(size = 18),    # Increase size of axis labels
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 22)     # Increase size of axis numbers
  )

p

ggsave(paste0("visualization/new_timeseries/", gsub(" ", "_", tolower(nit_price)), ".png"), 
       plot = p, 
       width = 350, height = 210, units = "mm",  # A4 size in landscape
       dpi = 300)  # 300 DPI for high-quality print


# N consum
nit_cons <- "Nitrogen Consumption (N K Tons)"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = nit_vol_k_ton)) +
  geom_line(color = "#900C3F", size = 3) +
  labs(title = paste(nit_cons), 
       x = "Fiscal Year", 
       y = nit_price) +
  scale_x_continuous(breaks = seq(min(data_tsibble$fis_yr-1), max(data_tsibble$fis_yr), by = 5)) +
  theme_light()  +
  theme(
    axis.title = element_text(size = 18),    # Increase size of axis labels
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 22)     # Increase size of axis numbers
  )

p

ggsave(paste0("visualization/new_timeseries/", gsub(" ", "_", tolower(nit_cons)), ".png"), 
       plot = p, 
       width = 350, height = 210, units = "mm",  # A4 size in landscape
       dpi = 300)  # 300 DPI for high-quality print



water <- "Water Availability (MAF)"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = water_maf )) +
  geom_line(color = "#900C3F", size = 3) +
  labs(title = paste(water), 
       x = "Fiscal Year", 
       y = water) +
  scale_x_continuous(breaks = seq(min(data_tsibble$fis_yr-1), max(data_tsibble$fis_yr), by = 5)) +
  theme_light()  +
  theme(
    axis.title = element_text(size = 18),    # Increase size of axis labels
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 22)     # Increase size of axis numbers
  )

p

ggsave(paste0("visualization/new_timeseries/", gsub(" ", "_", tolower(water)), ".png"), 
       plot = p, 
       width = 350, height = 210, units = "mm",  # A4 size in landscape
       dpi = 300)  # 300 DPI for high-quality print


crop_acre <- "Cropped Area (Million Hectares)"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = cr_area_mn_hec)) +
  geom_line(color = "#900C3F", size = 3) +
  labs(title = paste(crop_acre), 
       x = "Fiscal Year", 
       y = crop_acre) +
  scale_x_continuous(breaks = seq(min(data_tsibble$fis_yr-1), max(data_tsibble$fis_yr), by = 5)) +
  theme_light()  +
  theme(
    axis.title = element_text(size = 18),    # Increase size of axis labels
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 22)     # Increase size of axis numbers
  )

p

ggsave(paste0("visualization/new_timeseries/", gsub(" ", "_", tolower(crop_acre)), ".png"), 
       plot = p, 
       width = 350, height = 210, units = "mm",  # A4 size in landscape
       dpi = 300)  # 300 DPI for high-quality print


ag_cr <- "Total Credit Disbursed (Million Rs)"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = credit_dis_rs_mn)) +
  geom_line(color = "#900C3F", size = 3) +
  labs(title = paste(ag_cr), 
       x = "Fiscal Year", 
       y = ag_cr) +
  scale_x_continuous(breaks = seq(min(data_tsibble$fis_yr-1), max(data_tsibble$fis_yr), by = 5)) +
  theme_light()  +
  theme(
    axis.title = element_text(size = 18),    # Increase size of axis labels
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 22)     # Increase size of axis numbers
  )

p

ggsave(paste0("visualization/new_timeseries/", gsub(" ", "_", tolower(ag_cr)), ".png"), 
       plot = p, 
       width = 350, height = 210, units = "mm",  # A4 size in landscape
       dpi = 300)  # 300 DPI for high-quality print


tech <- "Technology Proxy"

# Create and save the plot
p <- ggplot(data_tsibble, aes(x = fis_yr, y = tech_proxy)) +
  geom_line(color = "#900C3F", size = 3) +
  labs(title = paste(tech), 
       x = "Fiscal Year", 
       y = tech) +
  scale_x_continuous(breaks = seq(min(data_tsibble$fis_yr-1), max(data_tsibble$fis_yr), by = 5)) +
  theme_light()  +
  theme(
    axis.title = element_text(size = 18),    # Increase size of axis labels
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 22)     # Increase size of axis numbers
  )

p

ggsave(paste0("visualization/new_timeseries/", gsub(" ", "_", tolower(tech)), ".png"), 
       plot = p, 
       width = 350, height = 210, units = "mm",  # A4 size in landscape
       dpi = 300)  # 300 DPI for high-quality print
