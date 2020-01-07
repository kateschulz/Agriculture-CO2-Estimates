library(readxl)
library(tidyr)
library(dplyr)
library(tseries)
library(forecast)
library(ggplot2)
library(wesanderson)

# read in data
# EDGAR data
lime = read_excel("EDGARv5_subset.xlsx", sheet = "Liming")
urea = read_excel("EDGARv5_subset.xlsx", sheet = "Urea Application")
biomass = read_excel("EDGARv5_subset.xlsx", sheet = "Biomass Burning")
# FAOSTAT data
energy = read.csv("FAOSTAT_Ag_Energy.csv")

######## FAOSTAT data processing ############
# Remove fisheries data
energy = energy[!grepl("fisheries", energy$Item),]
# Scale units from gigagrams to gigatons
energy <- transform(energy, CO2_Amount = as.integer(Value) * 0.000001)
# drop unnecessary columns
energy =  energy[ , which(names(energy) %in%
                            c("Country", "Item","Year","CO2_Amount"))]

# aggregate by year and reformat columns
energy_agg = aggregate(CO2_Amount ~ Year, energy, sum)
energy_agg$Type <- paste0("Energy Use", energy_agg$Desc)
energy_agg <- energy_agg[c("Year", "Type", "CO2_Amount")]

######## EDGAR data processing ############
# create tidy dataframes
df_list = list(lime, urea, biomass)
df_list = lapply(df_list, function(x){
  # Drop unnecessary columns
  x =  x[ , -which(names(x) %in%
                     c("World Region","IPCC-Annex","IPCC"))]
  # Take all columns for years and re-format into one Year column
  x <-  x %>% gather(Year, CO2_Amount,"1970":colnames(x)[ncol(x)])
  # Scale units from gigagrams to gigatons
  x <- transform(x, CO2_Amount = as.integer(CO2_Amount) * 0.000001)
  x})

# re-name list components
df_list = setNames(df_list, c("lime", "urea", "biomass"))
# break up list into separate dataframes
list2env(df_list,globalenv())

# bind into one dataframe and sum by year and emissions source
edgar_df = bind_rows(df_list, .id = "Type")
edgar_df_agg = aggregate(CO2_Amount ~ Year + IPCC_description, edgar_df, sum)
colnames(edgar_df_agg) <- c("Year", "Type", "CO2_Amount")

# bind final dataframe from EDGAR and FAOSTAT 
final_df_agg = rbind(edgar_df_agg, energy_agg)

######## Connected Scatter Plots ############
# Energy Use by Type
energy_agg_type = aggregate(CO2_Amount ~ Year+Item, energy, sum)
ggplot(energy_agg_type, aes(x = Year, y = CO2_Amount, color = Item, group = Item)) +
  theme_minimal() + geom_point() + geom_line() +
  ggtitle(expression("Agriculture Energy Use CO"[2]*" Emissions by Year, 1970 - 2012")) +
  scale_x_continuous(name = "Year", breaks = seq(1970, 2012, 6)) +
  scale_y_continuous(name = expression("Gt CO"[2]*" Emissions"),
                     limits = c(0,0.41), breaks=seq(0,0.4,0.1)) +
  scale_color_brewer(name = "Emissions Source", palette="Set2")

# Total Emissions
ggplot(final_df_agg, aes(x = Year, y = CO2_Amount, color = Type, group = Type)) +
  theme_minimal() + geom_point() + geom_line() +
  ggtitle(expression("Agriculture CO"[2]*" Emissions by Year, 1970 - 2018")) +
  scale_x_discrete(name = "Year", breaks = seq(1970, 2018, 8)) +
  scale_y_continuous(name = expression("Gt CO"[2]*" Emissions"),
                     limits = c(0,1.15), breaks=seq(0,1.2,0.2)) +
  scale_color_manual(name = "Emissions Source", labels = c("Biomass Burning", "Energy Use", "Liming", "Urea Application"),
                     values=wes_palette(n=4, name="Darjeeling1")) +
  annotate(geom="text",size = 3, x=42, y=1.12, label="Biomass Burning to 2015") +
  annotate(geom="text",size = 3, x=43, y=0.75, label="Energy Use to 2012")

######## Timeseries Analysis ############
# subset aggregated dataframes for categories to forecast
biomass_agg = final_df_agg[which(final_df_agg$Type == "Emissions from biomass burning"),]
biomass_agg = biomass_agg[, !(colnames(biomass_agg) %in% c("Type"))]
energy_agg = final_df_agg[which(final_df_agg$Type == "Energy Use"),]
energy_agg = energy_agg[, !(colnames(energy_agg) %in% c("Type"))]

# create timeseries objects
biomass_ts = ts(biomass_agg[,2], start = c(1970, 1), frequency = 1)
energy_ts = ts(energy_agg[,2], start = c(1970, 1), frequency = 1)

# Determine appropriate models
# For biomass_ts: 
# The ADF test returns a p-value > 0.99, while the KPSS test returns a p-value < 0.1. 
# This implies the time series has a unit root, and therefore a random walk with drift model 
# is the most appropriate choice. 
print(adf.test(biomass_ts))
print(kpss.test(biomass_ts))

# For energy_ts: 
# The ADF test returns a p-value = 0.9171, while the KPSS test returns a p-value < 0.1. 
# This implies the time series has a unit root, and therefore a random walk with drift model 
# is the most appropriate choice. 
print(adf.test(energy_ts))
print(kpss.test(energy_ts))

# Biomass random walk with drift model
biomass_arima = auto.arima(biomass_ts)
# forecast for 3 years to 2018
biomass_fcast = forecast(biomass_arima, 3, level=95)
# plot forecasts with 95% confidence intervals
autoplot(biomass_fcast, ts.geom = 'point', xlab= "Year", 
         ylab = expression("Gt CO"[2]*" Emissions"),
         main = "Biomass Burning Forecasts with 95% Confidence Interval") + 
  theme_minimal() 

# Energy random walk with drift model
energy_arima = auto.arima(energy_ts)
# forecast for 6 years to 2018
energy_fcast = forecast(energy_arima, 6, level = 95)
# plot forecasts with 95% confidence intervals
autoplot(energy_fcast, ts.geom = 'point', xlab= "Year", 
         ylab = expression("Gt CO"[2]*" Emissions"),
         main = "Energy Use Forecasts with 95% Confidence Interval") + 
  theme_minimal() 






