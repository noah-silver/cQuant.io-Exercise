library(tidyverse)
library(tidyquant)
library(patchwork)
library(lubridate)
library(writexl)
library(reshape)

path_head  = "C:/Users/nsilver/Desktop/cQuant Exercise/"



#### TASK 1: IMPORT DATA ----

contracts    <- read_csv(paste0(path_head,"contracts/Contracts.csv"))

plant_params <- read.csv(paste0(path_head,"plantParameters/Plant_Parameters.csv"))

fuel_prices  <- rbind(read.csv(paste0(path_head,"fuelPrices/GDA_TETSTX.csv")),
                     read.csv(paste0(path_head,"fuelPrices/Henry Hub.csv")))

power_prices <- rbind(read.csv(paste0(path_head,"powerPrices/ERCOT_DA_Prices_2016.csv")),
                      read.csv(paste0(path_head,"powerPrices/ERCOT_DA_Prices_2017.csv")),
                      read.csv(paste0(path_head,"powerPrices/ERCOT_DA_Prices_2018.csv")),
                      read.csv(paste0(path_head,"powerPrices/ERCOT_DA_Prices_2019.csv")))

##### Power Price Statistics ----
#### TASK 2: DESCRIPTIVE STATISTICS ----

pprice_desc_stats <- 
    power_prices %>% 
    mutate(Date       = as.Date(Date),
           year_month = as.yearmon(Date)) %>% 
    group_by(SettlementPoint, year_month) %>% 
    summarize(
        Avg_Price     = mean(Price),
        Min_Price     = min(Price),
        Max_Price     = max(Price),
        SD_Price      = sd(Price)
    ) %>% 
    ungroup()


#### TASK 3: VOLATILITY ----

pprice_volatility <- 
    power_prices %>% 
    mutate(Date       = as.Date(Date),
           year_month = as.yearmon(Date)) %>% 
    group_by(SettlementPoint) %>% 
    arrange(SettlementPoint) %>% 
    
    # volatility is the sd in log returns of hourly prices
    # or change in price from one hour to the next
    # use lag
    mutate(lag_price = lag(Price, 1),
           log_returns =log(Price/lag_price)) %>% 
    
    # remove NAs (first row in each group)
    na.omit() %>% 
    ungroup() %>% 
    
    # calculate volatility
    group_by(SettlementPoint, year_month) %>% 
    summarize(Volatility = sd(log_returns)) %>% 
    ungroup()


#### TASK 4: WRITE DESCRIPTIVE STATS TO FILE ----

# join descriptive stats and volatility, separate year and month column, reorder properly, and set column names
monthly_power_price_stats <- left_join(pprice_desc_stats, pprice_volatility, by = c("SettlementPoint", "year_month")) %>% 
    separate(col = year_month, into = c("Month", "Year"), sep = " ") %>% 
    select(SettlementPoint, Year, Month, everything())
colnames(monthly_power_price_stats) = c("SettlementPoint","Year","Month","Mean","Min","Max","SD","Volatility")

# write csv
write_csv(monthly_power_price_stats, file = paste0(path_head, "/Output/MonthlyPowerPriceStatistics.csv"))


##### Contract Valuation ----
#### TASK 5: EXPAND CONTRACTS ACROSS RELEVANT TIME PERIODS ----

# Separate the contracts into two separate data frames for daily and hourly contracts

contracts_daily <- contracts %>% 
    filter(Granularity == "Daily") %>% 
    mutate(StartDate = as_date(StartDate),
           EndDate = as_date(EndDate))


contracts_hourly <- contracts %>% 
    filter(Granularity == "Hourly") %>% 
    mutate(StartDate = as_datetime(StartDate),
           EndDate = as_datetime(EndDate))


# Expand daily contracts individually (re-bind later)

contract_S1_expanded <- 
    contracts_daily %>% 
    filter(ContractName == "S1") %>% 
    tibble(Date = seq(from = StartDate, to = EndDate, by = "day")) %>% 
    select(ContractName, Date, everything(), -StartDate, -EndDate)


contract_O1_expanded <- 
    contracts_daily %>% 
    filter(ContractName == "O1") %>% 
    tibble(Date = seq(from = StartDate, to = EndDate, by = "day")) %>% 
    select(ContractName, Date, everything(), -StartDate, -EndDate)


# Expand hourly contracts
contract_S2_expanded <- 
    contracts_hourly %>% 
    filter(ContractName == "S2") %>% 
    tibble(Date = seq(from = StartDate, to = EndDate, by = "hour")) %>% 
    select(ContractName, Date, everything(), -StartDate, -EndDate)


contract_O2_expanded <- 
    contracts_hourly %>% 
    filter(ContractName == "O2") %>% 
    tibble(Date = seq(from = StartDate, to = EndDate, by = "hour")) %>% 
    select(ContractName, Date, everything(), -StartDate, -EndDate)


# Bind the rows of the contract dataframes to get two separate dataframes for daily and hourly

contracts_daily_expanded <- rbind(contract_S1_expanded, contract_O1_expanded)
contracts_hourly_expanded <- rbind(contract_S2_expanded, contract_O2_expanded)


#### TASK 6: JOIN RELEVANT PRICES ----

# convert date in fuel prices data frame to date object
fuel_prices <- fuel_prices %>% mutate(Date = as_date(Date))

# join prices of daily contracts
contracts_daily_expanded <- 
    contracts_daily_expanded %>% left_join(fuel_prices, by = c("PriceName" = "Variable", "Date" = "Date"))


# convert date in power prices dataframe to datetime object
power_prices <- power_prices %>% mutate(Date = as_datetime(Date))

# join prices of hourly contracts
contracts_hourly_expanded <- 
    contracts_hourly_expanded %>% left_join(power_prices, by = c("PriceName" = "SettlementPoint", "Date" = "Date"))


#### TASK 7: CALCULATE PAYOFFS ----

# Formulas
# Swaps: (AssetPrice - StrikePrice) x Volume
# Options: [MAX(AssetPrice - StrikePrice, 0) - Premium] x Volume

# Calculate by daily and hourly

daily_payoffs <- 
    contracts_daily_expanded %>%
    mutate(Payoff = case_when(
        DealType == "Swap" ~ (Price-StrikePrice)*Volume,
        DealType == "European option" ~ (max(Price-StrikePrice,0)-Premium)*Volume
        ))


hourly_payoffs <- 
    contracts_hourly_expanded %>%
    mutate(Payoff = case_when(
        DealType == "Swap" ~ (Price-StrikePrice)*Volume,
        DealType == "European option" ~ (max(Price-StrikePrice,0)-Premium)*Volume
    ))


#### TASK 8: CALCULATE AGGREGATE PAYOFFS ----


# Sum Payoff, grouped by Contract and Year-Month

daily_payoffs_agg <- 
    daily_payoffs %>%  
    mutate(Year_Month = as.yearmon(Date)) %>% 
    group_by(ContractName, Year_Month) %>%
    summarize(Agg_Payoff = sum(Payoff, na.rm = T)) %>% 
    ungroup()
    
hourly_payoffs_agg <- 
    hourly_payoffs %>%  
    mutate(Year_Month = as.yearmon(Date)) %>% 
    group_by(ContractName, Year_Month) %>%
    summarize(Agg_Payoff = sum(Payoff, na.rm = T)) %>% 
    ungroup()


#### TASK 9: WRITE AGGREGATE PAYOFFS DATA TO FILE ----

# join descriptive stats and volatility, separate year and month column, reorder properly, and set column names
monthly_contract_payoffs <- 
    rbind(daily_payoffs_agg, hourly_payoffs_agg) %>% 
    separate(col = Year_Month, into = c("Month", "Year"), sep = " ") %>% 
    select(ContractName, Year, Month, everything())
colnames(monthly_contract_payoffs) = c("ContractName","Year","Month","TotalPayoff")

# write csv
write_csv(monthly_contract_payoffs, file = paste0(path_head, "/Output/MonthlyContractPayoffs.csv"))

##### Plant Dispatch Modeling ----

#### TASK 10: CALCULATE HOURLY RUNNING COST OF EACH POWER PLANT ----

# RunningCost = ((FuelPrice + FuelTransportationCost) x HeatRate) + VOM

# Join daily fuel prices to plant_params dataframe

# first get year and months of date in fuel_prices, to match across plant_params
fuel_prices_monthly <- 
    fuel_prices %>% 
    mutate(Year  = year(Date),
           Month = month(Date))

# Join
plant_params_prices <- 
    plant_params %>% 
    left_join(fuel_prices_monthly, by             = c("FuelPriceName" = "Variable",
                                          "Year"  = "Year",
                                          "Month" = "Month"))

# Calculate Running Cost
plant_running_cost  <- 
    plant_params_prices %>% 
    mutate(
        RunningCost = ((Price+FuelTransportationCost)*HeatRate)+VOM
    )



#### TASK 11: JOIN THE HOURLY PRICES ----

# align date format of power_prices with running cost dataframe
power_prices <- 
    power_prices %>% 
    mutate(daydate = as.Date(Date))

# join
plant_runcost_hourly_pprice <- 
    plant_running_cost %>% 
    mutate(Date = as_datetime(Date)) %>% 
    left_join(power_prices, by = c("PowerPriceName" = "SettlementPoint",
                                   "Date" = "daydate")) %>% 
    rename(c("Price.x" = "Fuel_Price",
           "Price.y"   = "Power_Price",
           "Date.y"    = "Hour"))


#### TASK 12: IDENTIFY HOURS IN WHICH THE POWER PLANT SHOULD BE ON ----

plant_runcost_hourly_pprice <- 
    plant_runcost_hourly_pprice %>% 
    mutate(Generation    = ifelse(Power_Price>RunningCost, Capacity, 0),
           RunningMargin = (Power_Price-RunningCost)*Generation)


#### TASK 13: ACCOUNT FOR START COSTS ----

#test <- plant_runcost_hourly_pprice %>% 
    
# need to identify starts and group by continuous blocks of generation

#for(i in 1:nrow(plant_runcost_hourly_pprice)){    
#     mutate(Start = ifelse(RunningMargin[i]!=0 && RunningMargin[i-1]==0, T, F))
#}
    

# then
# group_by( #blockformat )

# mutate( Generation2 = sum(Generation) )



#### TASK 14: WRITE RESULTS TO FILE
    
write_csv(plant_runcost_hourly_pprice, file = paste0(path_head, "/Output/PowerPlantDispatch.csv"))
    