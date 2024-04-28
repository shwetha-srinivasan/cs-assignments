"This code sample explores the relationship between locust swarm exposure and 
food prices at local markets. To explore the relationship the code first cleans
and merges locust swarm and food prices/local markets data. Next, it conducts
sanity checks and additional exploratory data analysis to to understand the 
dataset and chart out next steps for analysis. Finally, the code produces 
regression results that explore the effect of locust swarm exposure on food 
prices at local markets, accounting for fixed effects"

# Relevant Libraries

library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(haven)
library(lubridate)
library(geosphere)
library(openxlsx)
library(knitr)
library(psych)
library(car)
library(plm)
library(fixest)
library(estimatr)


############# PART 1: DATASET CREATION AND CLEANING ##################

source("D:/Dropbox/Research/LocustImpacts/codeR/food_emergency/fews_net/local_market/_master_local_market.R")
source("D:/Dropbox/Research/LocustImpacts/codeR/food_emergency/fews_net/local_market/data_clean_fewsnet.R")

# Check for NA values and drop NA values for our analysis
colSums(is.na(local_market_data_clean))

clean_fews <- local_market_data_clean %>%
  drop_na(value)


# Filter out commodities that are non-food groups
clean_fews <- clean_fews %>%
  filter(!(product %in% non_food_vec))

clean_fews <- clean_fews %>%
  filter(!(product == "Casual Labor (unskilled, daily, without food)"))

# Check dataset
colSums(is.na(clean_fews))

# Extract year and month as separate columns from period_date column
clean_fews$year <- year(clean_fews$period_date)
clean_fews$month <- month(clean_fews$period_date)


# Limit analysis to date ranges based on available data in FEWSNET and Swarm 
# location data
range(clean_fews$year)
clean_fews <- clean_fews %>%
  filter(year >= 1995 & year <= 2020)


# Read in locust swarm data
swarms <- read_dta("D:/Dropbox/Research/LocustImpacts/dataSTATA/locust_coordinates/swarm_geolocations.dta")


# Extract year and month as separate columns
swarms$date <- as_datetime(swarms$date, format = "%Y/%m/%d %H:%M:%S")

swarms$year <- year(swarms$date)
swarms$month <- month(swarms$date)


# Limit analysis to date ranges based on available data in FEWSNET and Swarm 
# location data
range(swarms$year)
swarms <- swarms %>%
  filter(year >= 1995 & year <= 2020)

# Check for NAs
colSums(is.na(swarms))


# Merge FEWSNET and SWarm data
merged_data <- left_join(clean_fews, swarms, by = c("year", "month"))
colSums(is.na(merged_data))


# Compute distance between local market and locust swarm
merged_data$dist_haversine <- geosphere::distHaversine(merged_data[, c("longitude", "latitude")], 
                                                       merged_data[, c("lon_swarm", "lat_swarm")])
# converting to km
merged_data$dist_haversine <- merged_data$dist_haversine/1000


# For each country/market/year/month, compute the number of swarms in total,
# number of swarms within 150km of the  market
# Create a dummy for swarms that are within 150kms of the local market

merged_data_new <- merged_data %>%
  group_by(country, market, year, month) %>%
  mutate(
    num_swarms = n(),
    num_swarms_within = sum(dist_haversine <= 150),
    num_swarms_outside = sum(dist_haversine > 150),
    swarm_exposure = ifelse(num_swarms_within > 0, 1, 0)
  )

merged_data_new <- merged_data_new %>%
  mutate(swarm_exposure = ifelse(is.na(swarm_exposure), 0, swarm_exposure))

# Our final dataset for analysis 
unique_commodity <- distinct(merged_data_new, country, market, year, month,
                             product, .keep_all = TRUE)


# Save as .csv file
write.xlsx(unique_commodity, "D:/Dropbox/Research/LocustImpacts/dataCSV/fews_net/local_market/unique_commodity.xlsx", rowNames = FALSE)


############# PART 2: DATASET CLEANING AND CHECKS ##################

# Check for single currency in a market/product
currency_check <- unique_commodity %>%
  group_by(market, product) %>%
  summarise(unique_currency = n_distinct(currency) == 1)

# Check for missing year/month combinations
reference_df <- expand.grid(year = 1995:2020, month = 1:12)

# Check for missing combinations
missing_combinations <- anti_join(reference_df, unique_commodity, by = c("year", "month"))

# If there are missing combinations, they will be stored in the 'missing_combinations' data frame
if (nrow(missing_combinations) > 0) {
  print("Missing year/month combinations:")
  print(missing_combinations)
} else {
  print("No missing year/month combinations found.")
}

############ COMMODITIES ANALYSIS ####################
summary(unique_commodity)
describe(unique_commodity)

# Find the number of unique markets overall
unique_markets_overall <- unique_commodity %>%
  summarise(unique_markets = n_distinct(market))

unique_markets_overall

# Unique Markerts and commodities per country
unique_commodities_per_country <- unique_commodity %>%
  group_by(country) %>%
  summarise(unique_commodities = n_distinct(product))

joined_table <- left_join(unique_markets_per_country, unique_commodities_per_country, by = "country")

# Print the joined table
kable(joined_table, caption = "Number of Unique Markets and Commodities per Country")


# Commodity counts for each market
market_commodity_count <- unique_commodity %>%
  group_by(country, market) %>%
  summarize(num_commodities = n_distinct(product),
            min_commodities = min(num_commodities),
            max_commodities = max(num_commodities),
            sd_commodities = sd(num_commodities),
            mean_commodities = mean(num_commodities))

# Display the table
print(market_commodity_count)

country_min_max_commodities <- unique_commodity %>%
  group_by(country, market) %>%
  summarize(num_commodities = n_distinct(product)) %>%
  group_by(country) %>%
  summarize(
    min_commodities = min(num_commodities),
    max_commodities = max(num_commodities),
    sd_commodities = sd(num_commodities),
    mean_commodities = mean(num_commodities)
  )

summary_table <- left_join(joined_table, country_min_max_commodities, by = "country")
kable(summary_table, caption = "Summary of Markets and Commodities per Country")


# Commodity Distribution Across Countries
commodity_countries_count <- unique_commodity %>%
  group_by(product) %>%
  summarize(num_countries = n_distinct(country),
            countries = paste(unique(country), collapse = ", "))

# Plot the distribution of commodities across countries
ggplot(commodity_countries_count, aes(x = num_countries)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Number of Countries", y = "Frequency", 
       title = "Distribution of Commodities Across Countries") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  theme_minimal()


# Local vs Imported Commodities
commodity_distribution <- unique_commodity %>%
  group_by(country, product_source) %>%
  summarise(count = n()) %>%
  mutate(product_source = factor(product_source, levels = c("Local", "Import")))

# Plot stacked bar chart
ggplot(commodity_distribution, aes(x = country, y = count, fill = product_source)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Count of Commodities", 
       title = "Distribution of Commodities by Source") +
  scale_fill_manual(values = c("Local" = "blue", "Import" = "orange"), 
                    name = "Commodity Source") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Group and summarize the data
commodity_summary <- unique_commodity %>%
  group_by(product, product_source) %>%
  summarise(count = n())

# Filter commodities based on source
local_commodities <- commodity_summary %>%
  filter(product_source == "Local") %>%
  select(product)

imported_commodities <- commodity_summary %>%
  filter(product_source == "Import") %>%
  select(product)

# Display the results
cat("Locally Sourced Commodities:\n")
print(local_commodities$product)

cat("\nImported Commodities:\n")
print(imported_commodities$product)


# Commodity Distribution by Source
commodity_distribution <- commodity_distribution %>%
  group_by(country) %>%
  mutate(percentage = count / sum(count) * 100)

# Plot stacked bar chart with percentages
ggplot(commodity_distribution, aes(x = country, y = percentage, fill = product_source)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage of Commodities", 
       title = "Distribution of Commodities by Source (Normalized)") +
  scale_fill_manual(values = c("Local" = "blue", "Import" = "orange"), 
                    name = "Commodity Source") +
  theme_minimal() +
  theme(legend.position = "bottom")

################ PRODUCT GROUP ANALYSIS ######################

product_group_count <- unique_commodity %>%
  group_by(product_group) %>%
  summarize(num_countries = n_distinct(country),
            countries = paste(unique(country), collapse = ", "))

country_market <- unique(unique_commodity[, c("country", "market")])
years <- unique(unique_commodity$year)
product_groups <- unique(unique_commodity$product_group)

year_product_group <- expand.grid(
  year = years,
  product_group = product_groups
)

# Merge country_market with year_product_group to get all combinations
combined_data <- merge(country_market, year_product_group, by = NULL)

unique_pairs <- unique(combined_data[, c("country", "market", "year", "product_group")])
combined_data$seq <- 1:nrow(unique_pairs)

combined_data_with_price <- merge(combined_data, unique_commodity, 
                                  by = c("country", "market", "year", 
                                         "product_group"), all.x = TRUE)

# Assign dummy for whether each product group reports any commodity in a given year
combined_data_with_price$price_dummy <- ifelse(is.na(combined_data_with_price$value),
                                               0, 1)

commodity_reporting <- combined_data_with_price %>%
  select(country, market, year, product_group, price_dummy) %>%
  distinct()

market_reporting_share <- commodity_reporting %>%
  group_by(country, year, product_group) %>%
  summarize(
    total_markets = n_distinct(market),                  # Total number of markets for the product group
    markets_with_value = sum(price_dummy, na.rm = TRUE)  # Number of markets reporting a value
  ) %>%
  mutate(
    market_reporting_share = markets_with_value / total_markets  # Calculate the share of markets reporting a value
  )

# Filter dataset by top 10 product groups to understand trends for most relevant
# product groups
top_10_product_groups <- product_group_count %>%
  top_n(10, num_countries) %>%
  select(product_group)

market_reporting <- market_reporting_share %>%
  filter(product_group %in% top_10_product_groups$product_group)


############### UNIT CHECK AND ANALYSIS #####################

unit_check <- unique_commodity %>%
  group_by(market, product) %>%
  summarise(unique_unit = n_distinct(unit) == 1)

unit_check_false <- unit_check %>%
  filter(unique_unit == FALSE)

unit_data <- unique_commodity %>%
  filter(market %in% unit_check_false$market & product %in% unit_check_false$product)

unit_data_market <- unique_commodity %>%
  filter(market %in% unit_check_false$market)

# Print intermediate result after filtering based on market
print(unique(unit_data_market$market))

unit_data_product <- unit_data_market %>%
  filter(market %in% unit_check_false$market & product %in% unit_check_false$product)

# Unique units for each product group
unique_units <- unique_commodity %>%
  group_by(product_group) %>%
  distinct(unit) %>%
  summarize(unique_units = paste(unit, collapse = ", "))


# Print the unique units within each product group
print(unique_units)

unit_list <- unique(unique_units$unique_units)

units_count <- unique_commodity %>%
  group_by(product_group, unit) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = unit, values_from = count, values_fill = 0)


### Correcting Units within product groups across the data
correct_units_values <- function(df) {
  df <- df %>%
    mutate(
      # If unit is "2_kg", divide value by 2 and update unit to "kg"
      value = ifelse(unit == "2_kg", value / 2, value),
      unit = ifelse(unit == "2_kg", "kg", unit),
      
      value = ifelse(unit == "50_kg", value / 50, value),
      unit = ifelse(unit == "50_kg", "kg", unit),
      
      value = ifelse(unit == "100_kg", value / 100, value),
      unit = ifelse(unit == "100_kg", "kg", unit),
      
      value = ifelse(unit == "6_lb", value * 0.453592, value),
      unit = ifelse(unit == "6_lb", "kg", unit),
      
      value = ifelse(unit == "gal", value * 3.78541, value),
      unit = ifelse(unit == "gal", "L", unit),
      
      value = ifelse(unit == "200_L", value / 200, value),
      unit = ifelse(unit == "200_L", "L", unit),
      
      value = ifelse(unit == "500_ml", value * 2, value),
      unit = ifelse(unit == "500_ml", "L", unit),
      
      value = case_when(
        unit %in% c("170_g", "350_g", "125_g", "226.8_g") ~ value / 1000,
        TRUE ~ value),
      unit = ifelse(unit %in% c("170_g", "350_g", "125_g", "226.8_g"), "kg", unit),
      
      unit = ifelse(unit == "500_g", "ea", unit),
      unit = ifelse(unit == "700_g", "ea", unit),
      
      value = ifelse(unit == "100_tubers", (value/100)*2.5, value),
      unit = ifelse(unit == "100_tubers", "kg", unit)
    )
  
  return(df)
}

unit_corrected_df <- correct_units_values(unique_commodity)


# Check units have been corrected
unique_units_corrected <- unit_corrected_df %>%
  group_by(product_group) %>%
  distinct(unit) %>%
  summarize(unique_units = paste(unit, collapse = ", "))


#################### REGRESSIONS #########################

# F-test and ANOVA
model <- lm(log_price ~ swarm_exposure + market + year + month + product, 
            data = unique_commodity)

# Perform F-test
f_test <- summary(model)$fstatistic
f_statistic <- f_test[1]
f_p_value <- pf(f_statistic, f_test[2], f_test[3], lower.tail = FALSE)

# Perform ANOVA
anova_result <- anova(model)

# Print results
cat("F-Test Results:\n")
cat("F-statistic:", f_statistic, "\n")
cat("p-value:", f_p_value, "\n\n")

cat("ANOVA Results:\n")
print(anova_result)



# Convert the data to a pdata.frame object
panel_data <- pdata.frame(unique_commodity, index = c("country", "year"))

# Run fixed effects model with individual fixed effects
fixed_model <- plm(log_price ~ dummy_dist + market + year + month + product, 
                   data = panel_data, model = "within")

# Summary of the fixed effects model
summary(fixed_model)

# Create Interaction Terms for Market-Commodity and Year-Month
unique_commodity$market_commodity <- interaction(unique_commodity$market,
                                                 unique_commodity$product)
unique_commodity$year_month <- interaction(unique_commodity$year,
                                           unique_commodity$month)
unique_commodity$market_product_group <- interaction(unique_commodity$market,
                                                     unique_commodity$product_group)

# Two separate datasets for analysis

# Dataset without Zimbabwe
panel_data_filtered <- unique_commodity %>%
  filter(country != "Zimbabwe")

# Dataset up to February 2019
panel_data_2019 <- unique_commodity %>%
  filter(year < 2019 | (year == 2019 & month == 1) | (year == 2019 & month == 2))


# Regressions for dataset without Zimbabwe
model_1 <- feols(log_price ~ swarm_exposure, data = panel_data_filtered)
model_2 <- feols(log_price ~ swarm_exposure | market, data = panel_data_filtered)
model_3 <- feols(log_price ~ swarm_exposure | product, data = panel_data_filtered)
model_4 <- feols(log_price ~ swarm_exposure | year + month, data = panel_data_filtered)
model_5 <- feols(log_price ~ swarm_exposure | market + product, 
                 data = panel_data_filtered)
model_6 <- feols(log_price ~ swarm_exposure | market + year + month,
                 data = panel_data_filtered)
model_7 <- feols(log_price ~ swarm_exposure | market + year + month + product,
                 data = panel_data_filtered)
model_8 <- feols(log_price ~ swarm_exposure | market_commodity + year_month,
                 data = panel_data_filtered)
model_9 <- feols(log_price ~ swarm_exposure | market_product_group + year_month,
                 data = panel_data_filtered)

regression_results <- list( model_2, model_3, model_4, model_5, model_6, model_7,
                            model_8, model_9)

# Use esttable to display results
esttable(regression_results)


# Regressions for dataset up to February 2019
model1 <- feols(log_price ~ swarm_exposure, data = panel_data_2019)
model2 <- feols(log_price ~ swarm_exposure | market, data = panel_data_2019)
model3 <- feols(log_price ~ swarm_exposure | product, data = panel_data_2019)
model4 <- feols(log_price ~ swarm_exposure | year + month, data = panel_data_2019)
model5 <- feols(log_price ~ swarm_exposure | market + product, data = panel_data_2019)
model6 <- feols(log_price ~ swarm_exposure | market + year + month,
                data = panel_data_2019)
model7 <- feols(log_price ~ swarm_exposure | market + year + month + product,
                data = panel_data_2019)
model8 <- feols(log_price ~ swarm_exposure | market_commodity + year_month,
                data = panel_data_2019)
model9 <- feols(log_price ~ swarm_exposure | market__product_group + year_month,
                data = panel_data_2019)


regression_results_2019 <- list(model2, model3, model4, model5, model6, model7
                                , model8, model9)

# Use esttable to display results
esttable(regression_results_2019)
