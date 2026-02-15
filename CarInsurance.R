#########################################################
#         1. Load required libraries                    #
#########################################################
library(cowplot)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
#library(readr)
library(lubridate)
library(kableExtra)
library(tidyr)
library(readxl)
library(stringr)
library(fontawesome)

#########################################################
#         2. Data Preparation                           #
#########################################################
# Load the CSV file into a data frame
insurance_data <- read.csv("data/Motor vehicle insurance data.csv", sep = ";", stringsAsFactors = TRUE)

# Convert factor dates to Date format
insurance_data <- insurance_data %>%
  mutate(
    Date_start_contract = dmy(as.character(Date_start_contract)),
    Date_last_renewal = dmy(as.character(Date_last_renewal)),
    Date_next_renewal = dmy(as.character(Date_next_renewal)),
    Date_birth = dmy(as.character(Date_birth)),
    Date_driving_licence = dmy(as.character(Date_driving_licence)),
    Date_lapse = dmy(as.character(Date_lapse))
  )

# Number of observations and variables :
data_summary <- insurance_data %>% summarise(observations = n(),
                                             variables = ncol(.))
data_summary

# View the first few rows of the data frame
head(insurance_data)

# Check the structure of the data frame
str(insurance_data)

# List the variables from the Excel spreadsheet
variables <- read_excel("data/Descriptive of the variables.xlsx")
variables

# Summary of the driver details
driver_summary <- insurance_data %>%
  select("Date_birth", "Date_driving_licence", "Seniority", "Area", "Second_driver") %>%
  summary()

# Summary of the car details
car_summary_1 <- insurance_data %>%
  select("N_doors", "Power", "Cylinder_capacity", "Type_fuel", "Length", "Weight") %>%
  summary()

car_summary_2 <- insurance_data %>%
  select("Year_matriculation", "Value_vehicle") %>%
  summary()

# Summary of the policy details
policy_summary_1 <- insurance_data %>%
  select("ID", "Date_start_contract", "Date_last_renewal", "Date_next_renewal", "Distribution_channel") %>%
  summary()

policy_summary_2 <- insurance_data %>%
  select("Policies_in_force", "Max_policies", "Max_products", "Lapse", "Date_lapse") %>%
  summary()

policy_summary_3 <- insurance_data %>%
  select("Payment", "Premium", "Type_risk") %>%
  summary()

policy_summary_4 <- insurance_data %>%
  select("Cost_claims_year", "N_claims_year", "N_claims_history", "R_Claims_history") %>%
  summary()

#########################################################
#         2. Claims Summary                             #
#########################################################
# Load the CSV file into a data frame
claims_data <- read.csv("data/sample type claim.csv", sep = ";", stringsAsFactors = TRUE)

# Number of observations : 7,366
str(claims_data)
head(claims_data)

# Number of claims from the main dataset : 19,646
nrow(insurance_data %>% filter(N_claims_year > 0))

# Count occurrences of each Claims_type
claims_type_order <- claims_data %>%
  group_by(Claims_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  pull(Claims_type)

# Reorder Claims_type based on the counts
claims_data$Claims_type <- factor(claims_data$Claims_type, levels = claims_type_order)

levels(claims_data$Claims_type)

# Create the bar plot for Claims_type
ggplot(data = claims_data, aes(x = Claims_type)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Repartition of Claims Type",
       x = "Claims Type",
       y = "Count") +
  theme_minimal()

#########################################################
#         3. Data Analysis                              #
#########################################################

# Calculate the total number of days covered by the data :
total_days <- insurance_data %>%
  summarize(days = as.integer(max(Date_last_renewal) - min(Date_last_renewal))) %>%
  pull(days)

# Calculate the total number of policies :
total_policies <- insurance_data %>%
  summarize(policies = n_distinct(ID)) %>%
  pull(policies)

# Calculate the total number of policies with at least one claim :
total_policies_with_claims <- insurance_data %>%
  filter(N_claims_year > 0) %>%
  summarize(policies = n_distinct(ID)) %>%
  pull(policies)

# Calculate the total number of claims :
total_claims <- insurance_data %>%
  summarize(claims = sum(N_claims_year)) %>%
  pull(claims)

# Calculate the total cost :
total_cost <- insurance_data %>%
  summarize(Total_cost = sum(Cost_claims_year)) %>%
  pull(Total_cost)

# Summarize the data per year :
# number of days, policies, policies with claims, claims and total cost
insurance_year <- insurance_data %>%
  mutate(Year = year(Date_last_renewal)) %>%
  select(ID, Year, N_claims_year, Cost_claims_year, Date_last_renewal) %>%
  group_by(Year) %>%
  summarize(
    Days = as.integer(max(Date_last_renewal) - min(Date_last_renewal)),
    Policies = n_distinct(ID),
    Policies_with_claims = sum(N_claims_year > 0),
    Total_claims = sum(N_claims_year),
    Total_Cost = sum(Cost_claims_year)
  ) %>%
  mutate(Year = as.character(Year)) %>%
  bind_rows(data.frame(Year = "Total",
                       Days = total_days,
                       Policies = total_policies,
                       Policies_with_claims = total_policies_with_claims,
                       Total_claims = total_claims,
                       Total_Cost = total_cost))


# Replace underscores with spaces in headers
colnames(insurance_year) <- gsub("_", " ", colnames(insurance_year))
insurance_year

# We consider a policy is new if the Date_last_renewal and the Date_start_contract are in the same year,
# and it will be terminating if Date_last_renewal and Date_lapse are in the same year.
# We display the number of policies per year, number of new and terminating policies,
# and the proportion of policies / new policies / terminating policies with a claim in the current year :
insurance_mv <- insurance_data %>%
  mutate(Year = year(Date_last_renewal),
         Year_Start = year(Date_start_contract),
         Year_End = year(Date_lapse)) %>%
  select(ID, N_claims_year, Year, Year_Start, Year_End) %>%
  group_by(Year) %>%
  summarise(
    Total_Policies = n_distinct(ID),
    New_Policies = sum(Year == Year_Start),
    Terminating_Policies = sum(Year == Year_End, na.rm = TRUE),
    Proportion_All_With_Claims = round(sum(N_claims_year > 0) / Total_Policies * ifelse(Total_Policies > 0, 1, 0), 3),
    Proportion_New_With_Claims = round(sum(N_claims_year[Year == Year_Start] > 0) / New_Policies * ifelse(New_Policies > 0, 1, 0), 3),
    Proportion_Terminating_With_Claims = round(sum(N_claims_year[Year == Year_End] > 0, na.rm = TRUE) / Terminating_Policies * ifelse(Terminating_Policies > 0, 1, 0), 3)
  ) %>%
  mutate(Year = as.character(Year))

# Replace underscores with spaces in headers
colnames(insurance_mv) <- gsub("_", " ", colnames(insurance_mv))
insurance_mv

#########################################################
#         3.1 Correlation matrix                        #
#########################################################

# We have 30 variables, we need to remove some of them
# The difference between Date_last_renewal and Date_next_renewal is exactly one year (365 or 366 days),
# We don't need to keep one of them and will only consider Date_last_renewal :
diff_renewal_days <- insurance_data$Date_next_renewal - insurance_data$Date_last_renewal
min(diff_renewal_days)
max(diff_renewal_days)

# We add Age and Driving_age columns as they may impact the number of claims and premium,
# We only have Date_birth and Date_driving_licence but they may not be relevant as
# people of same age for example may pay the same price regardless of the current year.
insurance_data <- insurance_data %>%
  mutate(
    Year_last_renewal = year(Date_last_renewal),
    Age = as.integer((Year_last_renewal - year(Date_birth))),
    Driving_age = as.integer((Year_last_renewal - year(Date_driving_licence)))
  )

# We have many NAs in Type_fuel and Length columns, we need to check if they can be related to other variables,
# We assume that Diesel car are heavier than Petrol ones,
# and the length and the weight are related :
insurance_data %>%
  filter(!is.na(Length)) %>%
  mutate(
    Type_fuel = as.character(Type_fuel),  # Convert factor to character
    Type_fuel = replace_na(Type_fuel, "0"),  # Replace NA in Type_fuel with "0"
    Type_fuel = ifelse(Type_fuel == "P", 1, 2)
    ) %>%
  select(Type_fuel, Length, Weight) %>%
  cor()

# Copy the insurance_data to a temporary data frame :
tmp_data <- insurance_data

# Replace NA values for Type_fuel :
# 0 for NA, 1 for Petrol (P) and 2 for Diesel (D)
tmp_data <- tmp_data %>%
  mutate(
    Type_fuel = as.character(Type_fuel),
    Type_fuel = replace_na(Type_fuel, "0"),
    Type_fuel = ifelse(Type_fuel == "P", 1, 2)
  )

# Define the date columns
date_columns <- c("Date_start_contract", "Date_last_renewal", 
                  "Date_next_renewal", "Date_birth", 
                  "Date_driving_licence", "Date_lapse")

# Convert each date column to numeric
for (col in date_columns) {
  tmp_data[[col]] <- as.numeric(tmp_data[[col]])
}
# As Date_lapse is numeric
tmp_data$Date_lapse[is.na(tmp_data$Date_lapse)] <- 0
# We remove :
# Date_birth (replaced by Age),
# Date_driving_licence (replaced by Driving_age),
# Length (highly correlated to Weight; cor = 0.83),
# Date_next_renewal (highly correlated to Date_last_renewal),
# Year_last_renewal (not used),
# ID (not used).
tmp_data <- tmp_data %>%
  select(-Date_birth, -Date_driving_licence, -Length, -Date_next_renewal, -Year_last_renewal, -ID)
str(tmp_data)

# We start by displaying the correlations between variables related to the car,
# the policy holder and some policy attributes (Premium, N_claims_year and Cost_claims_year) :
correlation_matrix_car <- tmp_data %>%
  select(Power, Cylinder_capacity, Weight, N_doors, Type_risk, Type_fuel,
         Premium, Value_vehicle, Year_matriculation, Second_driver, Age, Driving_age,
         N_claims_year, Cost_claims_year) %>%
  cor()
correlation_matrix_car

# Get the absolute values of the correlation matrix
abs_correlation <- abs(correlation_matrix_car)

# Order the variables based on the sum of absolute correlations
order_vec <- order(rowSums(abs_correlation), decreasing = FALSE)

# Reorder the correlation matrix
correlation_matrix_car <- correlation_matrix_car[order_vec, order_vec]
correlation_matrix_car

# We then display the correlations between variables related to the policy,
# and some car attributes (Power, Date_matriculation and Type_risk) :
correlation_matrix_policy <- tmp_data %>%
  select(Date_start_contract, Date_last_renewal, Age, Distribution_channel, Seniority,
         Policies_in_force, Max_policies, Max_products, Lapse, Date_lapse, Payment,
         Premium, Cost_claims_year, N_claims_year, N_claims_history, R_Claims_history,
         Type_risk, Area, Second_driver, Year_matriculation, Power, Value_vehicle) %>%
  cor()
correlation_matrix_policy

# Get the absolute values of the correlation matrix
abs_correlation <- abs(correlation_matrix_policy)

# Order the variables based on the sum of absolute correlations
order_vec <- order(rowSums(abs_correlation), decreasing = FALSE)

# Reorder the correlation matrix
correlation_matrix_policy <- correlation_matrix_policy[order_vec, order_vec]
correlation_matrix_policy

# Display the correlation matrix :
ggcorrplot(correlation_matrix_policy,
           lab_size = 3,
           ggtheme = theme_dark(base_size = 9)) +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "top"
  )

# Remove unnecessary objects :
rm(tmp_data, date_columns, abs_correlation, order_vec)

# Create a new matrix to keep only the relevant correlations
relevant_correlations <- correlation_matrix_policy[, c("N_claims_year", "Cost_claims_year")]
relevant_correlations

ggcorrplot(relevant_correlations, 
           lab_size = 3,
           ggtheme = theme_dark(base_size = 9)) +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "top"
  )

#########################################################
#         3.2 Claim Policy Ratios                       #
#########################################################

# Claim Policy Ratio compared to Power, Value_vehicle, Age and Driving_age
claim_policy_ratio <- function(insurance_data, x_var, x_max) {
  insurance_data %>%
    select(ID, Year = Year_last_renewal, !!sym(x_var), N_claims_year) %>%
    group_by(Year, !!sym(x_var)) %>%
    summarize(Nb_policy_claims = sum(N_claims_year > 0, na.rm = TRUE),
              #              Nb_claims = sum(N_claims_year, na.rm = TRUE),
              Nb_policies = n(),
              .groups = 'drop') %>%
    mutate(Year = as.factor(Year),
           Claim_Policy_Ratio = Nb_policy_claims / Nb_policies) %>%
#    filter(Nb_policies > 1) %>%
    ggplot(aes_string(x = x_var, y = "Claim_Policy_Ratio", color = "Year")) +
    geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
    labs(x = x_var, y = "Claim Policy Ratio") +
    xlim(0, x_max) +
    ylim(0, NA) +
    theme_minimal() +
    theme(text = element_text(size = 9), legend.position = "top")
}

# Number of Policies compared to Power, Value_vehicle, Age and Driving_age
nb_policies <- function(insurance_data, x_var, x_max) {
  insurance_data %>%
    select(ID, Year = Year_last_renewal, !!sym(x_var), N_claims_year) %>%
    group_by(Year, !!sym(x_var)) %>%
    summarize(Nb_policies = n(),
              .groups = 'drop') %>%
    mutate(Year = as.factor(Year)) %>%
    ggplot(aes_string(x = x_var, y = "Nb_policies", color = "Year")) +
    geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
    labs(x = x_var, y = "Nb Policies") +
    xlim(0, x_max) +
    ylim(0, NA) +
    theme_minimal() +
    theme(text = element_text(size = 9), legend.position = "top")
}

plot_grid(
  claim_policy_ratio(insurance_data, "Power", 250),
  claim_policy_ratio(insurance_data, "Value_vehicle", 60000),
  nb_policies(insurance_data, "Power", 250),
  nb_policies(insurance_data, "Value_vehicle", 60000),
  #  claim_policy_ratio(insurance_data, "Age"),
  #  claim_policy_ratio(insurance_data, "Driving_age"),
  #  claim_policy_ratio(insurance_data, "Premium"),
  #  claim_policy_ratio(insurance_data, "Seniority"),
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)







#########################################################
#         3.2 Claims : previous claims                  #
#########################################################

# Claims
claims_data <- insurance_data %>%
  group_by(Year_last_renewal, ID) %>%
  summarise(total_claims = sum(N_claims_year, na.rm = TRUE))
claims_data

# Total number of N_claims_year
total_claims <- sum(claims_data$total_claims, na.rm = TRUE)
total_claims
# Prepare to count IDs based on conditions
max_claims <- max(claims_data$total_claims, na.rm = TRUE)
max_claims

# Create a summary of counts for each year and claim category
claims_summary <- claims_data %>%
  group_by(Year_last_renewal, total_claims) %>%
  summarise(Count_claims = n(), .groups = 'drop') %>%
  complete(Year_last_renewal, total_claims = 0:max_claims, fill = list(Count_claims = 0)) %>%
  rename(Nb_claims = total_claims) %>%
  # Calculate cumulative counts
  group_by(Year_last_renewal) %>%
  mutate(Cumulative_claims = rev(cumsum(rev(Count_claims))),
         percentage = Cumulative_claims / lag(Cumulative_claims),
         percentage = ifelse(is.na(percentage), 100, percentage * 100)
         ) %>%
  ungroup()

# Display the resulting summary
claims_summary

# Risk of Raising a New Claim based on number of claims raised during the year
claims_summary %>% filter(Nb_claims > 0 & Cumulative_claims > 15) %>%
  ggplot(aes(x = Nb_claims, y = percentage)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(title = "Risk of Raising a New Claim",
       x = "Number of Previous Claims",
       y = "Risk (%)") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Risk of Raising a New Claim based on history (total claims raised)
# We only display data for 2017 (last full year)
insurance_data %>%
  group_by(Year_last_renewal) %>%
  select(Year_last_renewal, ID, N_claims_year, N_claims_history) %>%
  filter(N_claims_year > 0 & Year_last_renewal == 2017) %>%
  ggplot(aes(x = N_claims_history, y = N_claims_year)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Number of Previous Claims",
       y = "Number of claims for current year") +
  theme_minimal() +
  theme(text = element_text(size = 9))

#########################################################
#         3.4 Cost Policy Ratios                        #
#########################################################

# Cost Policy Ratio compared to Power, Value_vehicle, Age and Driving_age
cost_policy_ratio <- function(insurance_data, x_var) {
  insurance_data %>%
    select(ID, Year = Year_last_renewal, !!sym(x_var), Cost_claims_year) %>%
#    filter(Year > 2015) %>%
    group_by(Year, !!sym(x_var)) %>%
    summarize(Total_cost = sum(Cost_claims_year, na.rm = TRUE),
              Nb_policies = n(),
              .groups = 'drop') %>%
    mutate(Year = as.factor(Year),
           Claim_Policy_Ratio = Total_cost / Nb_policies) %>%
    ggplot(aes_string(x = x_var, y = "Claim_Policy_Ratio", color = "Year")) +
    geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
    labs(x = x_var, y = "Cost Policy Ratio") +
    ylim(0, NA) +
    theme_minimal() +
    theme(text = element_text(size = 9), legend.position = "top")
}

plot_grid(
  cost_policy_ratio(insurance_data, "Power"),
  cost_policy_ratio(insurance_data, "Value_vehicle"),
  cost_policy_ratio(insurance_data, "Age"),
  cost_policy_ratio(insurance_data, "Driving_age"),
#  cost_policy_ratio(insurance_data, "Premium"),
#  cost_policy_ratio(insurance_data, "Seniority"),
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

#########################################################
#         3.5 Claim Policy Ratios / Area                #
#########################################################

# Claim Policy Ratio compared to Area
area_summary <- insurance_data %>%
  select(ID, Year = Year_last_renewal, Area, N_claims_year, Cost_claims_year) %>%
  group_by(Year, Area) %>%
  summarize(
    Nb_policy_claims = sum(N_claims_year > 0, na.rm = TRUE),
    Total_cost = sum(Cost_claims_year, na.rm = TRUE),
    Nb_policies = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Year = as.factor(Year),
    Area = recode(Area, `0` = "Rural", `1` = "Urban"),  # Recode Area
    Claim_Policy_Ratio = Nb_policy_claims / Nb_policies,
    Cost_Policy_Ratio = Total_cost / Nb_policies
  )

area_summary

# Claim Policy Ratio compared to Area : Nb of Claims and cost
plot_grid(
  area_summary %>%
    ggplot(aes(x = Year, y = Claim_Policy_Ratio, fill = Area)) +
    geom_bar(stat = "identity", position = "dodge") +  # Use bar plot for binary Area
    labs(x = "Year", y = "Claim Policy Ratio") +
    ylim(0, NA) +
    theme_minimal() +
    theme(text = element_text(size = 9), legend.position = "top"),

  area_summary %>%
    ggplot(aes(x = Year, y = Cost_Policy_Ratio, fill = Area)) +
    geom_bar(stat = "identity", position = "dodge") +  # Use bar plot for binary Area
    labs(x = "Year", y = "Cost Policy Ratio") +
    ylim(0, NA) +
    theme_minimal() +
    theme(text = element_text(size = 9), legend.position = "top"),
  
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

#########################################################
#         3.5 Claim Policy Ratios / Type Risk           #
#########################################################

# Claim Policy Ratio compared to Type_risk
type_risk_summary <- insurance_data %>%
  select(ID, Year = Year_last_renewal, Type_risk, N_claims_year, Cost_claims_year) %>%
  group_by(Year, Type_risk) %>%
  summarize(
    Nb_policy_claims = sum(N_claims_year > 0, na.rm = TRUE),
    Total_cost = sum(Cost_claims_year, na.rm = TRUE),
    Nb_policies = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Year = as.factor(Year),
    Type_risk = recode(Type_risk, `1` = "Motorbikes",
                       `2` = "Vans",
                       `3` = "Passenger cars",
                       `4` = "Agricultural vehicles"),
    Claim_Policy_Ratio = Nb_policy_claims / Nb_policies,
    Cost_Policy_Ratio = Total_cost / Nb_policies
  )
type_risk_summary

# Claim Policy Ratio compared to Type Risk : Nb of Claims and cost
plot_grid(
  type_risk_summary %>%
    ggplot(aes(x = Year, y = Claim_Policy_Ratio, fill = Type_risk)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Year", y = "Claim Policy Ratio") +
    ylim(0, NA) +
    theme_minimal() +
    theme(text = element_text(size = 9), legend.position = "top") +
    guides(fill = guide_legend(label.theme = element_text(size = 8), nrow=2, byrow=TRUE)),
  
  type_risk_summary %>%
    ggplot(aes(x = Year, y = Cost_Policy_Ratio, fill = Type_risk)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Year", y = "Cost Policy Ratio") +
    ylim(0, NA) +
    theme_minimal() +
    theme(text = element_text(size = 9), legend.position = "top") +
    guides(fill = guide_legend(label.theme = element_text(size = 8), nrow=2, byrow=TRUE)),
  
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

# Exit the script
stop("Stopping the script.")




# Number of different policies per year and total :
insurance_year <- insurance_data %>%
  mutate(Year = year(Date_last_renewal)) %>%
  select(ID, Year) %>%
  group_by(Year) %>%
  summarise(Policies = n_distinct(ID)) %>%
  arrange(Year) %>%
  mutate(Year = as.character(Year)) %>%
  bind_rows(data.frame(Year = "Total", Policies = total_unique_ids))

# Replace underscores with spaces in headers
colnames(insurance_year) <- gsub("_", " ", colnames(insurance_year))

insurance_year

total_days <- insurance_data %>%
  summarize(days = as.integer(max(Date_last_renewal) - min(Date_last_renewal))) %>%
  pull(days)
total_policies <- insurance_data %>%
  summarize(policies = n_distinct(ID)) %>%
  pull(policies)
total_policies_with_claims <- insurance_data %>%
  filter(N_claims_year > 0) %>%
  summarize(policies = n_distinct(ID)) %>%
  pull(policies)
total_claims <- insurance_data %>%
  summarize(claims = sum(N_claims_year)) %>%
  pull(claims)
total_cost <- insurance_data %>%
  summarize(Total_cost = sum(Cost_claims_year)) %>%
  pull(Total_cost)

insurance_data %>%
  mutate(Year = year(Date_last_renewal)) %>%
  select(ID, Year, N_claims_year, Cost_claims_year, Date_last_renewal) %>%
  group_by(Year) %>%
  summarize(
    Days = as.integer(max(Date_last_renewal) - min(Date_last_renewal)),
    Policies = n_distinct(ID),
    Policies_with_claims = sum(N_claims_year > 0),
    Total_claims = sum(N_claims_year),
    Total_Cost = sum(Cost_claims_year)
  ) %>%
  mutate(Year = as.character(Year)) %>%
  bind_rows(data.frame(Year = "Total",
                       Days = total_days,
                       Policies = total_policies,
                       Policies_with_claims = total_policies_with_claims,
                       Total_claims = total_claims,
                       Total_Cost = total_cost))








insurance_data %>%
  select(ID, Year = Year_last_renewal, Age, N_claims_year) %>%
  filter(Year == 2015) %>%
  group_by(Year, Age) %>%
  summarize(Nb_policy_claims = sum(N_claims_year > 0, na.rm = TRUE),
            #              Nb_claims = sum(N_claims_year, na.rm = TRUE),
            Nb_policies = n(),
            .groups = 'drop') %>%
  filter(Nb_policies >= 10) %>%
  mutate(Year = as.factor(Year),
         Claim_Policy_Ratio = Nb_policy_claims / Nb_policies)
  
summary(tmp)






















area_summary <- insurance_data %>%
  select(ID, Year = Year_last_renewal, Area, N_claims_year, Cost_claims_year) %>%
  group_by(Year, Area) %>%
  summarize(
    Nb_claims = sum(N_claims_year, na.rm = TRUE),
    Total_cost = sum(Cost_claims_year, na.rm = TRUE),
    Nb_policies = n(),
    Nb_policy_claims = sum(N_claims_year > 0, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Year = as.factor(Year),
    Area = recode(Area, `0` = "Rural", `1` = "Urban"),  # Recode Area
    Claim_Policy_Ratio = Nb_policy_claims / Nb_policies,
    Cost_Policy_Ratio = Total_cost / Nb_policies
  )
area_summary





insurance_data

insurance_data %>%
  select(ID, Year = Year_last_renewal, Type_risk, N_claims_year) %>%
  #  filter(Year > 2015) %>%
  group_by(Year, Type_risk) %>%
  summarize(
    Nb_claims = sum(N_claims_year, na.rm = TRUE),
    Nb_policies = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Year = as.factor(Year),
    Type_risk = recode(Type_risk, `1` = "Motorbikes",
                       `2` = "Vans",
                       `3` = "Passenger cars",
                       `4` = "Agricultural vehicles"),
    Claim_Policy_Ratio = Nb_claims / Nb_policies
  )
















insurance_data %>%
  select(ID, Year = Year_last_renewal, Area, N_claims_year) %>%
  filter(N_claims_year > 0) %>%
  group_by(Year, Area, ID) %>%
  summarize(
    Nb_claims = sum(N_claims_year, na.rm = TRUE),
    Nb_policies = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Year = as.factor(Year),
    Area = recode(Area, `0` = "Rural", `1` = "Urban"),  # Recode Area
    Claim_Policy_Ratio = Nb_claims / Nb_policies
  ) %>%
  ggplot(aes(x = Year, y = Claim_Policy_Ratio, fill = Area)) +
  geom_boxplot(position = position_dodge(width = 0.75), outlier.shape = NA) +  # Use boxplot for continuous variable
  labs(x = "Year", y = "Claim Policy Ratio") +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9))


insurance_data %>%
  select(ID, Year = Year_last_renewal, Area, N_claims_year) %>%
  # filter(Year > 2015) %>%
  group_by(Year, Area) %>%
  summarize(
    Nb_claims = sum(N_claims_year, na.rm = TRUE),
    Nb_policies = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Year = as.factor(Year),
    Area = recode(Area, `0` = "Rural", `1` = "Urban"),  # Recode Area
    Claim_Policy_Ratio = Nb_claims / Nb_policies
  ) %>%
  ggplot(aes(x = Year, y = Claim_Policy_Ratio, fill = Area)) +
  geom_boxplot(position = position_dodge(width = 0.75), outlier.shape = NA) +  # Use boxplot for continuous variable
  labs(x = "Year", y = "Claim Policy Ratio") +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9))

























# Define function to create plots
plot_insurance_data <- function(insurance_data, x_var, show_legend = FALSE) {
  plot <- insurance_data %>%
    select(ID, Year_last_renewal, !!sym(x_var), N_claims_year) %>%
    filter(Year_last_renewal > 2015) %>%
    group_by(Year_last_renewal, !!sym(x_var)) %>%
    summarize(Nb_claims = sum(N_claims_year, na.rm = TRUE),
              Nb_policies = n(),
              .groups = 'drop') %>%
    mutate(Year_last_renewal = as.factor(Year_last_renewal),
           Claim_Policy_Ratio = Nb_claims / Nb_policies) %>%
    ggplot(aes_string(x = x_var, y = "Claim_Policy_Ratio", color = "Year_last_renewal")) +
    geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
    labs(x = x_var, y = "Claim Policy Ratio") +
    ylim(0, NA) +
    theme_minimal() +
    theme(text = element_text(size = 9))
  
  # Show legend if requested
  if (show_legend) {
    plot + theme(legend.position = "right")  # Keep the legend for the primary plot
  } else {
    plot + theme(legend.position = "none")  # Remove legend for other plots
  }
}

# Create the plots with a common legend
plot_power <- plot_insurance_data(insurance_data, "Power", show_legend = TRUE)
plot_value_vehicle <- plot_insurance_data(insurance_data, "Value_vehicle")
plot_age <- plot_insurance_data(insurance_data, "Age")
plot_driving_age <- plot_insurance_data(insurance_data, "Driving_age")
plot_premium <- plot_insurance_data(insurance_data, "Premium")
plot_seniority <- plot_insurance_data(insurance_data, "Seniority")

# Extracting the common legend from the first plot
legend <- get_legend(plot_power)

# Combine plots and the common legend
final_plot <- plot_grid(
  plot_power,
  plot_value_vehicle,
  plot_age,
  plot_driving_age,
  plot_premium,
  plot_seniority,
  ncol = 2,
  rel_heights = c(1, 1, 1, 1, 1, 1)
)

# Combine plots with legend at the bottom
plot_grid(final_plot, legend, ncol = 1, rel_heights = c(1, 0.1))













insurance_data %>%
  select(ID, Year_last_renewal, Power, N_claims_year) %>%
  group_by(Year_last_renewal, Power) %>%
  summarize(Nb_claims = sum(N_claims_year, na.rm = TRUE),
            Nb_policies = n(),
            .groups = 'drop') %>%
  mutate(Year_last_renewal = as.factor(Year_last_renewal))

insurance_data %>%
  select(ID, Year_last_renewal, Power, N_claims_year) %>%
  group_by(Year_last_renewal, Power) %>%
  summarize(Nb_claims = sum(N_claims_year, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Year_last_renewal = as.factor(Year_last_renewal)) %>%
  ggplot(aes(x = Power, y = Nb_claims, color = Year_last_renewal)) +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Power",
       y = "Number of claims") +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9))


  
  
  
  



head(tmp, 50)
min(tmp$Power)
max(tmp$Power)
nrow(tmp)
tmp %>% slice(1:50)
%>%
  ggplot(aes(x = Power, y = Nb_claims)) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Power",
       y = "Number of claims for current year") +
  theme_minimal() +
  theme(text = element_text(size = 9))


    filter(N_claims_year > 0 & Year_last_renewal == 2017) %>%
  select(Year_last_renewal, ID, Power, N_claims_year)

insurance_data %>%
  group_by(Year_last_renewal) %>%
  filter(N_claims_year > 0 & Year_last_renewal == 2017) %>%
  ggplot(aes(x = Power, y = N_claims_year)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Power",
       y = "Number of claims for current year") +
  theme_minimal() +
  theme(text = element_text(size = 9))

insurance_data %>%
  group_by(Year_last_renewal) %>%
  filter(N_claims_year > 0 & Year_last_renewal == 2017) %>%
  ggplot(aes(x = Value_vehicle, y = N_claims_year)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Value Vehicle",
       y = "Number of claims for current year") +
  theme_minimal() +
  theme(text = element_text(size = 9))

insurance_data %>%
  group_by(Year_last_renewal) %>%
  filter(N_claims_year > 0 & Year_last_renewal == 2017) %>%
  ggplot(aes(x = Age, y = N_claims_year)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Age",
       y = "Number of claims for current year") +
  theme_minimal() +
  theme(text = element_text(size = 9))

insurance_data %>%
  group_by(Year_last_renewal) %>%
  filter(N_claims_year > 0 & Year_last_renewal == 2017) %>%
  ggplot(aes(x = Driving_age, y = N_claims_year)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Driving Age",
       y = "Number of claims for current year") +
  theme_minimal() +
  theme(text = element_text(size = 9))

insurance_data %>%
  group_by(Year_last_renewal) %>%
  filter(N_claims_year > 0 & Year_last_renewal == 2017) %>%
  ggplot(aes(x = Premium, y = N_claims_year)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Premium",
       y = "Number of claims for current year") +
  theme_minimal() +
  theme(text = element_text(size = 9))

insurance_data %>%
  group_by(Year_last_renewal) %>%
  filter(N_claims_year > 0 & Year_last_renewal == 2017) %>%
  ggplot(aes(x = Seniority, y = N_claims_year)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Seniority",
       y = "Number of claims for current year") +
  theme_minimal() +
  theme(text = element_text(size = 9))









insurance_data %>%
  group_by(Year_last_renewal) %>%
  filter(Cost_claims_year > 0 & Year_last_renewal == 2017) %>%
  ggplot(aes(x = Driving_age, y = Cost_claims_year)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Driving Age",
       y = "Cost of claims for current year") +
  theme_minimal() +
  theme(text = element_text(size = 9))










# Correlation of variables related to car details :
correlation_car <- insurance_data %>%
  select("N_doors", "Power", "Cylinder_capacity", "Type_fuel", "Length", "Weight",
         "Year_matriculation", "Value_vehicle") %>%
  filter(!is.na(Length) & !is.na(Type_fuel)) %>%
  mutate(Type_fuel = ifelse(Type_fuel == "P", 0, 1)) %>%
  cor()

# Display the correlation matrix
correlation_car

# Graphical view of the correlations :
ggcorrplot(correlation_car, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           ggtheme = theme_dark(base_size = 9)) +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9)
  )
    



# Print the correlation matrix
print(correlation_matrix)




# Summary of the data
# Calculate the number of variables
num_vars <- ncol(insurance_data)

# Create an empty list to store summaries
summary_data <- list()

# Loop through columns in increments of 5
for (i in seq(1, num_vars, by = 5)) {
  # Select the next 5 variables and create a summary
  summary_df <- insurance_data %>%
    select(i:min(i + 4, num_vars)) %>%
    summary()
  
  # Store the summary in the list
  summary_data[[length(summary_data) + 1]] <- summary_df
}

# Now you can access the summaries using numeric indexing
# For example:
summary_data[[1]]  # Access the first summary
summary_data[[2]]  # Access the second summary







# Period covered by the dataset :
min(insurance_data$Date_last_renewal)
max(insurance_data$Date_last_renewal)





# Create new variables for analysis
insurance_data <- insurance_data %>%
  mutate(
    Age_at_review = year(Date_start_contract) - year(Date_birth),
    Months_since_last_renewal = as.numeric(difftime(Sys.Date(), Date_last_renewal, units = "days")) / 30
  )

# Exploratory Data Analysis (EDA)
summary_stats <- insurance_data %>%
  summarise(
    Average_Premium = mean(Premium, na.rm = TRUE),
    Average_Cost_Claims_Year = mean(Cost_claims_year, na.rm = TRUE),
    Total_N_Claims = sum(N_claims_year, na.rm = TRUE)
  )

print(summary_stats)

# Visualization: Premium distribution
ggplot(insurance_data, aes(x = Premium)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Premiums", x = "Premium", y = "Frequency")

# Visualization: Age vs. Number of Claims
ggplot(insurance_data, aes(x = Age_at_review, y = N_claims_year)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Age at Review vs. Number of Claims", x = "Age at Review", y = "Number of Claims")