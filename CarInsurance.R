#########################################################
#         0. Load required libraries                    #
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
#library(openxlsx)
library(stringr)
#library(fontawesome)
library(scales)
#library(glue)
library(purrr)
library(caret)
library(neuralnet)
library(randomForest)

#########################################################
#         3. Dataset                                    #
#########################################################

#########################################################
#         3.3 Files                                     #
#########################################################

# Load the CSV file into a data frame
insurance_data <- read.csv("data/Motor vehicle insurance data.csv", sep = ";", stringsAsFactors = TRUE)

# Number of observations and variables :
insurance_data %>% summarise(observations = n(),
                                             variables = ncol(.))

# View the first few rows of the data frame
head(insurance_data)

# Check the structure of the data frame
str(insurance_data)

# Period covered, we need first to convert the Date_last_renewal to a date format :
# Convert factor dates to Date format
insurance_data <- insurance_data %>%
  mutate(Date_last_renewal = dmy(as.character(Date_last_renewal)))
min(insurance_data$Date_last_renewal)
max(insurance_data$Date_last_renewal)

# Load the "sample type claim.csv" file into a data frame
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
#         3.4 Variables (description)                   #
#########################################################

# Get the description of the variables from the "Descriptive of the variables.xlsx" Excel spreadsheet
variables <- read_excel("data/Descriptive of the variables.xlsx")
variables

#########################################################
#         4. Data Transformation                        #
#########################################################

# Convert factor dates to Date format
insurance_data <- insurance_data %>%
  mutate(
    Date_start_contract = dmy(as.character(Date_start_contract)),
#    Date_last_renewal = dmy(as.character(Date_last_renewal)),
    Date_next_renewal = dmy(as.character(Date_next_renewal)),
    Date_birth = dmy(as.character(Date_birth)),
    Date_driving_licence = dmy(as.character(Date_driving_licence)),
    Date_lapse = dmy(as.character(Date_lapse))
  )

# The difference between Date_last_renewal and Date_next_renewal is exactly one year (365 or 366 days),
# We don't need to keep one of them and will only consider Date_last_renewal :
diff_renewal_days <- insurance_data$Date_next_renewal - insurance_data$Date_last_renewal
min(diff_renewal_days)
max(diff_renewal_days)

# Remove Date_next_renewal column
insurance_data <- insurance_data %>%
  select(-Date_next_renewal)

# Extract Current Year from Date_last_renewal
insurance_data <- insurance_data %>%
  mutate(Year = year(Date_last_renewal))

# We add Age and Driving_age columns as they may impact the number of claims and premium,
# We only have Date_birth and Date_driving_licence but they may not be relevant as
# people of same age for example may pay the same price regardless of the current year.
insurance_data <- insurance_data %>%
  mutate(
    Age = as.integer((Year - year(Date_birth))),
    Driving_age = as.integer((Year - year(Date_driving_licence)))
  )

# Remove Date_birth and Date_driving_licence columns
insurance_data <- insurance_data %>%
  select(-Date_birth, -Date_driving_licence)

# Problem with history data :
# For ID 12, 12 claims were raised but the history column shows only 8,
# For ID 18, the R_Claims_history is 0.36 which correspond to 9 claims over a period of 25 years, but not 7.2 over 20 years :
data_problem_sample <- insurance_data %>%
  filter(ID %in% c(12, 18)) %>%
  select(ID, N_claims_history, N_claims_year, R_Claims_history, Date_start_contract, Date_last_renewal, Seniority)
colnames(data_problem_sample) <- gsub("_", " ", colnames(data_problem_sample))
data_problem_sample

# We recalculate the Number of claims history :
# First we calculate the cumulative sum of the claims (N_claims_year) less the claims for the current year (to have the history),
# then we calculate the sum of claims (N_claims_year) per ID,
# we compare this sum with the value of N_claims_history, if N_claims_history is higher we use it as total claims,
# the difference being some claims raised before 2015 and not recorded in N_claims_year,
# then we add the difference D to the cumulative sum to get the history :
insurance_data <- insurance_data %>%
  group_by(ID) %>%
  mutate(N_claims_cumulative = cumsum(N_claims_year) - N_claims_year,
         diff_claims_history = pmax(N_claims_history - sum(N_claims_year), 0),
         N_claims_history_2 = N_claims_cumulative + diff_claims_history) %>%
  ungroup() %>%
  select(-N_claims_history, -N_claims_cumulative, -diff_claims_history) %>%
  rename(N_claims_history = N_claims_history_2)

# For the seniority, we first calculate the contract age as the difference in years between Date_last_renewal and Date_start_contract,
# then we add the difference between Seniority and the max contract age if there is any :
insurance_data <- insurance_data %>%
  group_by(ID) %>%
  mutate(Contract_year = round(as.numeric(difftime(Date_last_renewal, Date_start_contract, units = "days")) / 365.25),
         Max_Contract_Age = 1 + max(Contract_year, na.rm = TRUE),
         diff_seniority = pmax(Seniority - Max_Contract_Age, 0),
         Seniority_2 = Contract_year + diff_seniority) %>%
  ungroup() %>%
  select(-Seniority, -Contract_year, -Max_Contract_Age, -diff_seniority) %>%
  rename(Seniority = Seniority_2)

# We can now calculate the ratio of claims as 0 if Seniority is 0, otherwise N_claims_history / Seniority :
insurance_data <- insurance_data %>%
  mutate(R_Claims_history_2 = ifelse(Seniority == 0, 
                                     0, 
                                     N_claims_history / Seniority)) %>%
  select(-R_Claims_history) %>%
  rename(R_Claims_history = R_Claims_history_2)

# We now create the train and test sets based on the cutoff date.
# the idea is we will use the train set to create an algorithm,
# and then we will try to predict future costs from the cutoff date :

# Define the cutoff date
cutoff_date <- as.Date("2018-09-01")

# Create train_set
train_set <- insurance_data %>%
  filter(Date_last_renewal < cutoff_date)
nrow(train_set)

# and test_set
test_set <- insurance_data %>%
  filter(Date_last_renewal >= cutoff_date)
nrow(test_set)

#########################################################
#         5. Exploratory Data Analysis                  #
#########################################################

#########################################################
#         5.1.1 Timeline                                #
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
insurance_year <- train_set %>%
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
  ungroup() %>%
  mutate(Year = as.character(Year))

# Replace underscores with spaces in headers
colnames(insurance_year) <- gsub("_", " ", colnames(insurance_year))
insurance_year

# Rename columns to add "\n" :
names(insurance_year)[names(insurance_year) == "Policies with claims"] <- "Policies\nwith claims"

# Transform data to longer format
insurance_long <- insurance_year %>%
  pivot_longer(
    cols = c("Policies", "Policies\nwith claims",
             "Total claims", "Total Cost"),
    names_to = "Measure",
    values_to = "Value"
  )

# Define a custom color palette
custom_colors_year <- c("Policies" = "darkgreen",
                   "Policies\nwith claims" = "darkred",
                   "Total claims" = "gray",
                   "Total Cost" = "gray")

# Plot the number of policies (total and with claims) :
timeline_policies_claims <- insurance_long %>%
  filter(Measure %in% c("Policies", "Policies\nwith claims", "Total claims")) %>%
  ggplot(aes(x = Year, y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year",
       y = "Number of Policies / Claims") +
  scale_fill_manual(values = custom_colors_year) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top") +
  guides(fill = guide_legend(title.position = "top", label.theme = element_text(size = 8)))

# Plot the total cost :
timeline_cost <- insurance_long %>%
  filter(Measure %in% c("Total Cost")) %>%
  ggplot(aes(x = Year, y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year",
       y = "Cost") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  scale_fill_manual(values = custom_colors_year) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top") +
  guides(fill = guide_legend(title.position = "top", label.theme = element_text(size = 8)))

# Plot the 2 graphs :
plot_grid(
  timeline_policies_claims,
  timeline_cost,
  ncol = 2, align = 'hv'
)

# We consider a policy is new if the Date_last_renewal and the Date_start_contract are in the same year,
# and it will be terminating if Date_last_renewal and Date_lapse are in the same year.
# We display the number of policies per year, number of new and terminating policies,
# and the proportion of policies / new policies / terminating policies with a claim in the current year :
insurance_mv <- train_set %>%
  mutate(Year = year(Date_last_renewal),
         Year_Start = year(Date_start_contract),
         Year_End = year(Date_lapse)) %>%
  select(ID, N_claims_year, Year, Year_Start, Year_End) %>%
  group_by(Year) %>%
  summarise(
    Total_Policies = n_distinct(ID),
    New_Policies = sum(Year == Year_Start),
    Terminating_Policies = sum(Year == Year_End, na.rm = TRUE),
    Proportion_All_With_Claims = 100 * round(sum(N_claims_year > 0) / Total_Policies * ifelse(Total_Policies > 0, 1, 0), 3),
    Proportion_New_With_Claims = 100 * round(sum(N_claims_year[Year == Year_Start] > 0) / New_Policies * ifelse(New_Policies > 0, 1, 0), 3),
    Proportion_Terminating_With_Claims = 100 * round(sum(N_claims_year[Year == Year_End] > 0, na.rm = TRUE) / Terminating_Policies * ifelse(Terminating_Policies > 0, 1, 0), 3)
  ) %>%
  mutate(Year = as.character(Year))

# Replace underscores with spaces in headers
colnames(insurance_mv) <- gsub("_", " ", colnames(insurance_mv))
insurance_mv

# Rename columns to add "\n" :
names(insurance_mv)[names(insurance_mv) == "Total Policies"] <- "Total\nPolicies"
names(insurance_mv)[names(insurance_mv) == "New Policies"] <- "New\nPolicies"
names(insurance_mv)[names(insurance_mv) == "Terminating Policies"] <- "Terminating\nPolicies"
names(insurance_mv)[names(insurance_mv) == "Proportion All With Claims"] <- "% All\nWith Claims"
names(insurance_mv)[names(insurance_mv) == "Proportion New With Claims"] <- "% New\nWith Claims"
names(insurance_mv)[names(insurance_mv) == "Proportion Terminating With Claims"] <- "% Terminating\nWith Claims"

# Reshape the data from wide to long format
long_data <- insurance_mv %>%
  pivot_longer(
    cols = c("Total\nPolicies", "New\nPolicies", "Terminating\nPolicies",
             "% All\nWith Claims", "% New\nWith Claims", "% Terminating\nWith Claims"),
    names_to = "Measure",
    values_to = "Value"
  )
long_data

# Create an ordered factor for the Measure
long_data$Measure <- factor(long_data$Measure, 
                            levels = c("New\nPolicies", "Terminating\nPolicies", "Total\nPolicies",
                                       "% New\nWith Claims", "% Terminating\nWith Claims", "% All\nWith Claims"))

# Define a custom color palette
custom_colors <- c("New\nPolicies" = "darkgreen",
                   "Terminating\nPolicies" = "darkred",
                   "Total\nPolicies" = "gray",
                   "% New\nWith Claims" = "darkgreen",
                   "% Terminating\nWith Claims" = "darkred",
                   "% All\nWith Claims" = "gray")

# Plot the number of new/terminating and total policies :
timeline_nb_policies <- long_data %>%
  filter(Measure %in% c("New\nPolicies", "Terminating\nPolicies", "Total\nPolicies")) %>%
  ggplot(aes(x = Year, y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year",
       y = "Number of Policies") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top") +
  guides(fill = guide_legend(title.position = "top", label.theme = element_text(size = 8), nrow=2, byrow=TRUE))

# Plot the number of new/terminating and total policies :
timeline_r_policies_claim <- long_data %>%
  filter(Measure %in% c("% New\nWith Claims", "% Terminating\nWith Claims", "% All\nWith Claims")) %>%
  ggplot(aes(x = Year, y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year",
       y = "% of Policies with Claims") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top") +
  guides(fill = guide_legend(title.position = "top", label.theme = element_text(size = 8), nrow=2, byrow=TRUE))

# Plot the 2 graphs :
plot_grid(
  timeline_nb_policies,
  timeline_r_policies_claim,
  ncol = 2, align = 'hv'
)

#########################################################
#         5.1.2 Vehicle                                 #
#########################################################

# Summary of the car details :
car_summary_1 <- train_set %>%
  select("N_doors", "Power", "Cylinder_capacity", "Type_fuel", "Length", "Weight") %>%
  summary()

car_summary_2 <- train_set %>%
  select("Year_matriculation", "Value_vehicle") %>%
  summary()

car_summary_1
car_summary_2

# Distribution of Power :
distribution_power <- ggplot(train_set, aes(x = Power)) +
  geom_histogram(binwidth = 20, fill = "darkred", alpha = 0.8) +
  labs(x = "Power",
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Distribution of Value vehicle :
distribution_value <- ggplot(train_set, aes(x = Value_vehicle)) +
  geom_histogram(binwidth = 3000, fill = "darkred", alpha = 0.8) +
  labs(x = "Value",
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Plot the 2 graphs :
plot_grid(
  distribution_power,
  distribution_value,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

#########################################################
#         5.1.3 Policy Holder                           #
#########################################################
train_set
# Summary of the driver details
driver_summary <- train_set %>%
  select("Age", "Driving_age", "Seniority", "Area", "Second_driver") %>%
  summary()
driver_summary

# Distribution of ages :
distribution_age <- ggplot(train_set, aes(x = Age)) +
  geom_bar(fill="darkblue", alpha = 0.8) +
  labs(x = "Age",
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Distribution of licence age (age less driving age) :
distribution_licence_age <- ggplot(train_set, aes(x = Age - Driving_age)) +
  geom_histogram(binwidth = 1, fill = "darkblue", alpha = 0.8) +
  labs(x = "Driving Licence (age)",
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Plot the 2 graphs :
plot_grid(
  distribution_age,
  distribution_licence_age,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

#########################################################
#         5.1.4 Policy Details                          #
#########################################################

# Summary of the policy details
policy_summary_1 <- train_set %>%
  select("ID", "Date_start_contract", "Date_last_renewal", "Distribution_channel") %>%
  summary()

policy_summary_2 <- train_set %>%
  select("Policies_in_force", "Max_policies", "Max_products", "Lapse", "Date_lapse") %>%
  summary()

policy_summary_3 <- train_set %>%
  select("Payment", "Premium", "Type_risk") %>%
  summary()

policy_summary_4 <- train_set %>%
  select("Cost_claims_year", "N_claims_year", "N_claims_history", "R_Claims_history") %>%
  summary()

policy_summary_1
policy_summary_2
policy_summary_3
policy_summary_4

# Distribution of the Seniority :
distribution_seniority <- ggplot(train_set, aes(x = Seniority)) +
  geom_histogram(binwidth = 1, fill = "darkblue", alpha = 0.8) +
  labs(x = "Seniority",
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 9))
distribution_seniority


# Distribution of the Nb of Policies :
distribution_nb_policies <- ggplot(train_set, aes(x = Policies_in_force)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", alpha = 0.8) +
  labs(x = "Nb Policies",
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 9))
distribution_nb_policies

# Distribution of the Type Risk :
distribution_type_risk <- ggplot(train_set, 
                                 aes(x = factor(Type_risk, 
                                                levels = c(1, 2, 3, 4), 
                                                labels = c("Motorbike", "Van", "Car", "Tractor")))) +
  geom_bar(fill = "darkgreen", alpha = 0.8) +
  labs(x = "Type Risk",
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 9))
distribution_type_risk

# Distribution of the Premium values :
distribution_premium <- ggplot(train_set, aes(x = Premium)) +
  geom_histogram(binwidth = 10, fill = "darkgreen", alpha = 0.8) +
  labs(x = "Premium",
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 9))
distribution_premium

# Distribution of the Number of Claims :
distribution_claims <- ggplot(train_set, aes(x = N_claims_year)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", alpha = 0.8) +
  labs(x = "Nb Claims",
       y = "Frequency") +
#  scale_y_log10() +
  theme_minimal() +
  theme(text = element_text(size = 9))
distribution_claims

# Distribution of the Cost :
distribution_cost <- ggplot(train_set, aes(x = Cost_claims_year)) +
  geom_histogram(binwidth = 10, fill = "darkgreen", alpha = 0.8) +
  labs(x = "Cost",
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 9))
distribution_cost

# Plot 4 graphs :
plot_grid(
  distribution_nb_policies,
  distribution_type_risk,
  distribution_premium,
  distribution_claims,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

#########################################################
#         5.2 Correlations                              #
#########################################################

# We have 30 variables, we need to remove some of them

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
date_columns <- c("Date_start_contract", "Date_last_renewal", "Date_lapse")

# Convert each date column to numeric
for (col in date_columns) {
  tmp_data[[col]] <- as.numeric(tmp_data[[col]])
}

# As Date_lapse is numeric
tmp_data$Date_lapse[is.na(tmp_data$Date_lapse)] <- 0

# We remove :
# Length (highly correlated to Weight; cor = 0.83),
# ID (not used).
tmp_data <- tmp_data %>%
  select(-Length, -ID)
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

# Display the correlation matrix for the vehicles :
ggcorrplot(correlation_matrix_car,
           lab_size = 3,
           ggtheme = theme_dark(base_size = 9),
           colors = c("darkgreen", "white", "darkred")) +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "top"
  )

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
           ggtheme = theme_dark(base_size = 9),
           colors = c("darkgreen", "white", "darkred")) +
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
           ggtheme = theme_dark(base_size = 9),
           colors = c("darkgreen", "white", "darkred")) +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "top"
  )

#########################################################
#         5.2 Correlation Plots                         #
#########################################################

# As we have many data, we only keep 400 observations to display :
set.seed(123)
sampled_data <- insurance_data[sample(nrow(insurance_data), 400), ]

# Correlation plot of the Value vehicle vs Power :
corplot_power <- ggplot(sampled_data, aes(x = Power, y = Value_vehicle)) +
  geom_point(alpha = 0.5) +
  labs(x = "Power",
       y = "Value") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Correlation plot of the Premium vs Value vehicle :
corplot_premium <- ggplot(sampled_data, aes(x = Value_vehicle, y = Premium)) +
  geom_point(alpha = 0.5) +
  labs(x = "Value",
       y = "Premium") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Correlation plot of the Premium vs Year matriculation :
corplot_year_matriculation <- ggplot(sampled_data, aes(x = Year_matriculation, y = Premium)) +
  geom_point(alpha = 0.5) +
  labs(x = "Year Matriculation",
       y = "Premium") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Correlation plot of the Driving age vs Age :
corplot_driving_age <- ggplot(sampled_data, aes(x = Age, y = Driving_age)) +
  geom_point(alpha = 0.5) +
  labs(x = "Age",
       y = "Driving age") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Correlation plot of the Premium vs Age :
corplot_age <- ggplot(sampled_data, aes(x = Age, y = Premium)) +
  geom_point(alpha = 0.5) +
  labs(x = "Age",
       y = "Premium") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Plot 4 graphs :
plot_grid(
  corplot_power,
  corplot_premium,
  #  corplot_year_matriculation,
  corplot_driving_age,
  corplot_age,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

#########################################################
#         5.3.1 Cost - Timeline                         #
#########################################################

# Summarize total costs by month and year
monthly_costs <- insurance_data %>%
  mutate(Year_Month = floor_date(Date_last_renewal, "month")) %>%
  group_by(Year_Month) %>%
  summarize(Total_Policies = n(),
            Total_Claims = sum(N_claims_year, na.rm = TRUE),
            Total_Cost = sum(Cost_claims_year, na.rm = TRUE),
            .groups = "drop")

# Create the policies plot
monthly_total_policies <- ggplot(monthly_costs, aes(x = Year_Month, y = Total_Policies)) +
  geom_point(color = "darkblue") +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Month", y = "Total Policies") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Create the cost plot
monthly_total_cost <- ggplot(monthly_costs, aes(x = Year_Month, y = Total_Cost)) +
  geom_point(color = "darkblue") +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Month", y = "Total Cost") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Create the claims plot
monthly_total_claims <- ggplot(monthly_costs, aes(x = Year_Month, y = Total_Claims)) +
  geom_point(color = "darkblue") +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Month", y = "Total Claims") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Create the cost per policy plot
monthly_cost_per_policy <- ggplot(monthly_costs, aes(x = Year_Month, y = Total_Cost / Total_Policies)) +
  geom_point(color = "darkblue") +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Month", y = "Cost per Policy") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Create the cost per claim plot
monthly_cost_per_claim <- ggplot(monthly_costs, aes(x = Year_Month, y = Total_Cost / Total_Claims)) +
  geom_point(color = "darkblue") +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Month", y = "Cost per Claim") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Timeline totals
plot_grid(
  monthly_total_policies,
  monthly_total_cost,
  monthly_total_claims,
  ncol = 3, align = 'hv', rel_heights = c(2, 2, 2)
)

# Timeline averages
plot_grid(
  monthly_cost_per_policy,
  monthly_cost_per_claim,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

#########################################################
#         5.3.2 Cost - Vehicle                          #
#########################################################

# Cost Policy Ratio compared to Power, Value_vehicle, Age and Driving_age
cost_policy_ratio <- function(x_var) {
  train_set %>%
    mutate(Power = floor(train_set$Power / 10) * 10,
           Value_vehicle = floor(train_set$Value_vehicle / 1000) * 1000,) %>%
    select(ID, Year, !!sym(x_var), N_claims_year, Cost_claims_year) %>%
    group_by(Year, !!sym(x_var)) %>%
    summarize(Total_cost = sum(Cost_claims_year, na.rm = TRUE),
              #              Nb_claims = sum(N_claims_year, na.rm = TRUE),
              Nb_policies = n(),
              .groups = 'drop') %>%
    mutate(Year = as.factor(Year),
           Cost_Policy_Ratio = Total_cost / Nb_policies) %>%
    ggplot(aes_string(x = x_var, y = "Cost_Policy_Ratio", color = "Year")) +
    geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
    labs(x = x_var, y = "Cost Policy Ratio") +
    ylim(0, NA) +
    theme_minimal() +
    theme(text = element_text(size = 8), legend.position = "top")
}

# Number of Policies compared to Power, Value_vehicle, Age and Driving_age
nb_policies <- function(x_var) {
  train_set %>%
    mutate(Power = floor(train_set$Power / 10) * 10,
           Value_vehicle = floor(train_set$Value_vehicle / 1000) * 1000,) %>%
    select(ID, Year, !!sym(x_var), N_claims_year) %>%
    group_by(Year, !!sym(x_var)) %>%
    summarize(Nb_policies = n(),
              .groups = 'drop') %>%
    mutate(Year = as.factor(Year)) %>%
    ggplot(aes_string(x = x_var, y = "Nb_policies", color = "Year")) +
    geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
    labs(x = x_var, y = "Nb Policies") +
    ylim(0, NA) +
    theme_minimal() +
    theme(text = element_text(size = 8), legend.position = "top")
}

plot_grid(
  nb_policies("Power"),
  nb_policies("Value_vehicle"),
  cost_policy_ratio("Power"),
  cost_policy_ratio("Value_vehicle"),
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

#########################################################
#         5.3.2 Cost - Vehicle (Category)               #
#########################################################

# Claim Policy Ratio compared to Type_risk
type_risk_summary <- insurance_data %>%
  select(ID, Year, Type_risk, N_claims_year, Cost_claims_year) %>%
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

# Define a custom color palette
custom_colors_type_risk <- c("Agricultural vehicles" = "darkred",
                             "Motorbikes" = "darkgreen",
                             "Passenger cars" = "darkblue",
                             "Vans" = "gray")

# Claim Policy Ratio compared to Type Risk
type_risk_claim_policy_ratio <-   type_risk_summary %>%
  ggplot(aes(x = Year, y = Claim_Policy_Ratio, fill = Type_risk)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Claim Policy Ratio") +
  scale_fill_manual(values = custom_colors_type_risk) +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top") +
  guides(fill = guide_legend(label.theme = element_text(size = 8), nrow=2, byrow=TRUE))

# Claim Policy Ratio compared to Type Risk
type_risk_cost_policy_ratio <-   type_risk_summary %>%
  ggplot(aes(x = Year, y = Cost_Policy_Ratio, fill = Type_risk)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Cost Policy Ratio") +
  scale_fill_manual(values = custom_colors_type_risk) +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top") +
  guides(fill = guide_legend(label.theme = element_text(size = 8), nrow=2, byrow=TRUE))

# Plot the 2 graphs :
plot_grid(
  type_risk_claim_policy_ratio,
  type_risk_cost_policy_ratio,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

#########################################################
#         5.3.3 Cost - Policy Holder                    #
#########################################################

# Claims
claims_data <- train_set %>%
  group_by(Year, ID) %>%
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
  group_by(Year, total_claims) %>%
  summarise(Count_claims = n(), .groups = 'drop') %>%
  complete(Year, total_claims = 0:max_claims, fill = list(Count_claims = 0)) %>%
  rename(Nb_claims = total_claims) %>%
  # Calculate cumulative counts
  group_by(Year) %>%
  mutate(Cumulative_claims = rev(cumsum(rev(Count_claims))),
#         percentage = Cumulative_claims / lag(Cumulative_claims),
#         percentage = ifelse(is.na(percentage), 0, percentage * 100),
         percentage = 100 * lead(Cumulative_claims) / Cumulative_claims
  ) %>%
  ungroup() %>%
  filter(!is.na(percentage))

# Display the resulting summary
claims_summary

# Risk of Raising a New Claim based on number of claims raised during the year
risk_current_year <- claims_summary %>% filter(Cumulative_claims > 10) %>%
  ggplot(aes(x = Nb_claims, y = percentage)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Number of Previous Claims (same year)", y = "Risk (%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 20, by = 2)) +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Risk of Raising a Claim the next year based on number of claims raised during the current year
risk_tmp <- train_set %>% select(ID, N_claims_year, Cost_claims_year, Year) %>%
  mutate(Next_Year = Year + 1)
risk_df <- expand_grid(Year = unique(risk_tmp$Year), N_claims_year = unique(risk_tmp$N_claims_year)) %>%
  rowwise() %>% 
  mutate(
    Current_Count = sum(risk_tmp$Year == Year & risk_tmp$N_claims_year == N_claims_year),
    Next_Count = sum(risk_tmp$Year == Year + 1 & risk_tmp$N_claims_year > 0 & 
                       risk_tmp$ID %in% risk_tmp$ID[risk_tmp$Year == Year & risk_tmp$N_claims_year == N_claims_year]),
    Ratio = 100 * Next_Count / Current_Count
  ) %>%
  ungroup() %>%
  arrange(Year, N_claims_year)

# We don't display data for 2018 as there is nothing in 2019,
# we also don't display data for N_claims_year over 20 as we don't have many observations
risk_next_year <- risk_df %>%
  filter(Current_Count >= 10 & Year < 2018) %>%
  ggplot(aes_string(x = "N_claims_year", y = "Ratio")) +
  geom_point() +
  geom_smooth(method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Number of Claims from Previous Year", y = "Risk(%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) + 
  scale_x_continuous(breaks = seq(0, 20, by = 2)) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")

# Plot the 2 graphs :
plot_grid(
  risk_current_year,
  risk_next_year,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

# Risk of raising at least one claim for new policy holder :
risk_new_ph <- train_set %>%
  filter(Year == year(Date_start_contract)) %>%
  select(ID, Year, N_claims_year) %>%
  group_by(Year) %>%
  summarise(risk = 100 * sum(N_claims_year > 0) / n())

ggplot(risk_new_ph, aes(x = factor(Year), y = risk)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Year", y = "Risk (%)") +
  theme_minimal()

# Risk of Raising a New Claim based on number of claims raised during the year :
# Comparison between years
#claims_summary %>% filter(Nb_claims > 0 & Cumulative_claims > 10) %>%
#  mutate(Year = as.factor(Year)) %>%
#  ggplot(aes(x = Nb_claims, y = percentage, color = Year)) +
#  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
#  labs(x = "Number of Previous Claims",
#       y = "Risk (%)") +
#  theme_minimal() +
#  theme(text = element_text(size = 9))

# Risk of Raising a New Claim based on history (total claims raised)
# We only display data for 2017 (last full year)
risk_summary <- train_set %>%
  group_by(Year) %>%
  select(Year, ID, N_claims_year, Cost_claims_year, N_claims_history, R_Claims_history)

# Risk vs N_claims_history
risk_N_claims_history <- risk_summary %>%
  group_by(N_claims_history) %>%
  summarise(count = sum(N_claims_year > 0),
            total = n(),
            risk = 100 * count / total) %>%
  filter(total > 5) %>%
  ggplot(aes(x = N_claims_history, y = risk)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1, formula = y ~ x) +
  labs(x = "N_claims_history", y = "Risk(%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) + 
  #  scale_x_continuous(breaks = seq(0, 20, by = 2)) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")

# Risk vs R_Claims_history
risk_R_Claims_history <- risk_summary %>%
  group_by(R_Claims_history) %>%
  summarise(count = sum(N_claims_year > 0),
            total = n(),
            risk = 100 * count / total) %>%
  filter(total > 10 & R_Claims_history < 10) %>%
  ggplot(aes(x = R_Claims_history, y = risk)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1, formula = y ~ x) +
  labs(x = "R_Claims_history", y = "Risk(%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) + 
  #  scale_x_continuous(breaks = seq(0, 20, by = 2)) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")

# Plot the 2 graphs :
plot_grid(
  risk_N_claims_history,
  risk_R_Claims_history,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

# Calculate average N_claims_year by N_claims_history
avg_claims_N_hist <- risk_summary %>%
  group_by(N_claims_history) %>%
  summarise(Average_N_claims_year = mean(N_claims_year, na.rm = TRUE)) %>%
  ggplot(aes(x = N_claims_history, y = Average_N_claims_year)) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.7) +
  labs(x = "N_claims_history",
       y = "Avg N_claims_year") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Calculate average N_claims_year by R_Claims_history
avg_claims_R_hist <- risk_summary %>%
  mutate(R_Claims_history = round(R_Claims_history, digits = 1)) %>%
  filter(R_Claims_history < 10) %>%
  group_by(R_Claims_history) %>%
  summarise(Average_N_claims_year = mean(N_claims_year, na.rm = TRUE)) %>%
  ggplot(aes(x = R_Claims_history, y = Average_N_claims_year)) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.7) +
  labs(x = "R_Claims_history",
       y = "Avg N_claims_year") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Calculate average Cost_claims_year by N_claims_history
avg_cost_N_hist <- risk_summary %>%
  group_by(N_claims_history) %>%
  summarise(Average_Cost_claims_year = mean(Cost_claims_year, na.rm = TRUE)) %>%
  ggplot(aes(x = N_claims_history, y = Average_Cost_claims_year)) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.7) +
  labs(x = "N_claims_history",
       y = "Avg Cost_claims_year") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Calculate average Cost_claims_year by R_Claims_history
avg_cost_R_hist <- risk_summary %>%
  mutate(R_Claims_history = round(R_Claims_history, digits = 1)) %>%
  filter(R_Claims_history < 10) %>%
  group_by(R_Claims_history) %>%
  summarise(Average_Cost_claims_year = mean(Cost_claims_year, na.rm = TRUE)) %>%
  ggplot(aes(x = R_Claims_history, y = Average_Cost_claims_year)) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.7) +
  labs(x = "R_Claims_history",
       y = "Avg Cost_claims_year") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Plot the 4 different graphs :
plot_grid(
  avg_claims_N_hist,
  avg_claims_R_hist,
  avg_cost_N_hist,
  avg_cost_R_hist,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

#########################################################
#         5.3.3.3 Cost - History                        #
#########################################################

# Calculate counts, percentages, and average cost
summary_cost <- train_set %>%
  select(ID, Year, Age, Date_start_contract, N_claims_year, Cost_claims_year) %>%
  mutate(status = if_else(year(Date_start_contract) == Year, "New", "Ongoing"),
         age_group = if_else(Age < 25, "Young", "Old")) %>%
  group_by(Year, status, age_group) %>%
  summarise(
    count = n(),
    total_cost = sum(Cost_claims_year, na.rm = TRUE),
    average_cost = mean(Cost_claims_year, na.rm = TRUE),
    .groups = 'drop'
  )

# Summary across all years
summary_cost_all_years <- summary_cost %>%
  group_by(status) %>%
  summarise(
    count = sum(count),
    total_cost = sum(total_cost),
    average_cost = round(total_cost / count, 2),
  ) %>%
  ungroup()

# Define a custom color palette
custom_colors_status <- c("New" = "darkgreen",
                          "Ongoing" = "gray",
                          "New-Old" = "gray",
                          "New-Young" = "darkgreen",
                          "Ongoing-Old" = "darkblue",
                          "Ongoing-Young" = "darkred")

# Plot the number of policies (new and ongoing) :
cost_nb_policies <- summary_cost %>%
  ggplot(aes(x = Year, y = count, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year",
       y = "Policies") +
  scale_fill_manual(values = custom_colors_status) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")

# Plot the total cost value (new and ongoing) :
cost_total_value <- summary_cost %>%
  ggplot(aes(x = Year, y = total_cost, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year",
       y = "Total Cost") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  scale_fill_manual(values = custom_colors_status) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")

# Plot the average cost value (new and ongoing) :
cost_avg_value <- summary_cost %>%
  ggplot(aes(x = Year, y = average_cost, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year",
       y = "Average Cost") +
  scale_fill_manual(values = custom_colors_status) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")

# Plot the 3 graphs :
plot_grid(
  cost_nb_policies,
  cost_total_value,
#  cost_avg_value,
  ncol = 2, align = 'hv'
)

# Average cost per age and status group :
summary_cost_status <- summary_cost %>%
  mutate(status = paste(status, age_group, sep = "-")) %>%
  select(-age_group)
cost_avg_status <- summary_cost_status %>%
  ggplot(aes(x = Year, y = average_cost, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year",
       y = "Average Cost") +
  scale_fill_manual(values = custom_colors_status) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")
cost_avg_status

#########################################################
#         5.3.4 Cost - Policy                           #
#########################################################

# Nb Policy function :
nb_policy_binary <- function(data, var, labels) {
  data %>%
    select(ID, Year, !!sym(var), N_claims_year, Cost_claims_year) %>%
    group_by(Year, !!sym(var)) %>%
    summarize(Nb_policy_claims = sum(N_claims_year > 0, na.rm = TRUE),
              Total_cost = sum(Cost_claims_year, na.rm = TRUE),
              Nb_policies = n(),
              .groups = 'drop') %>%
    mutate(Year = as.factor(Year),
           Claim_Policy_Ratio = Nb_policy_claims / Nb_policies,
           Cost_Policy_Ratio = Total_cost / Nb_policies,
           !!sym(var) := case_when(
             !!sym(var) == 0 ~ labels[1],
             !!sym(var) == 1 ~ labels[2]
           ))
}
nb_policy_binary(train_set, "Area", c("Rural", "Urban"))

# Claim Policy Ratio function :
claim_policy_ratio_binary <- function(data, var, labels) {
  # Define a custom color palette
  custom_colors_label <- c("darkgreen", "gray")
  
  nb_policy_binary(data, var, labels) %>%
    ggplot(aes(x = Year, y = Claim_Policy_Ratio, fill = !!sym(var))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Year", y = "Claim Policy Ratio") +
    scale_fill_manual(values = custom_colors_label) +
    ylim(0, NA) +
    theme_minimal() +
    theme(text = element_text(size = 9), legend.position = "top")
}

# Cost Policy Ratio function :
cost_policy_ratio_binary <- function(data, var, labels) {
  # Define a custom color palette
  custom_colors_label <- c("darkgreen", "gray")
  
  nb_policy_binary(data, var, labels) %>%
    ggplot(aes(x = Year, y = Cost_Policy_Ratio, fill = !!sym(var))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Year", y = "Cost Policy Ratio") +
    scale_fill_manual(values = custom_colors_label) +
    ylim(0, NA) +
    theme_minimal() +
    theme(text = element_text(size = 9), legend.position = "top")
}

# Claim and Cost Policy Ratio compared to Area :
claim_policy_ratio_area <- claim_policy_ratio_binary(train_set, "Area", c("Rural", "Urban"))
cost_policy_ratio_area <- cost_policy_ratio_binary(train_set, "Area", c("Rural", "Urban"))

# Plot the 2 graphs :
plot_grid(
  claim_policy_ratio_area,
  cost_policy_ratio_area,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

# Cost Policy Ratio compared to Distribution channel and payment :
cost_policy_ratio_channel <- cost_policy_ratio_binary(train_set, "Distribution_channel", c("Agent", "Broker"))
cost_policy_ratio_payment <- cost_policy_ratio_binary(train_set, "Payment", c("Half yearly", "Annual"))

# Cost Policy Ratio compared to Distribution channel and payment :
nb_policy_binary(train_set, "Distribution_channel", c("Agent", "Broker"))
nb_policy_binary(train_set, "Payment", c("Half yearly", "Annual"))

# Plot the 2 graphs :
plot_grid(
  cost_policy_ratio_channel,
  cost_policy_ratio_payment,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

#########################################################
#         6.1 Methodology                               #
#########################################################

# RMSE function
RMSE <- function(costs, predictions) {
  sqrt(mean((predictions - costs)^2))
}

# Error function : ratio between difference predictions costs and costs
get_error <- function(total_costs, total_predictions) {
  (total_predictions - total_costs) / total_costs
}

# Accuracy function : proportion of differences predictions costs less than 10
get_accuracy <- function(costs, predictions) {
  mean(abs(costs - predictions) < 10)
}

# Define the new cutoff date
cutoff_subset_date <- as.Date("2018-06-01")

# Create train subset from train_set
# Add Status column (New or Ongoing),
# Claim to indicate 1 if there is a claim or more, 0 otherwise, 
# and remove unneeded columns :
# Premium and N_claims_year as they contain values that are related to Cost_claims_year
# and all columns containing NA values
train_subset <- train_set %>%
  filter(Date_last_renewal < cutoff_subset_date) %>%
  mutate(Status = if_else(year(Date_start_contract) == Year, 1, 0),
         Claim = if_else(Cost_claims_year > 0, 1, 0)) %>%
  select(where(~ !any(is.na(.)))) %>%
  select(-Premium, -N_claims_year, -Year)
nrow(train_subset)

# Create a validation subset from train_set the same way :
valid_subset <- train_set %>%
  filter(Date_last_renewal >= cutoff_subset_date) %>%
  mutate(Status = if_else(year(Date_start_contract) == Year, 1, 0),
         Claim = if_else(Cost_claims_year > 0, 1, 0)) %>%
  select(where(~ !any(is.na(.)))) %>%
  select(-Premium, -N_claims_year, -Year)
nrow(valid_subset)

# And add/remove columns on the test set the same way :
test_set <- test_set %>%
  mutate(Status = if_else(year(Date_start_contract) == Year, 1, 0),
         Claim = if_else(Cost_claims_year > 0, 1, 0)) %>%
  select(where(~ !any(is.na(.)))) %>%
  select(-Premium, -N_claims_year, -Year)

# Initialise the results table :
results <- tibble(Method = character(),
                  RMSE = numeric(),
                  Cost = numeric(),
                  Estimation = numeric(),
                  Error = numeric(),
                  Accuracy = numeric())

# Result function :
# Add metrics from prediction values to the results table :
update_results <- function(validation_set, predictions, title) {
  rmse <- RMSE(validation_set$Cost_claims_year, predictions)
  total_costs <- sum(validation_set$Cost_claims_year)
  total_predictions <- sum(predictions)
  rel_error <- get_error(total_costs, total_predictions)
  accuracy <- get_accuracy(validation_set$Cost_claims_year, predictions)
  
  # Update the results table :
  results %>% add_row(Method = title,
                                 RMSE = round(rmse, 2),
                                 Cost = round(total_costs, 2),
                                 Estimation = round(total_predictions, 2),
                                 Error = round(rel_error, 3),
                                 Accuracy = round(accuracy, 3))
}

str(train_subset)

#########################################################
#         6.2 Averages                                  #
#########################################################

# Average for all values of all dates within the train_subset :
avg_cost <- mean(train_subset$Cost_claims_year)

# Predict the values (use the average) :
y_hat <- rep(avg_cost, nrow(valid_subset))

# Update the results table :
results <- update_results(valid_subset, y_hat, "Average (Baseline)")
results

# Average for all values of last 3 months of the train_subset :
avg_cost_3m <- train_subset %>%
  filter(Date_last_renewal >= as.Date(cutoff_subset_date) - months(3)) %>%
  summarise(avg = mean(Cost_claims_year)) %>%
  pull(avg)

# Predict the values (use the average) :
y_hat <- rep(avg_cost_3m, nrow(valid_subset))

# Update the results table :
results <- update_results(valid_subset, y_hat, "Average (last 3 months)")
results

#########################################################
#         6.3 Zero                                      #
#########################################################

zero <- 0

# Number of 0 cost observations in the train set :
mean(train_subset$Cost_claims_year == 0)

# Predict the values (use zero) :
y_hat <- rep(zero, nrow(valid_subset))

# Update the results table :
results <- update_results(valid_subset, y_hat, "Zero")
results

y_hat <- rep(9.99, nrow(valid_subset))

# Update the results table :
results <- update_results(valid_subset, y_hat, "9.99")
results

#########################################################
#         6.4 Linear Models                             #
#########################################################

# Linear Model with 1 parameter :
# RMSE = 469.1506
# Error = 0.651
train <- train(Cost_claims_year ~ Date_last_renewal, method = "lm", data = train_subset)

# Predict the values :
y_hat <- predict(train, valid_subset, type = "raw")

# Update the results table :
results <- update_results(valid_subset, y_hat, "Linear Model (1 Variable)")
results

# Update the valid_subset with the predictions :
valid_subset <- valid_subset %>%
  mutate(lm_1 = y_hat)

# Linear Model with 3 parameters :
# RMSE = 474.2981
# Error = 0.562
train <- train(Cost_claims_year ~ Date_last_renewal +
                    R_Claims_history +
                    Second_driver, method = "lm", data = train_subset)

# Predict the values :
y_hat <- predict(train, valid_subset, type = "raw")

# Update the results table :
results <- update_results(valid_subset, y_hat, "Linear Model (3 Variables)")
results

# Update the valid_subset with the predictions :
valid_subset <- valid_subset %>%
  mutate(lm_3 = y_hat)

# Linear Model with 14 parameters :
# RMSE = 474.2981
# Error = 0.390
train <- train(Cost_claims_year ~ Date_last_renewal +
                      Cylinder_capacity +
                      Weight +
                      N_doors +
                      Type_risk +
                      R_Claims_history +
                      Area +
                      Max_products +
                      Second_driver +
                      Age +
                      Value_vehicle +
                      Payment +
                      Year_matriculation +
                      Status, method = "lm", data = train_subset)

# Predict the values :
y_hat <- predict(train, valid_subset, type = "raw")

# Update the results table :
results <- update_results(valid_subset, y_hat, "Linear Model (14 Variables)")
results

# Update the valid_subset with the predictions :
valid_subset <- valid_subset %>%
  mutate(lm_14 = y_hat)

#########################################################
#         6.4 Polynomial Regression                     #
#########################################################

# Linear Model with 1 parameter and polynomial regression :
# RMSE = 474.2981
# Error = 0.562
train <- train(Cost_claims_year ~ poly(Date_last_renewal, 4), method = "lm", data = train_subset)

# Predict the values :
y_hat <- predict(train, valid_subset, type = "raw")

# Update the results table :
results <- update_results(valid_subset, y_hat, "Polynomial Regression")
results

# Update the valid_subset with the predictions :
valid_subset <- valid_subset %>%
  mutate(lm_pr = y_hat)

#########################################################
#         6.4 Plots                                     #
#########################################################

# Calculate weekly summary data :
weekly_summary <- valid_subset %>%
  select(Date_last_renewal, Cost_claims_year, lm_1, lm_3, lm_14, lm_pr) %>%
  mutate(Week = floor_date(Date_last_renewal, unit = "week")) %>%
  group_by(Week) %>%
  summarize(
    Total_Cost = sum(Cost_claims_year, na.rm = TRUE),
    Total_lm_1 = sum(lm_1, na.rm = TRUE),
    Total_lm_3 = sum(lm_3, na.rm = TRUE),
    Total_lm_14 = sum(lm_14, na.rm = TRUE),
    Total_lm_pr = sum(lm_pr, na.rm = TRUE)
  )

# Plot the weekly summary :
weekly_summary_plot <- weekly_summary %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = Total_Cost, color = "Cost"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Total_lm_1, color = "Linear Model 1"), size = 1) +
#  geom_line(aes(y = Total_lm_3, color = "Linear Model 3"), size = 1) +
  geom_line(aes(y = Total_lm_14, color = "Linear Model 14"), size = 1) +
  geom_line(aes(y = Total_lm_pr, color = "Polynomial Regression"), size = 1) +
  labs(x = "Week", y = "Values") +
  scale_color_manual(name = "Legend", 
                     values = c("Cost" = "darkgrey",
                                "Linear Model 1" = "darkblue",
#                                "Linear Model 3" = "darkorange",
                                "Linear Model 14" = "darkgreen",
                                "Polynomial Regression" = "darkred"
                     )) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top") +
  guides(fill = guide_legend(label.theme = element_text(size = 8), nrow=2, byrow=TRUE))
weekly_summary_plot

# Plot the difference (error) between the prediction and the real cost :
# Reshape data for ggplot using pivot_longer
data_long <- valid_subset %>%
  mutate(
    diff_1 = Cost_claims_year - lm_1,
    diff_3 = Cost_claims_year - lm_3,
    diff_14 = Cost_claims_year - lm_14,
    diff_pr = Cost_claims_year - lm_pr
  ) %>%
  select(diff_1, diff_3, diff_14, diff_pr) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Value")

# Remove all values above 1000 (absolute value)
data_long <- data_long %>% filter(Value < 1000, Value > -1000)

# Plot histograms
ggplot(data_long, aes(x = Value)) +
  geom_histogram(binwidth = 10, fill = "darkblue", alpha = 0.7, color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  labs(x = "Value", y = "Count") +
  scale_x_continuous(limits = c(-1000, 1000)) +
  theme_minimal()

#########################################################
#         6.5 Deep Neural Network                       #
#########################################################

# Z-normalisation function
#z_normalise <- function(x) {
#  (x - mean(x)) / sd(x)
#}

# Store the mean and standard deviation for reversing the z-normalisation : 
#mean_cost_claims <- mean(train_subset$Cost_claims_year)
#sd_cost_claims <- sd(train_subset$Cost_claims_year)


# We create 2 data frames for the deep neural network algorithm
# to convert dates to numeric and normalise values :
train_subset_dnn <- train_subset %>%
  mutate(Date_last_renewal = as.numeric(Date_last_renewal),
         Date_start_contract = as.numeric(Date_start_contract)) %>%
  #  mutate(across(where(is.numeric), z_normalize)) %>%
  mutate(across(where(is.numeric), ~ ( . - min(.) ) / ( max(.) - min(.) )))
valid_subset_dnn <- valid_subset %>%
  mutate(Date_last_renewal = as.numeric(Date_last_renewal),
         Date_start_contract = as.numeric(Date_start_contract)) %>%
  #  mutate(across(where(is.numeric), z_normalize)) %>%
  mutate(across(where(is.numeric), ~ ( . - min(.) ) / ( max(.) - min(.) )))
#str(train_subset)
#str(train_subset_dnn)

# Set the seed so we get the same results each time:
set.seed(123)

# Build the neural network model
model <- neuralnet(
  Cost_claims_year ~ Date_last_renewal,
  data = train_subset_dnn,
  hidden = c(8, 7, 6, 5, 4, 3, 2, 1),
  linear.output = TRUE
  #  threshold = 0.01,
  #  rep = 5
)

# Plot the model :
plot(model, rep = "best")

# Make predictions on the validation set :
y_hat <- predict(model, valid_subset_dnn)

# Inverse normalisation for the predictions
y_hat <- y_hat * (max(train_subset$Cost_claims_year) - min(train_subset$Cost_claims_year)) + min(train_subset$Cost_claims_year)

# Update the results table :
results <- update_results(valid_subset, y_hat, "Deep Neural Network")
results

# Update the valid_subset with the predictions :
valid_subset <- valid_subset %>%
  mutate(dnn = y_hat)


RMSE(valid_subset$Cost_claims_year, y_hat)

#total_costs <- sum(valid_subset$Cost_claims_year)
#total_costs
#total_predictions <- sum(pred)
#total_predictions
#get_error(total_costs, total_predictions)
#get_accuracy(valid_subset$Cost_claims_year, pred)

# Exit the script
stop("Stopping the script.")


library(randomForest)

# Convert categorical variables into factors
str(train_subset)
train_subset <- train_subset %>% select(ID, Date_last_renewal, Distribution_channel, Policies_in_force,
                        Lapse, Payment, Cost_claims_year, Type_risk, Power, Cylinder_capacity,
                        Value_vehicle, Age, R_Claims_history, Status)
train_subset$Distribution_channel <- as.factor(train_subset$Distribution_channel)
train_subset$Policies_in_force <- as.factor(train_subset$Policies_in_force)
train_subset$Lapse <- as.factor(train_subset$Lapse)
train_subset$Payment <- as.factor(train_subset$Payment)
train_subset$Type_risk <- as.factor(train_subset$Type_risk)
#train_subset$Power <- as.factor(train_subset$Power)
#train_subset$Cylinder_capacity <- as.factor(train_subset$Cylinder_capacity)
#train_subset$Value_vehicle <- as.factor(train_subset$Value_vehicle)
#train_subset$Age <- as.factor(train_subset$Age)
train_subset$Status <- as.factor(train_subset$Status)
train_subset$Date_last_renewal <- as.numeric(as.Date(train_subset$Date_last_renewal))

# Start timing as the code below can be quite long to run
start_time <- Sys.time()
cat("Start Time : ", format(start_time, "%H:%M:%S.%OS"))

# Assuming train_subset is already prepared
n_predictors <- ncol(train_subset) - 1

mtry_value <- min(n_predictors, 2)  # Example with 2 as default, set to max available
set.seed(123)  # For reproducibility
rf_model <- randomForest(
  Cost_claims_year ~ .,
  data = train_subset,
  ntree = 500,         
  mtry = mtry_value,        
  importance = TRUE    
)

# End timing
end_time <- Sys.time()
cat("End Time : ", format(end_time, "%H:%M:%S.%OS"))

# Calculate duration
duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
cat("Time taken:", duration, "seconds\n")

# Prediction on training data
y_hat <- predict(rf_model, train_subset)

# Calculate the Mean Squared Error
mse <- mean((train_subset$Cost_claims_year - y_hat)^2)
print(paste("MSE:", mse))

results <- update_results(valid_subset, y_hat, "Random Forest")
results

# Plot variable importance
varImpPlot(rf_model)

##############################################
# Will a policy holder submit a claim        #
##############################################

train_subset_dnn <- train_subset_dnn %>% select(-Cost_claims_year)
train_subset_dnn <- train_subset_dnn %>%
  select(Date_last_renewal, Distribution_channel, Payment, Type_risk,
         Power, Value_vehicle, Age, R_Claims_history, Status, Claim)

valid_subset_dnn <- valid_subset_dnn %>% select(-Cost_claims_year)
valid_subset_dnn <- valid_subset_dnn %>%
  select(Date_last_renewal, Distribution_channel, Payment, Type_risk,
         Power, Value_vehicle, Age, R_Claims_history, Status, Claim)

str(train_subset_dnn)

# Start timing as the code below can be quite long to run
start_time <- Sys.time()
cat("Start Time : ", format(start_time, "%H:%M:%S.%OS"))

set.seed(123)
# Build the neural network model
model <- neuralnet(
  Claim ~ .,
  data = train_subset_dnn,
  hidden = c(6),
  linear.output = FALSE
  #  threshold = 0.01,
  #  rep = 5
)

# End timing
end_time <- Sys.time()
cat("End Time : ", format(end_time, "%H:%M:%S.%OS"))

# Calculate duration
duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
cat("Time taken:", duration, "seconds\n")

#str(train_subset_dnn)

# Plot the model
plot(model, rep = "best")

# Make predictions on the scaled validation set
pred <- predict(model, valid_subset_dnn)
# Convert predictions to binary outcomes
pred <- ifelse(pred > 0.5, 1, 0)
# Inverse normalization for the predictions
#pred <- pred * (max(train_subset$Cost_claims_year) - min(train_subset$Cost_claims_year)) + min(train_subset$Cost_claims_year)

# Accuracy is 0.9997186 (99.97%) :
accuracy <- mean(pred == valid_subset_dnn$Claim)
accuracy

# Save the model
save(model, file = "neural_network_model.RData")

# Load the model
load("neural_network_model.RData")

###############
# non zero
###############

str(train_subset)

train_subset$non_zero <- ifelse(train_subset$Cost_claims_year > 0, 1, 0)

# Data for modeling non-zero costs
non_zero_data <- train_subset[train_subset$non_zero == 1, ]

# Create a function to scale numeric features
scale_features <- function(data) {
  return(scale(data))
}

# Scale numeric features (excluding the target variable)
non_zero_data_scaled <- non_zero_data
non_zero_data_scaled[, c("Date_last_renewal", "R_Claims_history", "Value_vehicle")] <- scale_features(non_zero_data[, c("Date_last_renewal", "R_Claims_history", "Value_vehicle")])

# Make sure to keep the non-zero cost
non_zero_data_scaled$Cost_claims_year <- non_zero_data$Cost_claims_year

nn_model <- neuralnet(Cost_claims_year ~ Date_last_renewal + R_Claims_history + Value_vehicle, 
                      data = non_zero_data_scaled, 
                      hidden = c(5, 3),  # Example of hidden layers; you can tune this
                      linear.output = TRUE)  # Use TRUE for regression output

predictions <- predict(nn_model, newdata = non_zero_data_scaled)

# Calculate RMSE for non-zero costs
rmse <- sqrt(mean((predictions - non_zero_data_scaled$Cost_claims_year)^2))
print(paste("Neural Network RMSE for non-zero costs:", rmse))

predictions
non_zero_data_scaled$Cost_claims_year


# Fit the neural network
nn_model <- neuralnet(
  Cost_claims_year ~ Date_last_renewal + R_Claims_history + Value_vehicle, 
  data = non_zero_data_scaled, 
  hidden = c(5, 3),  # Number of neurons in hidden layers
  linear.output = TRUE, 
  rep = 1,           # You can adjust this to train multiple times
  stepmax = 1e6      # Controls the maximum number of steps for the training
)


# Create a binary indicator for zero costs
train_subset$zero_cost <- ifelse(train_subset$Cost_claims_year == 0, 1, 0)

# Prepare data for modeling
model_data <- train_subset[!is.na(train_subset$zero_cost), ]

# Scale numeric variables
model_data_scaled <- model_data
model_data_scaled[, c("Date_last_renewal", "R_Claims_history")] <- scale(model_data[, c("Date_last_renewal", "R_Claims_history")])

nn_model <- neuralnet(zero_cost ~ Date_last_renewal + R_Claims_history, 
                      data = model_data_scaled, 
                      hidden = c(5, 3),  # You can adjust the hidden layers
                      linear.output = FALSE)  # Use FALSE for binary classification

predictions <- predict(nn_model, newdata = model_data_scaled)

# Convert probabilities to binary predictions
predicted_classes <- ifelse(predictions > 0.5, 1, 0)  # Using 0.5 as the decision threshold

# Calculate accuracy
accuracy <- mean(predicted_classes == model_data_scaled$zero_cost)
print(paste("Model Accuracy:", accuracy))

# Confusion Matrix
table(Predicted = predicted_classes, Actual = model_data_scaled$zero_cost)

# Plot predictions
results <- data.frame(Actual = model_data_scaled$zero_cost, Predicted = predicted_classes)
ggplot(results, aes(x = Actual, fill = as.factor(Predicted))) +
  geom_bar(position = "dodge") +
  labs(title = "Actual vs Predicted Zero Costs", y = "Count") +
  scale_fill_discrete(name = "Predicted")



########################################
# Random Forest : 0 or non-zero
########################################

library(randomForest)

# Create a binary indicator for zero costs
train_subset$zero_cost <- ifelse(train_subset$Cost_claims_year == 0, 1, 0)
str(train_subset)
# Prepare data for modeling
model_data <- train_subset[!is.na(train_subset$zero_cost), ]
str(model_data)
# Convert categorical variables to factors if necessary
#model_data$variable1 <- as.factor(model_data$variable1)  # Example for categorical transformation
# Repeat for other categorical variables if needed

# Optionally, consider removing the original cost variable if it's not needed
model_data$Cost_claims_year <- NULL

# Remove cost and zero_cost
train_subset <- train_subset %>% select(-ID, -Cost_claims_year, -non_zero)

set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(model_data), 0.7 * nrow(model_data))  # 70% for training
train_data <- model_data[train_indices, ]
test_data <- model_data[-train_indices, ]

# Start timing as the code below can be quite long to run
start_time <- Sys.time()
cat("Start Time : ", format(start_time, "%H:%M:%S.%OS"))

rf_model <- randomForest(zero_cost ~ ., 
                         data = train_data, 
                         ntree = 100,  # Number of trees
                         mtry = 2,     # Number of variables considered at each split
                         importance = TRUE)

predictions <- predict(rf_model, newdata = test_data)

# End timing
end_time <- Sys.time()
cat("End Time : ", format(end_time, "%H:%M:%S.%OS"))

# Calculate duration
duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
cat("Time taken:", duration, "seconds\n")

# Check the first few predictions
head(predictions)

threshold <- 0.5  # You can change this value based on your needs
# Apply the threshold to convert probabilities to binary predictions
predictions <- ifelse(predictions >= threshold, 1, 0)

# Calculate accuracy
accuracy <- mean(predictions == test_data$zero_cost)
print(paste("Model Accuracy:", accuracy))

accuracy_zero <- mean(0 == test_data$zero_cost)
print(paste("Model Accuracy Zero:", accuracy_zero))

# Confusion Matrix
confusion_matrix <- table(Predicted = predictions, Actual = test_data$zero_cost)
print(confusion_matrix)

########################################
# Random Forest : 0 or non-zero
########################################

# Convert specified columns to factors
#factor_columns <- c("Distribution_channel", "Policies_in_force", "Payment", "Type_risk", "Status")
factor_columns <- c("Distribution_channel", "Payment", "Type_risk", "Status")
train_subset[factor_columns] <- sapply(train_subset[factor_columns], as.factor, simplify = FALSE)  # Use simplify = FALSE to keep the structure
# Convert Date_last_renewal to numeric
train_subset$Date_last_renewal <- as.numeric(as.Date(train_subset$Date_last_renewal))
train_subset$Date_start_contract <- as.numeric(as.Date(train_subset$Date_start_contract))

train_subset_rf <- train_subset %>% select(-Cost_claims_year, -Policies_in_force)
str(train_subset_rf)

set.seed(123)  # For reproducibility
# Start timing as the code below can be quite long to run
start_time <- Sys.time()
cat("Start Time : ", format(start_time, "%H:%M:%S.%OS"))

rf_model <- randomForest(Claim ~ ., 
                         data = train_subset_rf, 
                         ntree = 100,  # Number of trees
                         mtry = 2,     # Number of variables considered at each split
                         importance = TRUE)

predictions <- predict(rf_model, newdata = train_subset_rf)

# End timing
end_time <- Sys.time()
cat("End Time : ", format(end_time, "%H:%M:%S.%OS"))

# Calculate duration
duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
cat("Time taken:", duration, "seconds\n")

threshold <- 0.5  # You can change this value based on your needs
# Apply the threshold to convert probabilities to binary predictions
predictions <- ifelse(predictions >= threshold, 1, 0)

# Calculate accuracy : 0.941628590656324 (94.16%)
accuracy <- mean(predictions == train_subset_rf$Claim)
print(paste("Model Accuracy:", accuracy))

# Validation
# Convert specified columns to factors
valid_subset[factor_columns] <- sapply(valid_subset[factor_columns], as.factor, simplify = FALSE)
# Convert Date_last_renewal to numeric
valid_subset$Date_last_renewal <- as.numeric(as.Date(valid_subset$Date_last_renewal))
valid_subset$Date_start_contract <- as.numeric(as.Date(valid_subset$Date_start_contract))

#valid_subset_rf <- valid_subset_rf %>% select(-Policies_in_force, -lm_1, -lm_3, -lm_14, -lm_pr, -dnn)
valid_subset_rf <- valid_subset_rf %>% select(-Policies_in_force)
str(valid_subset_rf)

y_hat <- predict(rf_model, newdata = valid_subset_rf)
y_hat <- ifelse(y_hat >= threshold, 1, 0)

# Calculate accuracy : 0.912117801538173 (91.21%)
valid_accuracy <- mean(y_hat == valid_subset_rf$Claim)
print(paste("Model Accuracy:", valid_accuracy))

mean(train_subset_rf$Claim == 0)
mean(valid_subset_rf$Claim == 0)
mean(y_hat == 0)

########################################
# Random Forest : 0 or non-zero (end)
########################################

########################################
# Deep neural network : 0 or non-zero (end)
########################################

# Assuming your train_subset has a target variable 'cost'
# Filter the training data for only the claims
train_claims <- train_subset[train_subset$pred_claim == 1, ]

# Ensure you have a target variable 'cost' for training
# Define the formula for the neural network
formula <- cost ~ predictors  # replace 'predictors' with your predictor variables

# Train the neural network
nn_model <- neuralnet(formula, data = train_claims, hidden = c(5), linear.output = TRUE)

# Prepare the validation set
# If there are rows with pred_claim = 0, set their costs to zero
valid_subset$predicted_cost <- 0  # initialize the predicted costs with zeros

# Now filter the validation set for predictions
valid_claims <- valid_subset[valid_subset$pred_claim == 1, ]

# Make predictions on the validation set where pred_claim = 1
predictions <- predict(nn_model, newdata = valid_claims)

# Assign predictions to the corresponding rows in valid_subset
valid_subset$predicted_cost[valid_subset$pred_claim == 1] <- predictions

# View the results
head(valid_subset)

########################################
# Deep neural network : 0 or non-zero (end)
########################################












factor_columns <- c("Distribution_channel", "Policies_in_force", "Lapse", "Payment", "Type_risk", "Status")

# Print levels in the training dataset
train_levels <- lapply(train_subset[factor_columns], levels)
print(train_levels)

# Print levels in the validation dataset
valid_levels <- lapply(valid_subset_rf[factor_columns], levels)
print(valid_levels)


# Check the levels of factor variables in the training and validation datasets
train_factors <- sapply(train_subset_rf[factor_columns], levels)
valid_factors <- sapply(valid_subset_rf[factor_columns], levels)

# Compare levels for each factor variable
for (col in factor_columns) {
  print(paste("Column:", col))
  print(setdiff(valid_factors[[col]], train_factors[[col]]))  # Levels in valid that aren't in train
}

# Create a unified factor for both training and validation sets
for (col in factor_columns) {
  if (col %in% names(train_subset_rf)) {
    all_levels <- unique(c(levels(train_subset_rf[[col]]), levels(valid_subset_rf[[col]])))
    train_subset_rf[[col]] <- factor(train_subset_rf[[col]], levels = all_levels)
    valid_subset_rf[[col]] <- factor(valid_subset_rf[[col]], levels = all_levels)
  }
}

for (col in factor_columns) {
  valid_subset_rf[[col]][!(valid_subset_rf[[col]] %in% levels(train_subset_rf[[col]]))] <- NA
}





train_subset$Cost_claims_year_log <- log(train_subset$Cost_claims_year + 1)  # +1 avoids log(0)
summary(train_subset)
# Fit the model using the log-transformed target
model_log <- lm(Cost_claims_year_log ~ Date_last_renewal_numeric + R_Claims_history + Age, data = train_subset)
summary(model_log)

# Predictions
predictions_log <- exp(predict(model_log)) - 1
predictions_log

rmse_log <- sqrt(mean((predictions_log - train_subset$Cost_claims_year)^2))
print(paste("Log-transformed RMSE:", rmse_log))


min(train_subset$Cost_claims_year)
max(train_subset$Cost_claims_year)
sum(train_subset$Cost_claims_year == 0) / nrow(train_subset)

Q1 <- quantile(train_subset$Cost_claims_year, 0.25)
Q3 <- quantile(train_subset$Cost_claims_year, 0.75)
IQR <- Q3 - Q1
Q1
Q3
ggplot(train_subset, aes(x = Cost_claims_year)) + geom_histogram(binwidth = 10)
train_subset_clean$Cost_claims_year_log <- log(train_subset_clean$Cost_claims_year + 1)
ggplot(train_subset_clean, aes(x = Cost_claims_year_log)) + geom_histogram(binwidth = 10)

outlier_thresholds <- c(Q1 - 1.5 * IQR, Q3 + 1.5 * IQR)
train_subset$Date_last_renewal_numeric <- as.numeric(as.Date(train_subset$Date_last_renewal))
train_subset_clean <- train_subset %>%
  filter(Cost_claims_year > outlier_thresholds[1] & Cost_claims_year < outlier_thresholds[2])
outlier_thresholds[1]
outlier_thresholds[2]
head(train_subset_clean)
model <- lm(Cost_claims_year ~ poly(Date_last_renewal_numeric, 4), data = train_subset_clean)
summary(model)















































#########################################################
#         6.4 Biases                                    #
#########################################################

df <- train_subset %>% select(ID, Date_last_renewal, Cost_claims_year, R_Claims_history)
df_0 <- df %>% filter(R_Claims_history == 0)
df_1 <- df %>% filter(R_Claims_history > 0)
nrow(df)
nrow(df_0)
nrow(df_1)

nrow(df_0 %>% filter(Cost_claims_year == 0))
nrow(df_0 %>% filter(Cost_claims_year > 0))

nrow(df_1 %>% filter(Cost_claims_year == 0))
nrow(df_1 %>% filter(Cost_claims_year > 0))
avg_cost
df_0 %>% summarise(avg_cost = mean(Cost_claims_year)) %>% pull(avg_cost)
df_1 %>% summarise(avg_cost = mean(Cost_claims_year)) %>% pull(avg_cost)

# Rounding R_Claims_history to the next decimal
df$R_Claims_history <- ceiling(df$R_Claims_history * 10) / 10

min(df$R_Claims_history)
max(df$R_Claims_history)
mean(df$R_Claims_history)

# Calculate the average Cost_claims_year by R_Claims_history
b_history <- df %>%
  group_by(R_Claims_history) %>%
  summarise(b_h = mean(Cost_claims_year) - avg_cost)
b_history
# Create the plot
ggplot(data = b_history, aes(x = R_Claims_history, y = b_h)) +
  geom_point() +
  geom_line() +
  labs(x = "R Claims History", y = "Bias (History)") +
  theme_minimal()

new_df <- valid_subset %>% select(Date_last_renewal, Cost_claims_year, lm_pr, R_Claims_history)
# Rounding R_Claims_history to the next decimal
new_df$R_Claims_history <- ceiling(new_df$R_Claims_history * 10) / 10
new_df
new_df <- new_df %>% left_join(b_history, by = "R_Claims_history")
new_df <- new_df %>% mutate(lm_pr_2 = lm_pr + b_h)
new_df

RMSE(new_df$Cost_claims_year, new_df$lm_pr_2)
total_costs <- sum(new_df$Cost_claims_year)
total_costs
total_predictions <- sum(new_df$lm_pr_2)
total_predictions
get_error(total_costs, total_predictions)
get_accuracy(new_df$Cost_claims_year, new_df$lm_pr_2)

df <- train_subset %>%
  filter(Date_last_renewal >= as.Date(cutoff_subset_date) - months(3)) %>%
  select(ID, Date_last_renewal, Cost_claims_year, status)

df <- train_subset %>% select(ID, Date_last_renewal, Cost_claims_year, status)
df
# TEMP add the predictions to the train_set as well
lm_pr <- predict(train, df, type = "raw")
lm_pr
df <- df %>% mutate(lm_pr_val = lm_pr)
df
b_s <- df %>%
  group_by(status) %>%
  summarise(value = mean(Cost_claims_year - lm_pr_val))
b_s
new_df <- valid_subset %>% select(Date_last_renewal, Cost_claims_year, lm_pr, status)
new_df <- new_df %>% left_join(b_s, by = "status")
new_df <- new_df %>% mutate(lm_pr_2 = lm_pr + value)
new_df
results
RMSE(new_df$Cost_claims_year, new_df$lm_pr_2)
total_costs <- sum(new_df$Cost_claims_year)
total_costs
total_predictions <- sum(new_df$lm_pr_2)
total_predictions
get_error(total_costs, total_predictions)
get_accuracy(new_df$Cost_claims_year, new_df$lm_pr_2)

b_s <- df %>%
  group_by(status) %>%
  summarise(value = mean(Cost_claims_year - avg_cost_3m))
b_s

k <- 0
RMSE(valid_subset$Cost_claims_year, valid_subset$lm_1 + k)







k_values <- -100:100
rmse_results <- sapply(k_values, function(k) {
  sqrt(mean((valid_subset$Cost_claims_year - (valid_subset$lm_1 + k))^2))
})

# Display the RMSE results
print(rmse_results)
which.min(rmse_results)
rmse_results[which.min(rmse_results)]
# Plot RMSE results against k values
plot(k_values, rmse_results, type = "b", pch = 19, col = "blue", 
     xlab = "k Values", ylab = "RMSE", 
     main = "RMSE vs k Values")



#########################################################
#         6.4 Deep Neural Network                       #
#########################################################

str(train_subset)
train_subset_dnn <- train_subset %>%
  mutate(status = if_else(year(Date_start_contract) == year(Date_last_renewal), 1, 0))
train_subset_dnn$Date_start_contract <- as.numeric(as.Date(train_subset_dnn$Date_start_contract))
train_subset_dnn$Date_last_renewal <- as.numeric(as.Date(train_subset_dnn$Date_last_renewal))
str(train_subset_dnn)
valid_subset_dnn <- valid_subset %>%
  mutate(status = if_else(year(Date_start_contract) == year(Date_last_renewal), 1, 0))
valid_subset_dnn$Date_start_contract <- as.numeric(as.Date(valid_subset_dnn$Date_start_contract))
valid_subset_dnn$Date_last_renewal <- as.numeric(as.Date(valid_subset_dnn$Date_last_renewal))

train_subset_dnn <- train_subset_dnn %>%
  select(Cost_claims_year, Date_last_renewal, Cylinder_capacity, Weight, N_doors, Type_risk, R_Claims_history,
         Area, Max_products, Second_driver, Age, Value_vehicle, Payment, Year_matriculation, status)
valid_subset_dnn <- valid_subset_dnn %>%
  select(Cost_claims_year, Date_last_renewal, Cylinder_capacity, Weight, N_doors, Type_risk, R_Claims_history,
         Area, Max_products, Second_driver, Age, Value_vehicle, Payment, Year_matriculation, status)


# Load the necessary libraries
library(tidyverse)
library(neuralnet)

# Build the neural network model
model <- neuralnet(
  Cost_claims_year ~ .,
  data = train_subset_dnn,
  hidden = c(4, 2),
  linear.output = TRUE
)

# Plot the model
plot(model, rep = "best")

# Make predictions on the validation set
pred <- predict(model, valid_subset_dnn)

# Check predictions
prediction_label <- pred  # Directly use `pred` as predictions are numeric

# Create a confusion matrix (if applicable)
# Here, you might want to discretize the predicted values if you expect specific classes
# E.g., if Cost_claims_year is expected to be in certain classes
predicted_classes <- ifelse(pred > 0.5, 1, 0)  # Adjust threshold as needed

# Create a confusion table, ensure valid_subset_dnn has Cost_claims_year as factor
table(factor(valid_subset_dnn$Cost_claims_year), factor(predicted_classes))

# Load the necessary libraries
library(tidyverse)
library(neuralnet)

# Build the neural network model
model <- neuralnet(
  Cost_claims_year ~ .,
  data = train_subset_dnn,
  hidden = c(10),
  linear.output = TRUE  # Set TRUE for regression
)

# Plot the model
plot(model, rep = "best")

# Make predictions on the validation set
pred <- predict(model, valid_subset_dnn)

# Review predictions
print(pred)

# Optionally, you can create a summary of predictions:
summary(pred)

# Compare predictions with actual values
comparison <- data.frame(Actual = valid_subset_dnn$Cost_claims_year, Predicted = pred)
print(head(comparison))  # Display the first few actual vs predicted values

# Check performance metrics, such as RMSE
rmse <- sqrt(mean((comparison$Actual - comparison$Predicted)^2))
print(paste("RMSE: ", rmse))







#########################################################
#         6.4 Deep Neural Network (2)                   #
#########################################################

str(train_subset)
train_subset_dnn <- train_subset %>%
  mutate(status = if_else(year(Date_start_contract) == year(Date_last_renewal), 1, 0),
         is_claim = if_else(Cost_claims_year > 0, 1, 0))
train_subset_dnn$Date_start_contract <- as.numeric(as.Date(train_subset_dnn$Date_start_contract))
train_subset_dnn$Date_last_renewal <- as.numeric(as.Date(train_subset_dnn$Date_last_renewal))
str(train_subset_dnn)
valid_subset_dnn <- valid_subset %>%
  mutate(status = if_else(year(Date_start_contract) == year(Date_last_renewal), 1, 0),
         is_claim = if_else(Cost_claims_year > 0, 1, 0))
valid_subset_dnn$Date_start_contract <- as.numeric(as.Date(valid_subset_dnn$Date_start_contract))
valid_subset_dnn$Date_last_renewal <- as.numeric(as.Date(valid_subset_dnn$Date_last_renewal))

train_subset_dnn <- train_subset_dnn %>%
  select(Cost_claims_year, Date_last_renewal, Cylinder_capacity, Weight, N_doors, Type_risk, R_Claims_history,
         Area, Max_products, Second_driver, Age, Value_vehicle, Payment, Year_matriculation, status, is_claim)
valid_subset_dnn <- valid_subset_dnn %>%
  select(Cost_claims_year, Date_last_renewal, Cylinder_capacity, Weight, N_doors, Type_risk, R_Claims_history,
         Area, Max_products, Second_driver, Age, Value_vehicle, Payment, Year_matriculation, status, is_claim)


train_subset_dnn <- train_subset_dnn %>%
  select(Date_last_renewal, Cylinder_capacity, Type_risk, R_Claims_history,
         Age, Value_vehicle, Payment, status, is_claim)
valid_subset_dnn <- valid_subset_dnn %>%
  select(Date_last_renewal, Cylinder_capacity, Type_risk, R_Claims_history,
         Age, Value_vehicle, Payment, status, is_claim)

# Load the necessary libraries
library(tidyverse)
library(neuralnet)

# Normalize train and validation datasets
train_subset_dnn_scaled <- train_subset_dnn %>%
  mutate(across(where(is.numeric), ~ ( . - min(.) ) / ( max(.) - min(.) )))
valid_subset_dnn_scaled <- valid_subset_dnn %>%
  mutate(across(where(is.numeric), ~ ( . - min(.) ) / ( max(.) - min(.) )))

# Build the neural network model
model <- neuralnet(
  is_claim ~ .,
  data = train_subset_dnn_scaled,
  hidden = c(6, 2),  # Start with 10 neurons in one layer
  linear.output = TRUE
)

# Plot the model
plot(model, rep = "best")

# Make predictions on the scaled validation set
pred <- predict(model, valid_subset_dnn_scaled)

# Inverse normalization for the predictions
pred <- pred * (max(train_subset_dnn$Cost_claims_year) - min(train_subset_dnn$Cost_claims_year)) + min(train_subset_dnn$Cost_claims_year)

# Compare predictions with actual values
comparison <- data.frame(Actual = valid_subset_dnn$Cost_claims_year, Predicted = pred)
print(head(comparison))  # Display the first few actual vs predicted values

# Check performance metrics, such as RMSE
rmse <- sqrt(mean((comparison$Actual - comparison$Predicted)^2))
print(paste("RMSE: ", rmse))

###############################################################

# Build the neural network model to predict is_claim
model <- neuralnet(
  is_claim ~ .,
  data = train_subset_dnn_scaled,
  hidden = c(4, 2),  # Adjust based on your experimentation
  linear.output = FALSE  # Set to FALSE for binary classification
)

# Plot the model
plot(model, rep = "best")

# Make predictions on the scaled validation set
pred <- predict(model, valid_subset_dnn_scaled)

# Since the output will be probabilities, apply a threshold to classify as 0 or 1
pred_class <- ifelse(pred > 0.5, 1, 0)  # Change threshold as needed

# Compare predictions with actual values
comparison <- data.frame(Actual = valid_subset_dnn_scaled$is_claim, Predicted = pred_class)
print(head(comparison))  # Display the first few actual vs predicted values

# Check performance metrics, such as accuracy
accuracy <- mean(comparison$Actual == comparison$Predicted)
print(paste("Accuracy: ", accuracy))

# Optionally calculate confusion matrix
confusion_matrix <- table(comparison$Actual, comparison$Predicted)
print(confusion_matrix)

##############################################
##############################################


# Z-normalization function
z_normalize <- function(x) {
  (x - mean(x)) / sd(x)
}
# Assume you have stored these values during z-normalization
mean_cost_claims <- mean(train_subset$Cost_claims_year)
sd_cost_claims <- sd(train_subset$Cost_claims_year)

str(train_subset)
str(train_subset_dnn)
train_subset_dnn <- train_subset %>%
  mutate(status = if_else(status == "New", 1, 0),
         Claim = if_else(Cost_claims_year > 0, 1, 0),
         Date_start_contract = as.numeric(Date_start_contract),
         Date_last_renewal = as.numeric(Date_last_renewal)) %>%
#  mutate(across(where(is.numeric), z_normalize)) %>%
  mutate(across(where(is.numeric), ~ ( . - min(.) ) / ( max(.) - min(.) ))) %>%
  select(ID, Claim, Cost_claims_year, Date_last_renewal, Cylinder_capacity,
         Weight, N_doors, Type_risk, R_Claims_history,
         Area, Max_products, Second_driver, Age, Value_vehicle,
         Payment, Year_matriculation, status)
valid_subset_dnn <- valid_subset %>%
  mutate(status = if_else(status == "New", 1, 0),
         Claim = if_else(Cost_claims_year > 0, 1, 0),
         Date_start_contract = as.numeric(Date_start_contract),
         Date_last_renewal = as.numeric(Date_last_renewal)) %>%
#  mutate(across(where(is.numeric), z_normalize)) %>%
  mutate(across(where(is.numeric), ~ ( . - min(.) ) / ( max(.) - min(.) ))) %>%
  select(ID, Claim, Cost_claims_year, Date_last_renewal, Cylinder_capacity,
         Weight, N_doors, Type_risk, R_Claims_history,
         Area, Max_products, Second_driver, Age, Value_vehicle,
         Payment, Year_matriculation, status)
str(train_subset)
str(train_subset_dnn)

set.seed(123)
# Build the neural network model
model <- neuralnet(
  Cost_claims_year ~ Date_last_renewal,
  data = train_subset_dnn,
  hidden = c(8, 7, 6, 5, 4, 3, 2, 1),
  linear.output = TRUE
#  threshold = 0.01,
#  rep = 5
)

# Plot the model
plot(model, rep = "best")

# Make predictions on the scaled validation set
pred <- predict(model, valid_subset_dnn)

# Inverse normalization for the predictions
pred <- pred * (max(train_subset$Cost_claims_year) - min(train_subset$Cost_claims_year)) + min(train_subset$Cost_claims_year)
#pred

RMSE(valid_subset_dnn$Cost_claims_year, pred)

total_costs <- sum(valid_subset$Cost_claims_year)
#total_costs
total_predictions <- sum(pred)
#total_predictions
get_error(total_costs, total_predictions)
get_accuracy(valid_subset$Cost_claims_year, pred)

##############################################
# Will a policy holder submit a claim        #
##############################################

# Start timing as the code below can be quite long to run
start_time <- Sys.time()
cat("Start Time : ", format(start_time, "%H:%M:%S.%OS"))

set.seed(123)
# Build the neural network model
model <- neuralnet(
  Claim ~ .,
  data = train_subset_dnn,
  hidden = c(6, 4, 2, 1),
  linear.output = TRUE
  #  threshold = 0.01,
  #  rep = 5
)

# End timing
end_time <- Sys.time()
cat("End Time : ", format(end_time, "%H:%M:%S.%OS"))

# Calculate duration
duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
cat("Time taken:", duration, "seconds\n")

str(train_subset_dnn)
# Save the model
save(model, file = "neural_network_model.RData")

# Load the model
load("neural_network_model.RData")

# Plot the model
plot(model, rep = "best")

# Make predictions on the scaled validation set
pred <- predict(model, valid_subset_dnn)
# Convert predictions to binary outcomes
pred <- ifelse(pred > 0.5, 1, 0)
# Inverse normalization for the predictions
#pred <- pred * (max(train_subset$Cost_claims_year) - min(train_subset$Cost_claims_year)) + min(train_subset$Cost_claims_year)

# Accuracy is 0.9997186 (99.97%) :
accuracy <- mean(pred == valid_subset_dnn$Claim)
accuracy

##############################################
##############################################

train_subset_dnn_2 <- train_subset_dnn %>%
  select(Claim,
         Date_last_renewal,
         R_Claims_history,
         Cylinder_capacity,
         Value_vehicle,
         Type_risk,
         Age,
         Payment,
         Status)

# Start timing as the code below can be quite long to run
start_time <- Sys.time()
cat("Start Time : ", format(start_time, "%H:%M:%S.%OS"))

set.seed(123)
# Build the neural network model
model_2 <- neuralnet(
  Claim ~ .,
  data = train_subset_dnn_2,
  hidden = c(4, 2, 1),
  linear.output = TRUE
  #  threshold = 0.01,
  #  rep = 5
)

# End timing
end_time <- Sys.time()
cat("End Time : ", format(end_time, "%H:%M:%S.%OS"))

# Calculate duration
duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
cat("Time taken:", duration, "seconds\n")


















# Load the package
library(nnet)

# Create a neural network model without requiring Python
model <- nnet(
  Cost_claims_year ~ Date_last_renewal,
  data = train_subset_dnn,
  size = 10,  # Number of neurons
  linout = TRUE,  # Linear output for regression
  trace = FALSE   # Disable training output
)

# Make predictions
pred <- predict(model, valid_subset_dnn)
# Inverse Z-normalization for predictions
pred <- pred * sd_cost_claims + mean_cost_claims

# Calculate RMSE
RMSE(valid_subset$Cost_claims_year, pred)

rmse <- sqrt(mean((valid_subset$Cost_claims_year - pred)^2))
print(paste("RMSE:", rmse))

max(pred)


library(caret)
# Train the neural network model
model <- train(
  Cost_claims_year ~ Date_last_renewal,
  data = train_subset_dnn,
  method = "nnet",
  linout = TRUE,
  trace = FALSE,
  size = 6  # Number of neurons in one layer
)

# Make predictions
pred <- predict(model, valid_subset_dnn_scaled)

# Inverse Z-normalization for predictions
pred <- pred * sd_cost_claims + mean_cost_claims

# Calculate RMSE
RMSE(valid_subset$Cost_claims_year, pred)



library(tidyverse)
library(neuralnet)

model = neuralnet(
  Cost_claims_year ~ .,
  data = train_subset_dnn,
  hidden = c(4,2),
  linear.output = FALSE
)

plot(model, rep = "best")

pred <- predict(model, valid_subset_dnn)
labels <- setdiff(names(valid_subset_dnn), "Cost_claims_year")
prediction_label <- data.frame(max.col(pred)) %>%     
  mutate(pred = labels[max.col.pred.]) %>%
  select(2) %>%
  unlist()

table(valid_subset_dnn$Cost_claims_year, prediction_label)

check = as.numeric(valid_subset_dnn$Cost_claims_year) == max.col(pred)
accuracy = (sum(check)/nrow(valid_subset_dnn))*100
print(accuracy)












library(keras)
library(mlbench)
library(magrittr)
library(neuralnet)

n <- neuralnet(Cost_claims_year ~ .,
               data = train_subset_dnn,
               hidden = c(12,7),
               linear.output = F,
               lifesign = 'full',
               rep=1)

plot(n,col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')


data <- as.matrix(train_subset_dnn)
dimnames(data) <- NULL

set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(.7, .3))
training <- data[ind==1,1:13]
test <- data[ind==2, 1:13]
trainingtarget <- data[ind==1, 14]
testtarget <- data[ind==2, 14]
str(trainingtarget)
str(testtarget)

m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = 'relu', input_shape = c(13)) %>%
  layer_dense(units = 1)






# Movie bias
b_m <- edx_movies %>%
  group_by(movieId) %>%
  summarise(b_m = mean(rating - mu), .groups = "drop")
head(b_m)
edx_movies <- edx_movies %>% left_join(b_m, by = "movieId")




c_lm_1 <- valid_subset %>%
  mutate(
    diff_0 = Cost_claims_year - 0,
    diff_1 = Cost_claims_year - lm_1,
    diff_3 = Cost_claims_year - lm_3,
    diff_14 = Cost_claims_year - lm_14,
    diff_pr = Cost_claims_year - lm_pr
  ) %>%
  summarise(avg = mean(diff_1)) %>%
  pull(avg)
c_lm_1
data_long <- valid_subset %>%
  mutate(new_lm_1 = lm_1 + 2.5 * c_lm_1,
         diff_0 = Cost_claims_year - 0,
         diff_1 = Cost_claims_year - lm_1,
         diff_new_1 = Cost_claims_year - new_lm_1,
         diff_3 = Cost_claims_year - lm_3,
         diff_14 = Cost_claims_year - lm_14,
         diff_pr = Cost_claims_year - lm_pr
  ) %>%
  select(diff_0, diff_1, diff_new_1, diff_3, diff_14, diff_pr) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Value")
data_long <- data_long %>% filter(Value < 1000, Value > -1000)
# Plot histograms
ggplot(data_long, aes(x = Value)) +
  geom_histogram(binwidth = 10, fill = "darkblue", alpha = 0.7, color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  labs(x = "Value", y = "Count")

valid_subset <- valid_subset %>%
  mutate(new_lm_1 = lm_1 + 2.5 * c_lm_1)

rmse <- RMSE(valid_subset$Cost_claims_year, valid_subset$new_lm_1)
total_costs <- sum(valid_subset$Cost_claims_year)
total_predictions <- sum(valid_subset$new_lm_1)
rel_error <- get_error(total_costs, total_predictions)
accuracy <- get_accuracy(valid_subset$Cost_claims_year, valid_subset$new_lm_1)

# Update the results table :
results <- results %>% add_row(Method = "Test LM1",
                               RMSE = round(rmse, 2),
                               Cost = round(total_costs, 2),
                               Estimation = round(total_predictions, 2),
                               Error = round(rel_error, 3),
                               Accuracy = round(accuracy, 3))

results

#########################################################
#         6.5 Loess Model                               #
#########################################################

# Linear Model with 1 parameter and polynomial regression :
# RMSE = 474.2981
# Error = 0.562
#train_subset$Date_last_renewal <- scale(train_subset$Date_last_renewal)
#valid_subset$Date_last_renewal <- scale(valid_subset$Date_last_renewal)
#train_lm <- train(Cost_claims_year ~ Date_last_renewal, method = "knn", data = train_subset)
#y_hat_lm <- predict(train_lm, valid_subset, type = "raw")

#rmse_lm <- RMSE(valid_subset$Cost_claims_year, y_hat_lm)
#total_cost_actuals_lm <- sum(valid_subset$Cost_claims_year)
#total_cost_preds_lm <- sum(y_hat_lm)
#total_cost_diff_lm <- total_cost_preds_lm - total_cost_actuals_lm
#cost_error_lm <- total_cost_diff_lm / total_cost_actuals_lm

# Update the valid_subset with the predictions :
#valid_subset <- valid_subset %>%
#  mutate(lm_pr = y_hat_lm)

# Update the results table :
#results <- results %>% add_row(Method = "knn",
#                               RMSE = round(rmse_lm, 2),
#                               Cost = round(total_cost_actuals_lm, 2),
#                               Estimation = round(total_cost_preds_lm, 2),
#                               Error = round(cost_error_lm, 3))
#results

# Exit the script
stop("Stopping the script.")



total_days <- diff(range(train_subset$Date_last_renewal))
total_days <- diff(range(train_subset$Date_numeric))
str(train_subset)
total_days <- 941
total_days
span <- 21/total_days
span <- 0.3
fit <- loess(Cost_claims_year ~ Date_last_renewal, degree=1, span = span, data=train_subset)
y_hat_lm <- predict(fit, valid_subset, type = "raw")
y_hat_lm
y_hat_lm[is.na(y_hat_lm)] <- 0
y_hat_lm
RMSE(valid_subset$Cost_claims_year, y_hat_lm)

# Calculate weekly summary data :
weekly_summary_2 <- valid_subset %>%
  select(Date_last_renewal, Cost_claims_year, lm_pr) %>%
  mutate(loess = y_hat_lm) %>%
  mutate(Week = floor_date(Date_last_renewal, unit = "week")) %>%
  group_by(Week) %>%
  summarize(
    Total_Cost = sum(Cost_claims_year, na.rm = TRUE),
    Total_lm_1 = sum(lm_1, na.rm = TRUE),
    Total_lm_pr = sum(lm_pr, na.rm = TRUE),
    Total_lm_loess = sum(loess, na.rm = TRUE)
  )


valid_subset %>%
  select(Date_last_renewal, Cost_claims_year, lm_pr) %>%
  mutate(loess = y_hat_lm)















train_subset %>%
  filter(Date_last_renewal > as.Date("2018-04-01")) %>%
  summarise(avg_cost_2 = mean(Cost_claims_year))
RMSE(valid_subset$Cost_claims_year, y)
# Assuming valid_subset$Cost_claims_year is your actual values
results <- sapply(1:300, function(y) RMSE(valid_subset$Cost_claims_year, y))
# View the results
min(results)
which.min(results)
RMSE(valid_subset$Cost_claims_year, 49)
mean(valid_subset$Cost_claims_year)
train_set

# Filter data for the months April to August 2018
filtered_data <- train_set %>%
  filter(Date_last_renewal >= as.Date("2018-04-01") & Date_last_renewal <= as.Date("2018-08-31"))

# Group by week and calculate the average cost
average_cost_per_week <- filtered_data %>%
#  group_by(Week = floor_date(Date_last_renewal, "week")) %>%
  group_by(Week = floor_date(Date_last_renewal, "day")) %>%
  summarise(Average_Cost = mean(Cost_claims_year, na.rm = TRUE)) %>%
  ungroup()

# Display the results
average_cost_per_week

# Create the plot
ggplot(average_cost_per_week, aes(x = Week, y = Average_Cost)) +
  geom_line() +  # Add line connecting points
  geom_point() +  # Add points for each week's average cost
  labs(title = "Average Cost per Week (April-August 2018)",
       x = "Week",
       y = "Average Cost") +
  theme_minimal() +  # Use a minimal theme
  scale_x_date(date_labels = "%b %d", breaks = "1 week")  # Format x-axis labels

# Assuming 'Date_start_contract' and 'Date_last_renewal' are the date columns
date_columns <- c("Date_start_contract", "Date_last_renewal")

# Loop through each date column and convert to numeric
for (date_col in date_columns) {
  train_subset[[date_col]] <- as.numeric(as.POSIXct(train_subset[[date_col]]))
  valid_subset[[date_col]] <- as.numeric(as.POSIXct(valid_subset[[date_col]]))
}

# Set control parameters for RFE
control <- rfeControl(functions = lmFuncs,
                      method = "cv",
                      number = 10)

# Execute RFE
results <- rfe(train_subset[, -which(names(train_subset) == "Cost_claims_year")],
               train_subset$Cost_claims_year,
               sizes = c(1:25),
               rfeControl = control)

# View the results
print(results)


# Run Recursive Feature Elimination (RFE)
results <- rfe(train_subset[, -which(names(train_subset) == "Cost_claims_year")],
               train_subset$Cost_claims_year,
               sizes = c(1:25),
               rfeControl = control)

# Extract the RMSE and number of Variables
rmses <- results$results$RMSE
sizes <- results$results$Variables

# Initialize an empty list to hold variable names corresponding to each size
feature_names <- vector("list", length(sizes))

# Populate the list with variable names for each size
for (i in seq_along(sizes)) {
  feature_names[[i]] <- paste(results$optVariables[1:sizes[i]], collapse = ", ")
}

# Create a data frame to display variable names along with their associated RMSE
rmse_table <- data.frame(
  Features = feature_names,
  RMSE = rmses
)

# View the results
print(rmse_table)

# Filter the data frame to show only the 10 rows with the lowest RMSE
lowest_rmse_table <- rmse_table[order(rmse_table$RMSE), ][1:2, ]

# View the filtered results
print(lowest_rmse_table)

model <- glm(Cost_claims_year ~ Lapse + Payment + Policies_in_force + Second_driver + R_Claims_history, 
             data = train_subset_clean, 
             family = gaussian())  # or another appropriate family
summary(model)



train_lm <- train(Cost_claims_year ~ ., method = "lm", data = train_subset_clean)
y_hat_lm <- predict(train_lm, valid_subset_clean, type = "raw")
RMSE(valid_subset$Cost_claims_year, y_hat_lm)

train_lm <- train(Cost_claims_year ~ Lapse + Payment + Policies_in_force + Second_driver + R_Claims_history, method = "lm", data = train_subset_clean)
y_hat_lm <- predict(train_lm, valid_subset_clean, type = "raw")
RMSE(valid_subset$Cost_claims_year, y_hat_lm)

train_lm <- train(Cost_claims_year ~ Date_last_renewal, method = "lm", data = train_subset_clean)
y_hat_lm <- predict(train_lm, valid_subset_clean, type = "raw")
RMSE(valid_subset$Cost_claims_year, y_hat_lm)

train_lm <- train(Cost_claims_year ~ Date_last_renewal +
                    R_Claims_history +
                    Type_risk +
                    Second_driver +
                    Age +
                    Distribution_channel, method = "rf", data = train_subset_clean)
y_hat_lm <- predict(train_lm, valid_subset_clean, type = "raw")
RMSE(valid_subset$Cost_claims_year, y_hat_lm)


# Full model
full_model <- lm(Cost_claims_year ~ ., data = train_subset_clean)

# Stepwise selection
step_model <- step(full_model, direction = "both", trace = FALSE)

# Summary of the final model
summary(step_model)

# Calculate RMSE
predictions <- predict(step_model, newdata = valid_subset_clean)
rmse <- sqrt(mean((predictions - valid_subset_clean$Cost_claims_year)^2))
print(rmse)

# Start timing
start_time <- Sys.time()
start_time


# Generalized Linear Model (GLM) :
train_subset_clean <- train_subset[, colSums(is.na(train_subset)) == 0] %>%
  select(-Premium, -N_claims_year)
valid_subset_clean <- valid_subset[, colSums(is.na(valid_subset)) == 0] %>%
  select(-Premium, -N_claims_year)
train_glm <- train(Cost_claims_year ~ Lapse + Payment + Policies_in_force + Second_driver + R_Claims_history, method = "glm", data = train_subset_clean)
y_hat_glm <- predict(train_glm, valid_subset, type = "raw")
RMSE(valid_subset$Cost_claims_year, y_hat_glm)
sum(valid_subset$Cost_claims_year)
sum(y_hat_glm)

# End timing
end_time <- Sys.time()
end_time
end_time - start_time


# Start timing
start_time <- Sys.time()

str(train_subset_clean)

# Generalized Linear Model (GLM):
# Define the columns you want to use
selected_columns <- c("Date_last_renewal", "R_Claims_history", "Type_risk", "N_claims_history",
                      "Area", "Second_driver", "Age", "Distribution_channel", "Value_vehicle",
                      "Driving_age", "Seniority", "Power", "Payment", "Year_matriculation",
                      "Policies_in_force")

# Function to train GLM and calculate RMSE
train_and_evaluate <- function(columns) {
  # Create the formula dynamically for the selected columns
  formula <- as.formula(paste("Cost_claims_year ~", paste(columns, collapse = " + ")))
  
  # Train the model
  train_glm <- train(formula, method = "lm", data = train_subset_clean)
  
  # Make predictions
  y_hat_glm <- predict(train_glm, valid_subset_clean, type = "raw")
  
  # Calculate RMSE
  rmse_value <- RMSE(valid_subset$Cost_claims_year, y_hat_glm)
  
  return(data.frame(Columns = paste(columns, collapse = ", "), RMSE = rmse_value))
}

# Run the function for increasing slices of selected columns and store results
results <- do.call(rbind, lapply(1:length(selected_columns), 
                                 function(i) train_and_evaluate(selected_columns[1:i])))

# Print results
print(results)

# End timing
end_time <- Sys.time()

# Calculate duration
duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
cat("Time taken:", duration, "seconds\n")

# CART :
train_rpart <- train(Cost_claims_year ~ Date_last_renewal +
                       R_Claims_history,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = train_subset)
ggplot(train_rpart)
y_hat_rpart <- predict(train_rpart, valid_subset, type = "raw")
RMSE(valid_subset$Cost_claims_year, y_hat_rpart)
sum(valid_subset$Cost_claims_year)
sum(y_hat_rpart)

# Random Forest :
nodesize <- seq(1, 51, 10)
acc <- sapply(nodesize, function(ns){
  train(Cost_claims_year ~ ., method = "rf", data = train_subset_clean,
        tuneGrid = data.frame(mtry = 2),
        nodesize = ns)$results$Accuracy
})
qplot(nodesize, acc)

train_rf_2 <- randomForest(Cost_claims_year ~ ., data = train_subset_clean,
                           nodesize = nodesize[which.max(acc)])

# Compare all models :
models <- c("glm", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
set.seed(1, sample.kind = "Rounding")
fits <- lapply(models, function(model){ 
  print(model)
  train(Cost_claims_year ~ Date_last_renewal +
          R_Claims_history, method = model, data = train_subset)
}) 
names(fits) <- models
pred <- sapply(fits, function(object) 
  predict(object, newdata = valid_subset))
dim(pred)
#acc <- colMeans(pred == valid_subset$Cost_claims_year)
#acc
#votes <- rowMeans(pred == "7")
#y_hat <- ifelse(votes > 0.5, "7", "2")
#mean(y_hat == mnist_27$test$y)

#########################################################
#         X. Following                                  #
#########################################################

# Exit the script
stop("Stopping the script.")





























































train_set
train_set %>%
  select(ID, Year, Age, Second_driver, N_claims_year, Cost_claims_year) %>%
  group_by(Year, Age) %>%
  summarize(Total_SD = sum(Second_driver, na.rm = TRUE),
            Nb_policies = n(),
            .groups = 'drop') %>%
  mutate(Year = as.factor(Year),
         SD_Policy_Ratio = Total_SD / Nb_policies) %>%
  ggplot(aes_string(x = "Age", y = "SD_Policy_Ratio", color = "Year")) +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Age", y = "SD Policy Ratio") +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")


plot_0 <- train_set %>%
  filter(Second_driver == 0) %>%
  select(ID, Year, Age, N_claims_year, Cost_claims_year) %>%
  group_by(Year, Age) %>%
  summarize(Total_cost = sum(Cost_claims_year, na.rm = TRUE),
            Nb_policies = n(),
            .groups = 'drop') %>%
  mutate(Year = as.factor(Year),
         Cost_Policy_Ratio = Total_cost / Nb_policies) %>%
  ggplot(aes_string(x = "Age", y = "Cost_Policy_Ratio", color = "Year")) +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Age", y = "Cost Policy Ratio") +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")

plot_1 <- train_set %>%
  filter(Second_driver == 1) %>%
  select(ID, Year, Age, N_claims_year, Cost_claims_year) %>%
  group_by(Year, Age) %>%
  summarize(Total_cost = sum(Cost_claims_year, na.rm = TRUE),
            Nb_policies = n(),
            .groups = 'drop') %>%
  mutate(Year = as.factor(Year),
         Cost_Policy_Ratio = Total_cost / Nb_policies) %>%
  ggplot(aes_string(x = "Age", y = "Cost_Policy_Ratio", color = "Year")) +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Age", y = "Cost Policy Ratio") +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")

plot_grid(
  plot_0,
  plot_1,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)

sd_summary <- train_set %>%
  select(ID, Year, Second_driver, N_claims_year, Cost_claims_year) %>%
  group_by(Year, Second_driver) %>%
  summarize(Total_cost = sum(Cost_claims_year, na.rm = TRUE),
            Nb_policies = n(),
            .groups = 'drop') %>%
  mutate(Year = as.factor(Year),
         Cost_Policy_Ratio = Total_cost / Nb_policies,
         Second_driver = as.character(Second_driver))

str(sd_summary)
sd_summary %>%
  ggplot(aes(x = Year, y = Cost_Policy_Ratio, fill = Second_driver)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Cost Policy Ratio") +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")


#########################################################
#         2. Claims Summary                             #
#########################################################

#########################################################
#         3. Distributions                              #
#########################################################



#########################################################
#         3. Data Analysis                              #
#########################################################




##############################



#########################################################
#         3.1 Correlation matrix                        #
#########################################################




#########################################################
#         3.2 Cost                                      #
#########################################################

# Super slow :
#insurance_data %>%
#  filter(R_Claims_history < 5 & Cost_claims_year < 10000) %>%
#  ggplot(aes(x = R_Claims_history, y = Cost_claims_year)) +
#  geom_point(color = "darkblue") +
#  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
#  labs(x = "R_Claims_history", y = "Cost_claims_year") +
#  theme_minimal() +
#  theme(text = element_text(size = 9))

#########################################################
#         3.2 Claim Policy Ratios                       #
#########################################################


# Second driver
insurance_data %>%
  select(ID, Year, Second_driver, Age) %>%
  group_by(Year, Age) %>%
  summarise(
    Second_Driver = sum(Second_driver) / n(),
    .groups = 'drop') %>%
  mutate(Year = as.factor(Year)) %>%
  ggplot(aes_string(x = "Age", y = "Second_Driver", color = "Year")) +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Age", y = "Second_Driver") +
  xlim(0, NA) +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")

#########################################################
#         3.2 Claims : previous claims                  #
#########################################################



# Risk of Raising a New Claim based on history (total claims raised)
# We only display data for 2017 (last full year)
insurance_data %>%
  group_by(Year) %>%
  select(Year, ID, N_claims_year, N_claims_history) %>%
  filter(N_claims_year > 0 & Year == 2017) %>%
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


#########################################################
#         3.5 Claim Policy Ratios / Type Risk           #
#########################################################






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