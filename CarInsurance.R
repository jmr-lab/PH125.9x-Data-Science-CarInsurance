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

tmp_set <- train_set %>%
  mutate(Status = if_else(year(Date_start_contract) == Year, 1, 0),
         Claim = if_else(Cost_claims_year > 0, 1, 0)) %>%
  select(where(~ !any(is.na(.)))) %>%
  select(-Premium, -N_claims_year, -Year)
# Convert specified columns to factors
factor_columns <- c("Distribution_channel", "Policies_in_force", "Payment", "Type_risk", "Area", "Second_driver", "N_doors", "Status", "Claim")
tmp_set[factor_columns] <- sapply(tmp_set[factor_columns], as.factor, simplify = FALSE)
# Convert Date_last_renewal to numeric
tmp_set$Date_last_renewal <- as.numeric(as.Date(tmp_set$Date_last_renewal))
tmp_set$Date_start_contract <- as.numeric(as.Date(tmp_set$Date_start_contract))

# Create train subset from tmp_set
# Add Status column (New or Ongoing),
# Claim to indicate 1 if there is a claim or more, 0 otherwise, 
# and remove unneeded columns :
# Premium and N_claims_year as they contain values that are related to Cost_claims_year
# and all columns containing NA values
train_subset <- tmp_set %>%
  filter(Date_last_renewal < cutoff_subset_date)
nrow(train_subset)
str(train_subset)

# Create a validation subset from tmp_set the same way :
valid_subset <- tmp_set %>%
  filter(Date_last_renewal >= cutoff_subset_date)
nrow(valid_subset)

# Delete the tmp_set :
rm(tmp_set)

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
  #  filter(Cost_claims_year <= 882) %>%
  mutate(Date_last_renewal = as.numeric(Date_last_renewal),
         Date_start_contract = as.numeric(Date_start_contract)) %>%
  mutate_if(is.factor, ~ as.numeric(.)) %>%
  #  mutate(across(where(is.numeric), z_normalize)) %>%
  mutate(across(where(is.numeric), ~ ( . - min(.) ) / ( max(.) - min(.) )))
valid_subset_dnn <- valid_subset %>%
  mutate(Date_last_renewal = as.numeric(Date_last_renewal),
         Date_start_contract = as.numeric(Date_start_contract)) %>%
  mutate_if(is.factor, ~ as.numeric(.)) %>%
  #  mutate(across(where(is.numeric), z_normalize)) %>%
  mutate(across(where(is.numeric), ~ ( . - min(.) ) / ( max(.) - min(.) )))
#str(train_subset)
str(train_subset_dnn)
str(valid_subset_dnn)

# Get the column names from train_subset
all_predictors <- colnames(train_subset)

# Exclude the specific columns
desired_predictors <- all_predictors[!all_predictors %in% c("Cost_claims_year",
                                                            "ID",
                                                            "Lapse",
                                                            "Claim")]

# Create the formula
formula <- as.formula(paste("Cost_claims_year ~", paste(desired_predictors, collapse = " + ")))
formula

# Set the seed so we get the same results each time:
set.seed(123)

# Build the neural network model
neural_network_model <- neuralnet(
  formula,
  data = train_subset_dnn,
  hidden = c(9, 8, 6, 5, 4, 3, 2, 1),
  linear.output = TRUE
  #  threshold = 0.01,
  #  rep = 5
)

# Plot the model :
plot(neural_network_model, rep = "best")

# Plot with adjusted parameters
plot(neural_network_model, 
     rep = "best",               # Use the best net if there are multiple
     maxsize = 6,              # Max size of nodes
     cex = 0.5)                # Size of text

# Save as PNG with a larger size
#png("neural_network_plot.png", width = 1200, height = 800)
#plot(neural_network_model, max.size = 10) # Adjust max.size if necessary
#dev.off()

# Create a PNG file with a larger size
#png("neural_network_plot.png", width = 1200, height = 800)

# Save as PDF
#pdf("neural_network_plot.pdf", width = 12, height = 8)
#plot(neural_network_model, 
#     rep = "best", 
#     maxsize = 10, 
#     cex = 0.75)
#dev.off() # Close the device

# Make predictions on the validation set :
y_hat <- predict(neural_network_model, valid_subset_dnn)

# Inverse normalisation for the predictions
y_hat <- y_hat * (max(train_subset$Cost_claims_year) - min(train_subset$Cost_claims_year)) + min(train_subset$Cost_claims_year)

# Update the results table :
results <- update_results(valid_subset, y_hat, "Deep Neural Network")
results

# Update the valid_subset with the predictions :
valid_subset <- valid_subset %>%
  mutate(dnn = y_hat)

#########################################################
#         6.6 Polynomial Regression                     #
#########################################################

# Linear Model with 1 parameter and polynomial regression :
# RMSE = 474.2981
# Error = 0.562
formula <- Cost_claims_year ~
  poly(Date_last_renewal, 4) + 
  poly(R_Claims_history, 9) +
  poly(Driving_age, 8)

train <- train(formula, method = "lm", data = train_subset)

# Predict the values :
y_hat <- predict(train, valid_subset, type = "raw")

# Update the results table :
results <- update_results(valid_subset, y_hat, "Polynomial Regression")
results

# Update the valid_subset with the predictions :
valid_subset <- valid_subset %>%
  mutate(lm_pr = y_hat)

# Replace values in lm_pr below the threshold with 0
#mean(valid_subset$lm_pr < 0)
#modified_lm_pr <- ifelse(valid_subset$lm_pr < threshold, 0, valid_subset$lm_pr)

#########################################################
#         6.7 Plots                                     #
#########################################################

# Calculate weekly summary data :
weekly_summary <- valid_subset %>%
  select(Date_last_renewal, Cost_claims_year, lm_1, lm_3, lm_14, dnn, lm_pr) %>%
  mutate(Date_last_renewal = as.Date(Date_last_renewal, origin = "1970-01-01"), # Adjust origin if necessary
         Week = floor_date(Date_last_renewal, unit = "week")) %>%
  group_by(Week) %>%
  summarize(
    Total_Cost = sum(Cost_claims_year, na.rm = TRUE),
    Total_lm_1 = sum(lm_1, na.rm = TRUE),
    Total_lm_3 = sum(lm_3, na.rm = TRUE),
    Total_lm_14 = sum(lm_14, na.rm = TRUE),
    Total_dnn = sum(dnn, na.rm = TRUE),
    Total_lm_pr = sum(lm_pr, na.rm = TRUE)
  )

# Plot the weekly summary :
weekly_summary_plot <- weekly_summary %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = Total_Cost, color = "Cost"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Total_lm_1, color = "Linear Model 1"), size = 1) +
#  geom_line(aes(y = Total_lm_3, color = "Linear Model 3"), size = 1) +
  geom_line(aes(y = Total_lm_14, color = "Linear Model 14"), size = 1) +
  geom_line(aes(y = Total_dnn, color = "Deep Neural Network"), size = 1) +
  geom_line(aes(y = Total_lm_pr, color = "Polynomial Regression"), size = 1) +
  labs(x = "Week", y = "Values") +
  scale_color_manual(name = "Legend", 
                     values = c("Cost" = "darkgrey",
                                "Linear Model 1" = "darkblue",
#                                "Linear Model 3" = "darkorange",
                                "Linear Model 14" = "darkgreen",
                                "Deep Neural Network" = "darkorange",
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
    err_lm_1 = lm_1 - Cost_claims_year,
    err_lm_3 = lm_3 - Cost_claims_year,
    err_lm_14 = lm_14 - Cost_claims_year,
    err_lm_pr = lm_pr - Cost_claims_year
  ) %>%
  select(err_lm_1, err_lm_3, err_lm_14, err_lm_pr) %>%
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

# Exit the script
stop("Stopping the script.")

#########################################################
#         6.6 Deep Neural Network                       #
#########################################################

str(valid_subset)
valid_subset <- valid_subset %>%
  mutate(pred = Cost_claims_year - lm_pr)

# Predict the values :
y_hat <- predict(train, train_subset, type = "raw")
train_subset <- train_subset %>%
  mutate(pred = Cost_claims_year - y_hat)

#valid_subset %>% select(Cost_claims_year, pred)

# We create 2 data frames for the deep neural network algorithm
# to convert dates to numeric and normalise values :
train_subset_dnn <- train_subset %>%
  #  filter(Cost_claims_year <= 882) %>%
  mutate(Date_last_renewal = as.numeric(Date_last_renewal),
         Date_start_contract = as.numeric(Date_start_contract)) %>%
  mutate_if(is.factor, ~ as.numeric(.)) %>%
  #  mutate(across(where(is.numeric), z_normalize)) %>%
  mutate(across(where(is.numeric), ~ ( . - min(.) ) / ( max(.) - min(.) )))
valid_subset_dnn <- valid_subset %>%
  mutate(Date_last_renewal = as.numeric(Date_last_renewal),
         Date_start_contract = as.numeric(Date_start_contract)) %>%
  mutate_if(is.factor, ~ as.numeric(.)) %>%
  #  mutate(across(where(is.numeric), z_normalize)) %>%
  mutate(across(where(is.numeric), ~ ( . - min(.) ) / ( max(.) - min(.) )))
#str(train_subset)
str(train_subset_dnn)
str(valid_subset_dnn)

# Get the column names from train_subset
all_predictors <- colnames(train_subset)

# Exclude the specific columns
desired_predictors <- all_predictors[!all_predictors %in% c("Cost_claims_year",
                                                            "ID",
                                                            "Lapse",
                                                            "Claim",
                                                            "pred",
                                                            "Date_last_renewal",
                                                            "R_Claims_history",
                                                            "Driving_age")]

# Create the formula
formula <- as.formula(paste("pred ~", paste(desired_predictors, collapse = " + ")))
formula

# Set the seed so we get the same results each time:
set.seed(123)

# Build the neural network model
neural_network_model <- neuralnet(
  formula,
  data = train_subset_dnn,
  hidden = c(9, 8, 6, 5, 4, 3, 2, 1),
  linear.output = TRUE
  #  threshold = 0.01,
  #  rep = 5
)

# Make predictions on the validation set :
y_hat <- predict(neural_network_model, valid_subset_dnn)

# Inverse normalisation for the predictions
y_hat <- y_hat * (max(train_subset$Cost_claims_year) - min(train_subset$Cost_claims_year)) + min(train_subset$Cost_claims_year)
y_hat
valid_subset$pred

min(train_subset$pred)
max(train_subset$pred)
mean(train_subset$pred)

min(valid_subset$pred)
max(valid_subset$pred)
mean(valid_subset$pred)

min(y_hat)
max(y_hat)
mean(y_hat)

valid_subset <- valid_subset %>%
  mutate(pred_final = lm_pr + y_hat)

RMSE(valid_subset$Cost_claims_year, valid_subset$pred_final)

# Exit the script
stop("Stopping the script.")

#########################################################
#         6.6 Polynomial Regression                     #
#########################################################

# Polynomial regression using three predictors. Degrees chosen
# by prior tuning: Date_last_renewal (degree 4), R_Claims_history
# (degree 9) and Driving_age (degree 8). The target is heavily
# zero-inflated, so we apply a log10(offset + target) transform
# during training and invert it for predictions.

# Formula for the linear model on transformed target :
formula <- Cost_claims_year ~
  poly(Date_last_renewal, 4) + 
  poly(R_Claims_history, 9) +
  poly(Driving_age, 8)

offsets <- seq(1000, 60000, by = 2000)

pr_sweep_offsets <- function(offset) {
  # prepare training data with log10 transform using offset
  train_log10 <- train_subset %>%
    mutate(Cost_claims_year = log10(Cost_claims_year + offset))
  
  # fit model (lm via caret::train as in your original code)
  model <- train(formula, method = "lm", data = train_log10)
  
  # predict on validation (predictions are on log10 scale)
  y_hat_log <- predict(model, valid_subset, type = "raw")
  
  # invert transform and floor negatives
  y_hat <- 10^as.numeric(y_hat_log) - offset
  y_hat <- pmax(y_hat, 0)
}

preds_offsets <- sapply(offsets, pr_sweep_offsets)

# Create a data frame to store offsets and RMSE values
metrics_df <- data.frame(offset = offsets,
                         rmse = NA,
                         error = NA,
                         accuracy = NA)

# Compute RMSE for each offset using res_pr
for (i in seq_along(offsets)) {
  # Get predictions for the current offset
  current_predictions <- preds_offsets[, i]
  
  # Calculate Metrics (RMSE, Relative Error and Accuracy) :
  metrics_df$rmse[i] <- RMSE(valid_subset$Cost_claims_year, current_predictions)
  metrics_df$error[i] <- get_error(sum(valid_subset$Cost_claims_year), sum(current_predictions))
  metrics_df$accuracy[i] <- get_accuracy(valid_subset$Cost_claims_year, current_predictions)
}

# Plot RMSE vs offset :
ggplot(metrics_df, aes(x = offset, y = rmse)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(x = "Offset", y = "RMSE", title = "RMSE vs Offset") +
  theme_minimal()

# Plot relative error vs offset :
ggplot(metrics_df, aes(x = offset, y = error)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(x = "Offset", y = "Error", title = "Error vs Offset") +
  theme_minimal()

# Plot accuracy vs offset :
ggplot(metrics_df, aes(x = offset, y = accuracy)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(x = "Offset", y = "Accuracy", title = "Accuracy vs Offset") +
  theme_minimal()


#
