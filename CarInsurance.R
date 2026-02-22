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
    Proportion_All_With_Claims = round(sum(N_claims_year > 0) / Total_Policies * ifelse(Total_Policies > 0, 1, 0), 3),
    Proportion_New_With_Claims = round(sum(N_claims_year[Year == Year_Start] > 0) / New_Policies * ifelse(New_Policies > 0, 1, 0), 3),
    Proportion_Terminating_With_Claims = round(sum(N_claims_year[Year == Year_End] > 0, na.rm = TRUE) / Terminating_Policies * ifelse(Terminating_Policies > 0, 1, 0), 3)
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
           ggtheme = theme_dark(base_size = 9)) +
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
  summarize(Total_Cost = sum(Cost_claims_year, na.rm = TRUE),
            Total_Claims = sum(N_claims_year, na.rm = TRUE),
            .groups = "drop")

# Create the cost plot
monthly_cost <- ggplot(monthly_costs, aes(x = Year_Month, y = Total_Cost)) +
  geom_point(color = "darkblue") +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Month", y = "Total Cost") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Create the claims plot
monthly_claims <- ggplot(monthly_costs, aes(x = Year_Month, y = Total_Claims)) +
  geom_point(color = "darkblue") +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Month", y = "Total Claims") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Create the cost per claim plot
monthly_ratio <- ggplot(monthly_costs, aes(x = Year_Month, y = Total_Cost / Total_Claims)) +
  geom_point(color = "darkblue") +
  geom_smooth(se = FALSE, method = "loess", size = 1, formula = y ~ x) +
  labs(x = "Month", y = "Cost per Claim") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Timeline
plot_grid(
  monthly_cost,
  monthly_claims,
  monthly_ratio,
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
    theme(text = element_text(size = 9), legend.position = "top")
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
    theme(text = element_text(size = 9), legend.position = "top")
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

# Claim Policy Ratio compared to Type Risk
type_risk_claim_policy_ratio <-   type_risk_summary %>%
  ggplot(aes(x = Year, y = Claim_Policy_Ratio, fill = Type_risk)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Claim Policy Ratio") +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top") +
  guides(fill = guide_legend(label.theme = element_text(size = 8), nrow=2, byrow=TRUE))

# Claim Policy Ratio compared to Type Risk
type_risk_cost_policy_ratio <-   type_risk_summary %>%
  ggplot(aes(x = Year, y = Cost_Policy_Ratio, fill = Type_risk)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Cost Policy Ratio") +
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
         percentage = Cumulative_claims / lag(Cumulative_claims),
         percentage = ifelse(is.na(percentage), 100, percentage * 100)
  ) %>%
  ungroup()

# Display the resulting summary
claims_summary

# Risk of Raising a New Claim based on number of claims raised during the year
claims_summary %>% filter(Nb_claims > 0 & Cumulative_claims > 10) %>%
  ggplot(aes(x = Nb_claims, y = percentage)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Number of Previous Claims",
       y = "Risk (%)") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Risk of Raising a New Claim based on number of claims raised during the year :
# Comparison between years
claims_summary %>% filter(Nb_claims > 0 & Cumulative_claims > 10) %>%
  mutate(Year = as.factor(Year)) %>%
  ggplot(aes(x = Nb_claims, y = percentage, color = Year)) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  labs(x = "Number of Previous Claims",
       y = "Risk (%)") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Risk of Raising a New Claim based on history (total claims raised)
# We only display data for 2017 (last full year)
risk_summary <- train_set %>%
  group_by(Year) %>%
  select(Year, ID, N_claims_year, Cost_claims_year, N_claims_history, R_Claims_history)

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
#         5.3.4 Cost - Policy                           #
#########################################################

# Claim Policy Ratio compared to Area
area_summary <- insurance_data %>%
  select(ID, Year, Area, N_claims_year, Cost_claims_year) %>%
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

# Claim Policy Ratio compared to Area :
area_claim_policy_ratio <-  area_summary %>%
  ggplot(aes(x = Year, y = Claim_Policy_Ratio, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use bar plot for binary Area
  labs(x = "Year", y = "Claim Policy Ratio") +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")

# Cost Policy Ratio compared to Area :
area_cost_policy_ratio <-  area_summary %>%
  ggplot(aes(x = Year, y = Cost_Policy_Ratio, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use bar plot for binary Area
  labs(x = "Year", y = "Cost Policy Ratio") +
  ylim(0, NA) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")

# Plot the 2 graphs :
plot_grid(
  area_claim_policy_ratio,
  area_cost_policy_ratio,
  ncol = 2, align = 'hv', rel_heights = c(2, 2, 2)
)


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