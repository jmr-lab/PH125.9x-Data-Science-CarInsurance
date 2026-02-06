#########################################################
#         1. Load required libraries                    #
#########################################################
library(dplyr)
library(ggplot2)
#library(readr)
library(lubridate)
library(kableExtra)
library(tidyr)
library(readxl)

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

# Load the CSV file into a data frame
variables <- read_excel("data/Descriptive of the variables.xlsx")
variables




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