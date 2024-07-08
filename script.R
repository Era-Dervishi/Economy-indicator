# Importing libraries
library(readr)
library(dplyr)
library(ggplot2)
library(janitor) 

# Importing the dataset
dataset <- read_csv("C:/Users/User/Desktop/Global Economy Indicators.csv")

# Initial exploration of the data
glimpse(dataset)
summary(dataset)

# Cleaning and Transforming the Data
cleaned_dataset <- dataset %>%
  clean_names() %>% 
  filter(!is.na(population), !is.na(gross_national_income_gni_in_usd), !is.na(ama_exchange_rate)) %>%
  select(population, gross_national_income_gni_in_usd, ama_exchange_rate) %>%
  arrange(population)

# Viewing the cleaned data
glimpse(cleaned_dataset)
summary(cleaned_dataset)

# Visualization of the Data
# Scatter plot to show the relationship between population and gross national income
population_income_plot <- ggplot(cleaned_dataset, aes(x = population, y = gross_national_income_gni_in_usd)) +
  geom_point(color = "blue") +
  labs(title = "Relationship between Population and Gross National Income", x = "Population", y = "Gross National Income in USD")

# Histogram to show the distribution of population
population_histogram <- ggplot(cleaned_dataset, aes(x = population)) +
  geom_histogram(binwidth = 1000000, fill = "skyblue", color = "black") +
  labs(title = "Population Histogram", x = "Population", y = "Frequency")

# Scatter plot to show the relationship between AMA exchange rate and gross national income
exchange_gni_plot <- ggplot(cleaned_dataset, aes(x = ama_exchange_rate, y = gross_national_income_gni_in_usd)) +
  geom_point(color = "darkgreen") +
  labs(title = "Relationship between AMA Exchange Rate and Gross National Income", x = "AMA Exchange Rate", y = "Gross National Income in USD")

# Displaying all the graphs
population_income_plot
population_histogram
exchange_gni_plot

# Statistical Analysis
# Correlation between population and gross national income
correlation <- cor(cleaned_dataset$population, cleaned_dataset$gross_national_income_gni_in_usd)
print(correlation)

# Linear Regression
linear_model <- lm(gross_national_income_gni_in_usd ~ population, data = cleaned_dataset)
summary(linear_model)
