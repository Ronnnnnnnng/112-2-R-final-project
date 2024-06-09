library(readr)
your_data <- read_csv("final/your_data.csv")
# Load required libraries
library(tidyverse)

# Assuming your_data is already loaded in your R environment
# If your_data is in a data frame format, you can skip the loading step

# Check the structure of your_data
head(your_data)

# Group data and calculate average "粗率"
grouped_data <- your_data %>%
  mutate(粗率 = `粗率 (每10萬人口)`) %>%
  group_by(癌症診斷年, 平均年齡, 癌症別) %>%
  summarise(avg_粗率 = mean(粗率, na.rm = TRUE), .groups = 'drop')

# Check the grouped data
head(grouped_data)

# Function to remove extreme outliers
remove_outliers <- function(data, variable, q = 0.99) {
  upper_bound <- quantile(data[[variable]], q, na.rm = TRUE)
  filtered_data <- filter(data, {{variable}} <= upper_bound)
  return(filtered_data)
}

# Remove extreme outliers from the data
filtered_data <- grouped_data %>%
  group_by(癌症別) %>%
  filter(avg_粗率 <= quantile(avg_粗率, 0.99, na.rm = TRUE))

# Create a list to store plots5t
plots <- list()

# Loop through each cancer type and create a plot
for(cancer_type in unique(filtered_data$癌症別)) {
  p <- filtered_data %>%
    filter(癌症別 == cancer_type) %>%
    ggplot(aes(x = 平均年齡, y = avg_粗率, color = 癌症診斷年)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(0, 100, by = 5)) +
    scale_y_continuous(breaks = seq(0, max(filtered_data$avg_粗率), by = 10)) +
    labs(title = cancer_type, x = "年齡", y = "粗率", color = "癌症診斷年") +
    theme_minimal()
  
  plots[[cancer_type]] <- p
}

# Display plots
plots
