library(tidyverse)
library(lubridate)  # For date manipulation
library(patchwork)  # For combining plots

# Data Wrangling
analysis_data <- data %>%
  mutate(
    # Convert date_added to proper format
    date_added = mdy(date_added),
    year_added = year(date_added),
    
    # Clean and separate duration
    duration_num = as.numeric(str_extract(duration, "\\d+")),
    duration_unit = ifelse(str_detect(duration, "Season"), "Seasons", "Minutes"),
    
    # Clean country data
    primary_country = str_split(country, ", ", simplify = TRUE)[,1],
    
    # Simplify ratings
    rating = case_when(
      rating %in% c("TV-MA", "R", "NC-17") ~ "Mature",
      rating %in% c("TV-14", "PG-13") ~ "Teen",
      rating %in% c("TV-PG", "PG") ~ "General",
      rating %in% c("TV-Y", "TV-Y7", "G") ~ "Kids",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(type))

# Create unified visualization
type_analysis <- list()

# Panel 1: Overall Distribution
type_analysis$distribution <- analysis_data %>%
  count(type) %>%
  ggplot(aes(x = type, y = n, fill = type)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#E50914", "#221F1F")) +
  labs(title = "Content Type Distribution", 
       x = "", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Panel 2: Acquisition Timeline
type_analysis$timeline <- analysis_data %>%
  filter(year_added >= 2015) %>%
  count(year_added, type) %>%
  ggplot(aes(x = year_added, y = n, color = type)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#E50914", "#221F1F")) +
  labs(title = "Content Acquisition Over Time",
       x = "Year", y = "Titles Added") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Panel 3: Rating Comparison
type_analysis$ratings <- analysis_data %>%
  count(type, rating) %>%
  ggplot(aes(x = rating, y = n, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#E50914", "#221F1F")) +
  labs(title = "Content Ratings by Type",
       x = "Rating Category", y = "Count") +
  theme_minimal() +
  coord_flip()

# Panel 4: Duration Analysis
type_analysis$duration <- analysis_data %>%
  filter(duration_num < 150) %>%  # Remove outliers
  ggplot(aes(x = duration_num, fill = type)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~duration_unit, scales = "free") +
  scale_fill_manual(values = c("#E50914", "#221F1F")) +
  labs(title = "Duration Distribution",
       x = "Duration", y = "Density") +
  theme_minimal()

# Combine all panels
(type_analysis$distribution + type_analysis$timeline) /
  (type_analysis$ratings + type_analysis$duration) +
  plot_annotation(
    title = "Comprehensive Analysis of Netflix Content Types",
    subtitle = "Movies vs TV Shows: Distribution, Trends, and Characteristics",
    theme = theme(plot.title = element_text(size = 18, face = "bold"),
                  plot.subtitle = element_text(size = 14))
  )