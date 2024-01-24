library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")
str(surveys)

# select by including columns
select(surveys, plot_id, species_id, weight)

# select by excluding columns
select(surveys, -record_id, -species_id)

# filter rows
filter(surveys, year == 1995)

# piping
surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight)

surveys_sml <- select(filter(surveys, weight < 5), species_id, sex, weight)

surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

surveys %>%
  filter(year < 1995) %>%
  select(year, sex, weight)

# Manipulate columns
surveys %>%
  mutate(weight_kg = weight / 1000) %>%
  View()

surveys %>%
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2) %>%
  # relocate(weight_kg, weight_lb, .after = weight) %>%
  View()

surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1e3,
         weight_lb = weight_kg * 2.2) %>%
  View()

# split-apply-combine
surveys %>%
  group_by(sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>%
  print(n = 15)

surveys %>%
  filter(!is.na(weight), !is.na(sex)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE),
            min_weight = min(weight))

surveys %>%
  filter(!is.na(weight), !is.na(sex)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE),
            min_weight = min(weight)) %>%
  arrange(-min_weight)

# Count
surveys %>%
  count(sex)

surveys %>%
  count(sex, species) %>%
  arrange(species, desc(n))

surveys %>%
  count(plot_type)

surveys %>%
  filter(!is.na(hindfoot_length)) %>%
  group_by(species_id) %>%
  summarize(
    min_hindfoot_len = min(hindfoot_length),
    mean_hindfoot_len = mean(hindfoot_length),
    max_hindfoot_len = max(hindfoot_length),
    count = n()
  )

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(year) %>%
  filter(weight == max(weight)) %>%
  select(year, genus, species, weight) %>%
  arrange(year) %>%
  distinct(weight, .keep_all = TRUE)

# Pivoting
surveys_gw <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(plot_id, genus) %>%
  summarize(mean_weight = mean(weight))

str(surveys_gw)

surveys_wide <- surveys_gw %>%
  pivot_wider(
    id_cols = plot_id,
    names_from = genus,
    values_from = mean_weight,
    values_fill = 0
  )

surveys_long <- surveys_wide %>%
  pivot_longer(
    names_to = "genus",
    values_to = "mean_weight",
    cols = -plot_id
  )


# Challenge
# Reshape the surveys data frame with year as columns, plot_id as rows,
# and the number of genera per plot as the values.
# You will need to summarize before reshaping, and use the function n_distinct()
# to get the number of unique genera within a particular chunk of data.
# It’s a powerful function! See ?n_distinct for more.

n_genera_w <- surveys %>%
  group_by(year, plot_id) %>%
  summarize(ngenera = n_distinct(genus)) %>%
  pivot_wider(
    id_cols = plot_id,
    names_from = year,
    values_from = ngenera
  )

# Challenge
# Now take that data frame and pivot_longer() it, so each row is a unique plot_id by year combination.

n_genera_l <- n_genera_w %>%
  pivot_longer(
    names_to = "year",
    values_to = "ngenera",
    cols = -plot_id
  )

# Challenge
# The surveys data set has two measurement columns: hindfoot_length and weight.
# This makes it difficult to do things like look at the relationship between
# mean values of each measurement per year in different plot types.
# Let’s walk through a common solution for this type of problem. First, use
# pivot_longer() to create a dataset where we have a names column called
# measurement and a value column that takes on the value of either
# hindfoot_length or weight.
# Hint: You’ll need to specify which columns will be part of the reshape.

surveys_long <- surveys %>%
  pivot_longer(
    cols = c(hindfoot_length, weight),
    names_to = "measurement",
    values_to = "value"
  ) %>%
  select(plot_type, measurement, value, year) %>%
  drop_na(value)

# With this new data set, calculate the average of each measurement in each year
# for each different plot_type. Then pivot_wider() them into a data set with a
# column for hindfoot_length and weight.
# Hint: You only need to specify the names and values columns for pivot_wider().

surveys_long %>%
  group_by(year, plot_type, measurement) %>%
  summarize(mean_value = mean(value)) %>%
  pivot_wider(
    names_from = measurement,
    values_from = mean_value
    )
