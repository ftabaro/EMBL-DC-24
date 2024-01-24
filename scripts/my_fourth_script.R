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


