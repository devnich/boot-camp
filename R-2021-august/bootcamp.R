library(tidyverse)

# Download dataset
download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")
surveys <- read_csv('data_raw/portal_data_joined.csv')

head(surveys)
tail(surveys)
view(surveys)

str(surveys)

#SELECT
suspect <- select(surveys, plot_id, species_id, weight)
ssubset <- select(surveys, -record_id)

survey_1995 <- filter(surveys, year == 1995)

reduced_survey <- select(filter(surveys, weight < 5),
                         species_id, sex, weight)

reduced_survey <- surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

survys_kg <- surveys %>% mutate(weight_kg = weight/1000)
mean(survys_kg$weight_kg, na.rm = TRUE)

surveys %>%
  group_by(sex, species_id) %>%
  summarize(mn=mean(weight, na.rm = TRUE),
            stdv = sd(weight, na.rm = TRUE))

surveys %>%
  count(sex, species_id) %>%
  arrange(species_id, desc(n))

# Create new data
surveys_complete <- surveys %>%
  filter(!is.na(weight),
         !is.na(hindfoot_length),
         !is.na(sex))

species_counts <- surveys_complete %>%
  count(species_id) %>% 
  filter(n >= 50)

surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)

nrow(surveys_complete)

library(ggplot2)

# Basic plotting
surveys_plot <- ggplot(data = surveys_complete,
                       mapping = aes(x = weight,
                                     y = hindfoot_length))
surveys_plot + geom_point(alpha = 0.5,
                          aes(color = species_id))
# Box plot
surveys_complete %>%
  ggplot(mapping = aes(x = species_id,
                       y = weight)) +
  geom_boxplot(outlier.color = "hot pink") +
  geom_jitter(alpha = 0.1)

# Line graphs
yearly_count <- surveys_complete %>%
  count(year, genus)
yearly_count %>%
  ggplot(aes(x = year, y = n, color = genus)) +
  geom_line() +
  theme_classic()

# facets
yearly_count %>%
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(~genus, scale = "free_y")

yearly_count_sex <- surveys_complete %>%
  count(year, genus, sex)

yearly_count_sex %>%
  ggplot(aes(x = year, y = n, color = genus)) +
  geom_line() +
  facet_grid(genus~sex, scales = "free_y")

yearly_count_sex %>%
  ggplot(aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~genus) +
  labs(title = "Observed number of individuals per year",
       x = "Year of observation",
       y = "Counts") +
  theme_bw() +
  theme(strip.background = element_blank())

ggsave(filename = "figures/fig1.png")

# put figures next to each other with patchwork
# library(patchwork)
# fig1 + fig2
# fig1 / fig2