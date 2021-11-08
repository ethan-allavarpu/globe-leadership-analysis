
library(tidyverse)
leadership <- read_csv('GLOBE-Phase-2-Aggregated-Leadership-Data.csv')
culture <- read_csv('GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv')
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b',
            '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')

  # collectivism ----

culture %>%
  select(country = 'Country', country_name = 'Country Name',
         country_cluster = 'Country Cluster', contains('Collectivism')) %>%
  pivot_longer(contains('Collectivism')) %>%
  group_by(country, country_name, country_cluster) %>%
  summarize(collectivism = mean(value), .groups = 'drop') %>%
  filter(!is.na(country_cluster)) %>%
  ggplot() +
  geom_boxplot(aes(reorder(country_cluster, collectivism), collectivism,
                   color = country_cluster), show.legend = FALSE) +
  scale_y_continuous(breaks = seq(4, 5.5, by = 0.25), minor_breaks = NULL) +
  scale_color_manual(values = colors) +
  labs(title = 'Mean Collectivism Score by Country Cluster',
       caption = 'Note: countries with missing country clusters were excluded.',
       x = 'Country Cluster', y = 'Mean Collectivism Score') +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45),
                          panel.grid.major.x = element_blank())

  # practices v. values ----

spv <- culture %>%
  select(country = 'Country', country_name = 'Country Name',
         country_cluster = 'Country Cluster', contains('Practices')) %>%
  pivot_longer(contains('Practices'), values_to = 'practice') %>%
  mutate(name = str_replace_all(name, ' Societal Practices', '')) %>%
  full_join(culture %>%
              select(country = 'Country', country_name = 'Country Name',
                     country_cluster = 'Country Cluster', contains('Values')) %>%
              pivot_longer(contains('Values')) %>%
              mutate(name = str_replace_all(name, ' Societal Values', ''),
                     name = str_replace(name, 'Human', 'Humane')),
            by = c('country', 'country_name', 'country_cluster', 'name')) %>%
  filter(!is.na(country_cluster))

ggplot(spv) +
  geom_point(aes(practice, value, color = country_cluster),
             cex = 2.3, alpha = 0.65) +
  scale_color_manual(values = colors, name = 'Country Cluster') +
  labs(title = 'Values v. Practices by Cultural Dimension and Country Cluster',
       caption = 'Note: countries with missing country clusters were excluded.',
       x = 'Societal Practices', y = 'Societal Values') +
  theme_bw() +
  facet_wrap(~ name, nrow = 3, scales = 'free')

  # team v. charismatic ----

tc <- leadership %>%
  select(country = 'Country', country_name = 'Country Name',
         country_cluster = 'Country Cluster', matches('Team [0-9]')) %>%
  pivot_longer(contains('Team')) %>%
  group_by(country, country_name, country_cluster) %>%
  summarize(team = mean(value), .groups = 'drop') %>%
  full_join(leadership %>%
              select(country = 'Country', country_name = 'Country Name',
                     country_cluster = 'Country Cluster',
                     matches('Charismatic [0-9]')) %>%
              pivot_longer(contains('Charismatic')) %>%
              group_by(country, country_name, country_cluster) %>%
              summarize(charismatic = mean(value), .groups = 'drop'),
            by = c('country', 'country_name', 'country_cluster')) %>%
  filter(!is.na(country_cluster))
  
ggplot(tc) +
  geom_point(aes(team, charismatic, color = country_cluster),
             cex = 2.3, alpha = 0.65) +
  scale_color_manual(values = colors, name = 'Country Cluster') +
  labs(title = 'Mean Charismatic v. Team Scores by Country Cluster',
       caption = 'Note: countries with missing country clusters were excluded.',
       x = 'Mean Team Score', y = 'Mean Charismatic Score') +
  theme_bw() + coord_fixed()
