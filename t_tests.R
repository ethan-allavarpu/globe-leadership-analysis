
library(tidyverse)
leadership <- read_csv('GLOBE-Phase-2-Aggregated-Leadership-Data.csv')
culture <- read_csv('GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv')

  # data preparation ----

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
            by = c('country', 'country_name', 'country_cluster', 'name'))

  # calculate p-values ----

get_t_test_p_values <- function(n) {
  model <- lm(practice ~ value, data = spv %>% filter(name == n))
  p <- summary(model)$coefficients['value', 'Pr(>|t|)']
  tibble(name = n, p_value = p)
}
p_values <- map_df(unique(spv$name), get_t_test_p_values)

alpha <- 0.05 / nrow(p_values)

  # visualize results ----

ggplot(p_values) +
  geom_col(aes(reorder(name, p_value), p_value,
               color = p_value < alpha, fill = p_value < alpha)) +
  scale_color_manual(values = c('#d62728', '#2ca02c'), name = 'Significant') +
  scale_fill_manual(values = c('#d62728', '#2ca02c'), name = 'Significant') +
  labs(title = 'Significance of Values in Modeling Practices',
       x = 'Cultural Dimension', y = 'p-value') +
  theme_bw() + coord_flip()
