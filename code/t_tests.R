
library(tidyverse)
leadership <- read_csv('data/raw/GGLOBE-Phase-2-Aggregated-Leadership-Data.csv')
culture <- read_csv('data/raw/GGLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv')

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

p_values %>%
  mutate(corrected = p_value / nrow(p_values),
         name = ifelse(str_detect(name, 'Collectivism'),
                       str_extract(name, '(?<=\\()[A-z- ]{1,}'), name)) %>%
  ggplot() + geom_col(aes(reorder(name, p_value), p_value,
                          color = p_value < alpha, fill = p_value < alpha)) +
  scale_color_manual(values = c('#d62728', '#2ca02c'), name = 'Significant') +
  scale_fill_manual(values = c('#d62728', '#2ca02c'), name = 'Significant') +
  labs(title = str_c('Significance of Societal Values as Predictors of ',
                     'Societal Practices, by Cultural Dimension'),
       x = 'Cultural Dimension', y = 'p-value',
       caption = paste0("Significance Threshold: ", round(alpha, 5))) +
  geom_hline(yintercept = alpha, colour = "blue", lty = 2, show.legend = TRUE) +
  theme_bw() + coord_flip()

  # correlations ----

get_correlations <- function(n) {
  tibble(name = n,
         r = cor(spv %>% filter(name == n) %>% select(practice, value)))
}
correlations <- map_df(unique(spv$name),
                       get_correlations)[seq(from = 1, by = 2,
                                             length.out = 9), ]
correlations <- with(correlations, tibble(name, corr = r[,2]))

  # visualize correlations ----
correlations %>%
  ggplot() + geom_col(aes(reorder(name, corr), corr,
                          color = c("Positive", "Negative")[(corr < 0) + 1],
                          fill = c("Positive", "Negative")[(corr < 0) + 1])) +
  scale_color_manual(values = c('#d62728', '#2ca02c'), name = 'Correlation') +
  scale_fill_manual(values = c('#d62728', '#2ca02c'), name = 'Correlation') +
  labs(title = str_c('Correlation Between Societal Values and Practices'),
       x = 'Cultural Dimension', y = 'Correlation Coefficient') +
  geom_hline(yintercept = 0, colour = "black", lty = 2, show.legend = TRUE) +
  theme_bw() + coord_flip()
