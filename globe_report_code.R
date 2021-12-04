# Setup ----
library(knitr)
library(MASS)
library(leaps)
library(kableExtra)
library(tidyverse)
library(readxl)
library(readr)
library(stringr)
library(rworldmap)

# Description of Dataset ----
set.seed(75)
leadership <- read.csv("GLOBE-Phase-2-Aggregated-Leadership-Data.csv")
leadership$Country.Cluster <- trimws(leadership$Country.Cluster)
society <- read.csv("GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv")
society$Country.Name[society$Country.Name == "IRAN"] <- "Iran"
society$Country.Cluster <- trimws(society$Country.Cluster)
colnames(leadership) <- vapply(colnames(leadership),
                               function(x){ gsub(pattern = "[\\.]+",
                                                 replacement = " ", x)},
                               character(1))
colnames(society) <- vapply(colnames(society),
                            function(x){ gsub(pattern = "[\\.]+",
                                              replacement = " ", x)},
                            character(1))
rename_columns <- function(x) {
  new_name <- gsub(pattern = "[\\.]+", replacement = " ", x)
  new_name <- gsub(pattern = "Originally Labeled _", replacement = "(", new_name)
  gsub(pattern = "_", replacement = ")", new_name)
}
colnames(leadership) <- vapply(colnames(leadership), rename_columns, character(1))
change_names <- str_extract_all(colnames(leadership),"(?<=[:digit:] ).*")
colnames(leadership)[vapply(change_names, length, numeric(1)) > 0] <- unlist(change_names)

# Leadership EDA ----
library(rvest)

iso_codes <- matrix(nrow = 249, ncol = 2)
site <- session("https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3")
for (i in seq_len(nrow(iso_codes))) {
  code <- site %>%
    html_nodes(paste0('#mw-content-text > div.mw-parser-output > div:nth-child(11) > div > ul > li:nth-child(',
                      i,
                      ') > span')) %>%
    html_text()
  country_name <- site %>%
    html_nodes(paste0('#mw-content-text > div.mw-parser-output > div:nth-child(11) > div > ul > li:nth-child(',
                      i,
                      ') > a')) %>%
    html_text()
  iso_codes[i, ] <- c(code, country_name[1])
}

col_palette <- rgb(c(0.50, 0.50, 0.00, 0.45, 0.00, 0.25, 0.75, 0.15, 0.32, 0.80),
                   c(0.50, 0.00, 0.50, 0.60, 0.00, 0.45, 0.45, 0.75, 0.25, 0.23),
                   c(0.00, 0.50, 0.50, 0.60, 0.15, 0.15, 0.00, 0.45, 0.70, 0.20),
                   alpha = 0.75)
leadership <- read.csv("GLOBE-Phase-2-Aggregated-Leadership-Data.csv")
leadership$Country.Cluster <- trimws(leadership$Country.Cluster)
society <- read.csv("GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv")
society$Country.Name[society$Country.Name == "IRAN"] <- "Iran"
society$Country.Cluster <- trimws(society$Country.Cluster)
combined_data <- full_join(leadership, society,
                           by = c("Country", "Country.Name", "Country.Cluster"))
countries <- data.frame("country" = leadership$Country.Name)
iso_codes <- as.data.frame(iso_codes)
names(iso_codes) <- c("code", "country")
country_codes <- countries %>% left_join(iso_codes)
country_codes[is.na(country_codes$code), "code"] <- c("GBR", "CZE", "VEN",
                                                      "IRN", "RUS", "KOR",
                                                      "ZAF", "BOL", "CHE",
                                                      "ZAF", "CAN", "DEU",
                                                      "DEU", "USA")
combined_data <- combined_data %>% mutate("Code" = country_codes$code)
colnames(combined_data) <- vapply(colnames(combined_data),
                                  function(x){ gsub(pattern = "[\\.]+",
                                                    replacement = " ", x)},
                                  character(1))
mapped_data <- joinCountryData2Map(combined_data, nameJoinColumn = "Code")
par(mai = rep(0.95, 4), xaxs = "i", yaxs = "i")
mapCountryData(mapped_data, nameColumnToPlot = "Autocratic",
               numCats = 43,
               colourPalette = rgb(0.5, 0, 0,
                                   alpha = seq(from = 1, to = 7,
                                               length.out = 43) / 7),
               borderCol = "black",
               missingCountryCol = rgb(0, 0, 0, alpha = 0.5),
               mapTitle = "Figure 1: Autocratic Levels by Country")

# Society EDA ----
leadership <- read_csv('GLOBE-Phase-2-Aggregated-Leadership-Data.csv')
culture <- read_csv('GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv')
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b',
            '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
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
change_name <- !is.na(str_extract(spv$name, "(?<=\\().+(?=\\))"))
spv$name[change_name] <- str_extract(spv$name, "(?<=\\().+(?=\\))")[change_name]
ggplot(spv) +
  geom_point(aes(practice, value, color = country_cluster),
             cex = 1.5, alpha = 0.65) +
  scale_color_manual(values = colors, name = 'Country Cluster') +
  labs(title = 'Figure 2: Values vs. Practices by Cultural Dimension\nand Country Cluster',
       caption = 'Note: Countries with missing country clusters were excluded.',
       x = 'Societal Practices', y = 'Societal Values') +
  theme_bw() +
  facet_wrap(~ name, nrow = 3, scales = 'free') +
  theme(strip.text.x = element_text(size = 7))

# Leadership Analysis ----

library(tidyverse)
library(rworldmap)
library(reshape2)

## Source ISO Codes
library(rvest)

iso_codes <- matrix(nrow = 249, ncol = 2)
site <- session("https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3")
for (i in seq_len(nrow(iso_codes))) {
  code <- site %>%
    html_nodes(paste0('#mw-content-text > div.mw-parser-output > div:nth-child(11) > div > ul > li:nth-child(',
                      i,
                      ') > span')) %>%
    html_text()
  country_name <- site %>%
    html_nodes(paste0('#mw-content-text > div.mw-parser-output > div:nth-child(11) > div > ul > li:nth-child(',
                      i,
                      ') > a')) %>%
    html_text()
  iso_codes[i, ] <- c(code, country_name[1])
}

col_palette <- rgb(c(0.50, 0.50, 0.00, 0.45, 0.00, 0.25, 0.75, 0.15, 0.32, 0.80),
                   c(0.50, 0.00, 0.50, 0.60, 0.00, 0.45, 0.45, 0.75, 0.25, 0.23),
                   c(0.00, 0.50, 0.50, 0.60, 0.15, 0.15, 0.00, 0.45, 0.70, 0.20),
                   alpha = 0.75)

leadership <- read.csv("GLOBE-Phase-2-Aggregated-Leadership-Data.csv")
leadership$Country.Cluster <- trimws(leadership$Country.Cluster)
countries <- data.frame("country" = leadership$Country.Name)
iso_codes <- as.data.frame(iso_codes)
names(iso_codes) <- c("code", "country")
country_codes <- countries %>% left_join(iso_codes)
country_codes[is.na(country_codes$code), "code"] <- c("GBR", "CZE", "VEN",
                                                      "IRN", "RUS", "KOR",
                                                      "ZAF", "BOL", "CHE",
                                                      "ZAF", "CAN", "DEU",
                                                      "DEU", "USA")
leadership <- leadership %>% mutate("Code" = country_codes$code)
rename_columns <- function(x) {
  new_name <- gsub(pattern = "[\\.]+", replacement = " ", x)
  new_name <- gsub(pattern = "Originally Labeled _", replacement = "(", new_name)
  gsub(pattern = "_", replacement = ")", new_name)
}
colnames(leadership) <- vapply(colnames(leadership), rename_columns, character(1))
leadership <- leadership %>% dplyr::select(-Country)
leadership <- leadership[, is.na(str_match(names(leadership),
                                           "Global Leadership Dimension"))]

# PCA
num_data <- leadership[, vapply(leadership, is.numeric, logical(1))]
rownames(num_data) <- leadership$`Country Name`
change_names <- str_extract_all(colnames(num_data),"(?<=[:digit:] ).*")
colnames(num_data)[vapply(change_names, length, numeric(1)) > 0] <- unlist(change_names)
pca <- prcomp(num_data)

# Cluster the leadership characteristics
pca4 <- pca$rotation[, 1:4]
set.seed(0)
w_ss <- numeric(20)
k_clust <- list()
length(k_clust) <- 20
for (k in seq_along(w_ss)) {
  k_clusters <- kmeans(pca4, centers = k, nstart = 20)
  k_clust[[k]] <- k_clusters
  w_ss[k] <- k_clusters$tot.withinss
}
## k = 5 seems best, but k = 3 provides more relevant groups
cols <- rgb(c(0.25, 0.50, 0.00), c(0.50, 0.00, 0.50), c(0.00, 0.50, 0.50),
            alpha = 0.75)[sort(k_clust[[3]]$cluster)]
names(cols) <- names(sort(k_clust[[3]]$cluster))
pca4 <- pca4[names(cols), ]
melt(pca4) %>%
  mutate(Var1 = str_replace(Var1, " (\\(.*\\))", "\n\\1")) %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = rgb(0.5, 0, 0, alpha = 1),
                       high = rgb(0, 0.5, 0, alpha = 1),
                       mid = "white",
                       midpoint = 0, space = "Lab",
                       name = "PCA Coefficient") +
  labs(title = "Figure 3: PCA Coefficients for Each Variable",
       x = "Variable", y = "Principal Component Number",
       caption = "Note: Variables colored by cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
        axis.text.y = element_text(color = cols, size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14)) +
  coord_flip()

# Use first four principal components for clustering
pca_x <- pca$x[, 1:4]
set.seed(85)
w_ss <- numeric(30)
k_clust <- list()
length(k_clust) <- 30
for (k in seq_along(w_ss)) {
  k_clusters <- kmeans(pca_x, centers = k, nstart = 20)
  k_clust[[k]] <- k_clusters
  w_ss[k] <- k_clusters$tot.withinss
}

## 4 clusters appears to be the best
best_4 <- k_clust[[4]]
leadership <- mutate(leadership, "Similar Values" = factor(best_4$cluster))

country_clusters <- matrix(nrow = max(table(best_4$cluster)), ncol = 4)
for (i in unique(best_4$cluster)) {
  country_clusters[seq_len(sum(best_4$cluster == i)), i] <-
    sort(names(best_4$cluster)[best_4$cluster == i])
}
cluster_cols <- col_palette[seq_len(4) + 3]
country_clusters[is.na(country_clusters)] <- ""
kbl(data.frame(country_clusters),
    col.names = paste("Cluster", 1:4),
    booktabs = TRUE,
    position = "ht",
    caption = "Regions with Similar Leadership Perceptions") %>%
  kable_styling(latex_options = "striped") %>%
  column_spec(1, color = cluster_cols[1]) %>%
  column_spec(2, color = cluster_cols[2]) %>%
  column_spec(3, color = cluster_cols[3]) %>%
  column_spec(4, color = cluster_cols[4])

# K-means Clusters and Leadership Values
mapped_data <- joinCountryData2Map(leadership, nameJoinColumn = "Code")
mapCountryData(mapped_data, nameColumnToPlot = "Similar Values",
               numCats = 4, catMethod = "categorical",
               colourPalette = col_palette[seq_len(4) + 3],
               borderCol = "black",
               missingCountryCol = rgb(0, 0, 0, alpha = 0.5),
               mapTitle = "Figure 4: Country Clusters with Similar Leadership Values",
               addLegend = FALSE)

characteristic_values <- sapply(num_data, FUN = function(x) {
  tapply(X = x, INDEX = leadership$`Similar Values`, FUN = median)
})
cols <- cols[colnames(characteristic_values)]
cols <- cols[apply(characteristic_values, 2, mean) %>% order()]
characteristic_values <- characteristic_values[, apply(characteristic_values,
                                                       2, mean) %>% order()]
melt(characteristic_values) %>%
  mutate(Var2 = str_replace(Var2, " (\\(.*\\))", "\n\\1")) %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = rgb(0.5, 0, 0, alpha = 1),
                       high = rgb(0, 0.5, 0, alpha = 1),
                       midpoint = 4, space = "Lab",
                       name = "Median Survey Score") +
  labs(title = "Figure 5: Median Leadership Category Scores\nby Cluster",
       x = "Cluster", y = "Leadership Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(color = col_palette[seq_len(4) + 3]),
        axis.text.y = element_text(color = cols, size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14))

# Society Analysis ----
get_spv_slr <- function(n) {
  model <- lm(practice ~ value, data = spv %>% filter(name == n))
  results <- summary(model)$coefficients['value', ]
  tibble(name = n, coefficient = unname(results['Estimate']),
         p_value = unname(results['Pr(>|t|)']))
}
slr_results <- map_df(unique(spv$name), get_spv_slr) %>%
  arrange(p_value)
alpha <- 0.05 / nrow(slr_results)

significant <- slr_results$p_value < alpha
kable(slr_results, col.names = c('Cultural Dimension', 'Coefficient Value',
                                 'p-value'), booktabs = TRUE,
      caption = 'Results of Simple Linear Regression by Cultural Dimension') %>%
  kable_styling(latex_options = "hold_position") %>%
  row_spec(which(significant), background = '#E5F5E0') %>%
  row_spec(which(!significant)[c(FALSE, TRUE)], background = '#F0F0F0')
