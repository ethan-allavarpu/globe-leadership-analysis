library(tidyverse)
library(rworldmap)
library(reshape2)
source("iso_codes.R")

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
leadership <- leadership %>%
  select(-Country)
leadership <- leadership[, is.na(str_match(names(leadership),
                                           "Global Leadership Dimension"))]
num_data <- leadership[, vapply(leadership, is.numeric, logical(1))]
cor_mat <- cor(num_data)

# Visual display of correlation matrix for predictors
melted_cor_mat <- melt(cor_mat)
melted_cor_mat %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))

set.seed(1)
w_ss <- numeric(30)
k_clust <- list()
length(k_clust) <- 30
for (k in seq_along(w_ss)) {
  k_clusters <- kmeans(num_data, centers = k, nstart = 20)
  k_clust[[k]] <- k_clusters
  w_ss[k] <- k_clusters$tot.withinss
}
plot(w_ss, type = "o")

# k = 4 seems best
best_k_clusters <- k_clust[[4]]

# PCA
rownames(num_data) <- leadership$`Country Name`
pca <- prcomp(num_data)
melt(pca$rotation) %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = rgb(0, 0.5, 0, alpha = 1),
                       high = rgb(0.5, 0, 0, alpha = 1),
                       mid = "white",
                       midpoint = 0, space = "Lab",
                       name = "PCA Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_flip()
biplot(pca, cex = 0.25)
pca_sum <- summary(pca)
var_explain <- c(0, pca_sum$importance["Cumulative Proportion", ])
plot(x = seq(from = 0, by = 1, length.out = length(var_explain)), var_explain,
             type = "o",
     main = "Total Proportion of Varaince Explained",
     xlab = "Number of Principal Components")

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
plot(w_ss, type = "o")

## 4 clusters appears to be the best
best_4 <- k_clust[[4]]
leadership <- mutate(leadership, "Similar Values" = factor(best_4$cluster))

mapped_data <- joinCountryData2Map(leadership, nameJoinColumn = "Code")

par(mai = rep(0.95, 4), xaxs = "i", yaxs = "i")


# Edit the following function to create maps for different variables
# nameColumnToPlot should be the variable name
# numCats defines how many breaks you want (44 is the maximum)
# colourPalette defines the range of colors to plot
## alpha in rgb() is the transparency factor
### Edit the from and to arguments to match the variable of interest
### Divide by the maximum value of the variable of interest

# Autocratic Scores by Country and Country Cluster ----

set.seed(10)
mapCountryData(mapped_data, nameColumnToPlot = "Similar Values",
               numCats = 4, catMethod = "categorical",
               colourPalette = sample(col_palette, 4),
               borderCol = "black",
               missingCountryCol = rgb(0, 0, 0, alpha = 0.5),
               mapTitle = "Similar Leadership Values",
               addLegend = FALSE)
