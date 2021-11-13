library(tidyverse)
library(rworldmap)
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
                                                      "ZAF", "BOL", NA,
                                                      "ZAF", "CAN", "GER",
                                                      "GER", "USA")
leadership <- leadership %>% mutate("Code" = country_codes$code)
colnames(leadership) <- vapply(colnames(leadership),
                               function(x){gsub(pattern = "[\\.]+",
                                                replacement = " ", x)},
                               character(1))
leadership <- leadership[, -1]
num_data <- leadership[, vapply(leadership, is.numeric, logical(1))]
cor_mat <- cor(num_data)
kmeans(num_data, centers = 3)
mapped_data <- joinCountryData2Map(combined_data, nameJoinColumn = "Code")

par(mai = rep(0.95, 4), xaxs = "i", yaxs = "i")


# Edit the following function to create maps for different variables
# nameColumnToPlot should be the variable name
# numCats defines how many breaks you want (44 is the maximum)
# colourPalette defines the range of colors to plot
## alpha in rgb() is the transparency factor
### Edit the from and to arguments to match the variable of interest
### Divide by the maximum value of the variable of interest

# Autocratic Scores by Country and Country Cluster ----

mapCountryData(mapped_data, nameColumnToPlot = "Autocratic",
               numCats = 44,
               colourPalette = rgb(0.5, 0, 0,
                                   alpha = seq(from = 1, to = 7,
                                               length.out = 44) / 7),
               borderCol = "black",
               missingCountryCol = rgb(0, 0, 0, alpha = 0.5),
               mapTitle = "Autocratic Levels by Country")
