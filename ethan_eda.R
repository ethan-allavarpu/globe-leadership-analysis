library(tidyverse)
library(rworldmap)
source("iso_codes.R")

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
mapCountryData(mapped_data, nameColumnToPlot = "Diplomatic",
               numCats = 26,
               colourPalette = rgb(0, 0, 0.5,
                                   alpha = seq(from = 1, to = 7,
                                               length.out = 26) / 7),
               borderCol = "black",
               missingCountryCol = rgb(0, 0, 0, alpha = 0.5),
               mapTitle = "Diplomatic Levels by Country")
mapCountryData(mapped_data,
               nameColumnToPlot = "Gender Egalitarianism Societal Practices",
               numCats = 44,
               colourPalette = rgb(0, 0.33, 0,
                                   alpha = seq(from = 1, to = 7,
                                               length.out = 44) / 7),
               borderCol = "black",
               missingCountryCol = rgb(0, 0, 0, alpha = 0.5),
               mapTitle = "Gender Egalitarianism Practices by Country")
mapCountryData(mapped_data,
               nameColumnToPlot = "Gender Egalitarianism Societal Values",
               numCats = 44,
               colourPalette = rgb(0, 0.33, 0,
                                   alpha = seq(from = 1, to = 7,
                                               length.out = 44) / 7),
               borderCol = "black",
               missingCountryCol = rgb(0, 0, 0, alpha = 0.5),
               mapTitle = "Gender Egalitarianism Values by Country")
mapCountryData(mapped_data,
               nameColumnToPlot = "Future Orientation Societal Values",
               numCats = 32,
               colourPalette = rgb(0, 0.5, 0.5,
                                   alpha = seq(from = 1, to = 7,
                                               length.out = 32) / 7),
               borderCol = "black",
               missingCountryCol = rgb(0, 0, 0, alpha = 0.5),
               mapTitle = "Future Orientation by Country")

combined_data %>%
  group_by(`Country Cluster`) %>%
  filter(`Country Cluster` != "") %>%
  ggplot() +
  geom_boxplot(aes(reorder(`Country Cluster`, Autocratic), Autocratic,
                   color = `Country Cluster`),
               show.legend = FALSE) +
  scale_color_manual(values = col_palette) +
  labs(title = "Autocratic Scores by Country Cluster",
       x = "Country Cluster", y = "Autocratic Score") +
  theme_minimal() +
  ylim(1.65, 4) +
  theme(axis.text.x = element_text(size = 8, angle = 45),
        panel.grid.major.x = element_blank())

