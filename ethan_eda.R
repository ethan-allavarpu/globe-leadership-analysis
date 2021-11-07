library(tidyverse)
library(rworldmap)
source("iso_codes.R")

leadership <- read.csv("GLOBE-Phase-2-Aggregated-Leadership-Data.csv")
society <- read.csv("GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv")
society$Country.Name[society$Country.Name == "IRAN"] <- "Iran"

combined_data <- full_join(leadership, society,
                           by = c("Country", "Country.Name", "Country.Cluster"))

countries <- data.frame("country" = leadership$Country.Name)
iso_codes <- as.data.frame(iso_codes)
names(iso_codes) <- c("code", "country")
country_codes <- countries %>% left_join(iso_codes)
country_codes[is.na(country_codes$code), "code"] <- c("GBR", "CZE", "VEN",
                                                      "IRN", "RUS", "KOR",
                                                      "ZAF", "BOL", NA,
                                                      "ZAF", "CAN", "GER",
                                                      "GER", "USA")


combined_data <- combined_data %>% mutate("Code" = country_codes$code)
mapped_data <- joinCountryData2Map(combined_data, nameJoinColumn = "Code")

par(mai = rep(0.85, 4), xaxs = "i", yaxs = "i")


# Edit the following function to create maps for different variables
# nameColumnToPlot should be the variable name
# numCats defines how many breaks you want (44 is the maximum)
# colourPalette defines the range of colors to plot
## alpha in rgb() is the transparency factor
### Edit the from and to arguments to match the variable of interest
### Divide by the maximum value of the variable of interest

mapCountryData(mapped_data, nameColumnToPlot = "Autocratic",
               numCats = 44,
               colourPalette = rgb(0.5, 0, 0,
                                   alpha = seq(from = 1.89, to = 3.86,
                                               length.out = 44) / 3.86),
               borderCol = "black",
               missingCountryCol = rgb(0, 0, 0, alpha = 0.5),
               mapTitle = "Autocratic Levels by Country")

combined_data %>%
  group_by(Country.Cluster) %>%
  summarize(avg_autocratic = mean(Autocratic),
            med_autocratic = median(Autocratic)) %>%
  arrange(desc(avg_autocratic))

boxplot(Autocratic ~ Country.Cluster, data = combined_data,
        cex.axis = 0.5, las = 2)


