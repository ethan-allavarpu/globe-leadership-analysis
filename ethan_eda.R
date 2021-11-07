library(tidyverse)
library(rworldmap)
source("iso_codes.R")

leadership <- read.csv("GLOBE-Phase-2-Aggregated-Leadership-Data.csv")
society <- read.csv("GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv")
society$Country.Name[society$Country.Name == "IRAN"] <- "Iran"

combined_data <- full_join(leadership, society,
                           by = c("Country", "Country.Name"))

View(combined_data)

countries <- data.frame("country" = leadership$Country.Name)
iso_codes <- as.data.frame(iso_codes)
names(iso_codes) <- c("code", "country")
country_codes <- countries %>% left_join(iso_codes)
country_codes[is.na(country_codes$code), "code"] <- c("GBR", "CZE", "VEN",
                                                      "IRN", "RUS", "KOR",
                                                      "ZAF", "BOL", NA, "ZAF",
                                                      "CAN", "GER", "GER", "USA")


combined_data <- combined_data %>% mutate("Code" = country_codes$code)

mapped_data <- joinCountryData2Map(combined_data, nameJoinColumn = "Code")

mapCountryData(mapped_data, nameColumnToPlot = "Autocratic")


