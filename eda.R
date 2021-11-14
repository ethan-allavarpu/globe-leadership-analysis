# Packages ----
library(tidyverse)
library(readxl)
library(readr)
library(stringr)
library(rworldmap)
library(rvest)

# Leadership ----

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
               mapTitle = "Autocratic Levels by Country")

# Societal Values and Practices ----
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
  labs(title = 'Values vs. Practices by Cultural Dimension and Country Cluster',
       caption = 'Note: Countries with missing country clusters were excluded.',
       x = 'Societal Practices', y = 'Societal Values') +
  theme_bw() +
  facet_wrap(~ name, nrow = 3, scales = 'free') +
  theme(strip.text.x = element_text(size = 7))


# Code used for EDA not included in the report ----
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

# ---

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

# ---

leadership <- read.csv("GLOBE-Phase-2-Aggregated-Leadership-Data.csv")
social <- read.csv("GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv")

colnames(leadership) <- vapply(colnames(leadership),
                               function(x){gsub(pattern = "[\\.]+",
                                                replacement = " ", x)},
                               character(1))
colnames(social) <- vapply(colnames(social),
                           function(x){gsub(pattern = "[\\.]+",
                                            replacement = " ", x)},
                           character(1))
social$`Country Name`[social$`Country Name` == "IRAN"] <- "Iran"
data <- full_join(leadership, social,
                  by = c("Country" = "Country",
                         "Country Name" = "Country Name",
                         "Country Cluster" = "Country Cluster")
)
# Just because society considers being humane as essential,
# Doesn't mean it necessarily value humane practices
plot(data$`Human Orientation Societal Values`, # double check on this
     data$`Humane Orientation Societal Practices`,
     xlab = "Humane Orientation - Societal Values",
     ylab = "Humane Orientation - Societal Practices")
# No significant relationship between Social humane orientation
# and Leadership humane orientation
plot(data$`Human Orientation Societal Values`,
     data$`Humane oriented`,
     xlab = "Humane Orientation - Societal Values",
     ylab = "Humane Orientation - Leadership")

# BUT Social humane practice does seem to have positive relationship
# with Leadership humane orientation
plot(data$`Humane Orientation Societal Practices`,
     data$`Humane oriented`,
     xlab = "Humane Orientation - Societal Values",
     ylab = "Humane Orientation - Leadership")
cor(data$`Humane Orientation Societal Practices`, data$`Humane oriented`)
