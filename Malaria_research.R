setwd("/Users/m1/Downloads/Research/")

#installing packages
install.packages("readxl")
library(readxl)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("purrr")
library(purrr)
install.packages("stringi")
library(stringi)
install.packages("imputeTS")
library(imputeTS)
install.packages("data.table")
library(data.table)
install.packages("writexl")
library(writexl)
install.packages("sf")
library(sf)
install.packages("gstat")
library(gstat)
install.packages("corrr")
library(corrr)
install.packages("lubridate")
library(lubridate)
install.packages("car")
library(car)


#loading datasets and merging the files
#merging files
files <- list.files(pattern="*.xls")
merged_df <- files %>% map_dfr(read_excel)

#viewing the merged files
glimpse(merged_df)
dim(merged_df)

#splitting the period name column to two columns & deleting excess columns
#columns_split_df <- stri_split_fixed(merged_df$periodname, " ", simplify = TRUE)
merged_df <- merged_df[ , !(names(merged_df) %in% c("orgunitlevel1", "orgunitlevel2", "orgunitlevel4", "NMCP IPD Confirmed Malaria Cases", "NMCP IPD Total Malaria Deaths"))]
dates_merged_df <- separate(merged_df, col=periodname, into=c("Month", "Year"), sep=" ")
dates_merged_df <- separate(dates_merged_df, col =orgunitlevel3, into = c("District", "facility"), sep = "-" )
glimpse(dates_merged_df)
colSums(is.na(dates_merged_df))
head(dates_merged_df)
view(datesmerged_df)


#renaming the NMCP OPD Confirmed Malaria Cases to remove spaces
v1_merged_df <- dates_merged_df %>%
  rename("Incidence" = "NMCP OPD Confirmed Malaria Cases", 
         "cluster" = "organisationunitname")
glimpse(v1_merged_df)


#Plotting to explore the incidence overtime 2015-2025
hist(v1_merged_df$Incidence)

#getting only specific years from 2018 to 2024
summarized_df <- v1_merged_df %>%
  filter(Year >= 2018, Year <= 2024)
  
#checking null values
glimpse(summarized_df)

#checking missing values on OPD incidence cases
sum(is.na(summarized_df$Incidence))
dim(summarized_df)

#checking missing values per column
colSums(is.na(summarized_df))
sum(is.null(summarized_df))
summary(summarized_df)

#checking percentage of missing incidence data
(8617/63654)*100

#Data cleaning - Handling missing variables and values
#key: level 3 district, level 4 facility, orgunitname facility

#cleaning missing facilities names
summarized_df <- summarized_df %>%
  group_by(orgunitlevel3) %>%
  tidyr::fill(orgunitlevel4, .direction = "downup")
#checking if the orgunitlevel4 has been cleaned and only 98 remain
colSums(is.na(summarized_df))


#Cleaning missing Incidence column using kalman filtering 
summarized_df <- summarized_df %>% 
  group_by(District) %>%
  mutate(Incidence = round(na_kalman(Incidence)))
#checking if the incidence column has been cleaned
colSums(is.na(summarized_df))
view(summarized_df)


#importing climate data into workspace
lines <- readLines("Windspeed-2010-2025-Monthly.csv")
head(lines, 20)
head(wind_df)
#removing the beginner header string

begin_data <- grep("-END HEADER-", lines)
wind_df <- read_csv("Windspeed-2010-2025-Monthly.csv", skip = begin_data)
temp_df <-read_csv("Temperature-2010-2025-Monthly.csv", skip = begin_data)
rain_df <-read_csv("Rainfall-Monthly_2010_2025.csv", skip = begin_data)
hum_df <-read_csv("Humidity-2010-2025-Monthly.csv", skip = begin_data)
head(temp_df)
head(rain_df)
head(hum_df)
head(wind_df)


#saving malaria data as csv and Merging the climate data frames


#Saving the malaria incidence data as csv first and importing it
write.csv(summarized_df, "malaria.csv", row.names = FALSE)
malaria_df <- read_csv("malaria.csv")
glimpse(malaria_df)

#merging both climate and incidence data

#changing all columns to lower letters in malaria data
names(malaria_df) <- tolower(names(malaria_df))

#changing all names to lowercase for climate data
names(wind_df) <- tolower(names(wind_df))
names(rain_df) <- tolower(names(rain_df))
names(hum_df) <- tolower(names(hum_df))
names(temp_df) <- tolower(names(temp_df))

#filtering period of analysis for climate varibles from 2018-2025
wind_df <- wind_df %>%
  filter(year >= 2018, year <= 2024)

rain_df <- rain_df %>%
  filter(year >= 2018, year <= 2024)

hum_df <- hum_df %>%
  filter(year >= 2018, year <= 2024)

temp_df <- temp_df %>%
  filter(year >= 2018, year <= 2024)

#removing the column annual from the climate datasets
temp_df <- temp_df[ , !(names(temp_df) %in% c("ann"))]
rain_df <- rain_df[ , !(names(rain_df) %in% c("ann"))]
hum_df <- hum_df[ , !(names(hum_df) %in% c("ann"))]
wind_df <- wind_df[ , !(names(wind_df) %in% c("ann"))]


#checking column names as merging was becoming an issue and found out the header row is bringing problems
colnames(malaria_df)
colnames(wind_df)
colnames(hum_df)
colnames(rain_df)
colnames(temp_df)

#changing month to date time to plot in order
malaria_df$date <- as.Date(paste("01", malaria_df$month, malaria_df$year),
                           format = "%d %B %Y")


#merging auxiliary facilities/clusters to parent facilities
malaria_df <- malaria_df %>%
  mutate(district = case_when(
    district == "Queen Elizabeth Central Hospital" ~ "Blantyre",
    district == "Zomba Mental Hospital" ~ "Zomba",
    district == "Zomba Central Hospital" ~ "Zomba",
    district == "Nkhata" ~ "Nkhatabay",
    district == "Mzuzu Central Hospital" ~ "Mzuzu",
    TRUE ~ district #keeping all other names unchanged
  ))


unique(malaria_df$district)


#plot of overall incidence from 2018 to 2019
p <- ggplot(malaria_df, aes(x = date, y = incidence)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Malaria Incidence from 2018 - 2024",
    x = "Year",
    y = "Incidence"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("malaria_incidence_wide.png", plot = p, width = 12, height = 5, units = "in")


#adding the month and year on the x axis of the plots
ggplot(malaria_df, aes(x = date, y = incidence)) +
  geom_line(color = "steelblue", linewidth = 1) +
  facet_wrap(~ district, scales = "free_y") +
  scale_x_date(
    date_labels = "%b %Y",   # shows "Jan 2021", "Feb 2021", etc.
    date_breaks = "2 months" # control spacing between labels
  ) +
  labs(
    title = "Monthly Malaria Incidence per District",
    x = "Month and Year",
    y = "Incidence (per 1,000 population)"
  ) +
  theme_minimal(base_size = 7) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )



ggplot(malaria_df, aes(x = date, y = incidence)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ district, scales = "free_y") +
  labs(
    title = "Malaria Incidence per District",
    x = "Year",
    y = "Incidence"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

#converting climate datasets to data.table
setDT(rain_df)
setDT(wind_df)
setDT(temp_df)
setDT(hum_df)

#reshaping to long format using melt
months <- c("jan","feb","mar","apr","may","jun",
            "jul","aug","sep","oct","nov","dec")


rain_long <- melt(rain_df,
                  id.vars = c("year","lat","lon"),
                  measure.vars = months,
                  variable.name = "month",
                  value.name = "rainfall")

hum_long <- melt(hum_df,
                 id.vars = c("year","lat","lon"),
                 measure.vars = months,
                 variable.name = "month",
                 value.name = "humidity")

temp_long <- melt(temp_df,
                  id.vars = c("year","lat","lon"),
                  measure.vars = months,
                  variable.name = "month",
                  value.name = "temperature")

wind_long <- melt(wind_df,
                  id.vars = c("year","lat","lon"),
                  measure.vars = months,
                  variable.name = "month",
                  value.name = "windspeed")

#converting month names to number and creating a date column
climate_list <- list(rain_long, hum_long, temp_long, wind_long)

for(i in seq_along(climate_list)){
  climate_list[[i]][, month_num := match(tolower(month), tolower(month.abb))]
  climate_list[[i]][, date := as.Date(paste(year, month_num, "01", sep = "-"))]
}

# Assign back
rain_long <- climate_list[[1]]
hum_long  <- climate_list[[2]]
temp_long <- climate_list[[3]]
wind_long <- climate_list[[4]]

#keeping only necessary columns
rain_long <- rain_long[,  .(year, lat, lon, month, date, rainfall)]
hum_long  <- hum_long[,   .(year, lat, lon, month, date, humidity)]
temp_long <- temp_long[,  .(year, lat, lon, month, date, temperature)]
wind_long <- wind_long[,  .(year, lat, lon, month, date, windspeed)]

#merging climate datasets
climate_df <- merge(rain_long, hum_long,
                    by = c("year","lat","lon","month","date"), all = TRUE)
climate_df <- merge(climate_df, temp_long,
                    by = c("year","lat","lon","month","date"), all = TRUE)
climate_df <- merge(climate_df, wind_long,
                    by = c("year","lat","lon","month","date"), all = TRUE)
head(climate_df)


#Geolocating points in the climate data and linking it to distrits for merging with the malaria data

# Load Malawi districts shapefile

districts <- st_read("mwi_adm_nso_hotosm_20230405_shp/mwi_admbnda_adm2_nso_hotosm_20230405.shp")

# Transform to same CRS as your climate points
districts <- st_transform(districts, crs = 4326)  # WGS84


# 2. Convert climate dataframe to sf

# Replace 'climate_df_raw' with your actual dataframe
climate_sf <- st_as_sf(climate_df, coords = c("lon", "lat"), crs = 4326)


# 3. Spatial join with districts (buffer + nearest fallback)

# First, buffer districts slightly to catch edge points
malawi_buffered <- st_buffer(districts, dist = 0.01)

# Initial join using buffer
points_with_district <- st_join(climate_sf, malawi_buffered["ADM2_EN"], join = st_within)

# Handle points still NA (nearest district)
na_points <- points_with_district[is.na(points_with_district$ADM2_EN), ]
nearest_index <- st_nearest_feature(na_points, districts)
na_points$ADM2_EN <- districts$ADM2_EN[nearest_index]
points_with_district$ADM2_EN[is.na(points_with_district$ADM2_EN)] <- na_points$ADM2_EN


# 4. Clean dataframe
colnames(points_with_district)
head(points_with_district)

climate_df <- points_with_district %>%
  st_drop_geometry() %>%
  select(year, month, date, rainfall, humidity, temperature, windspeed, ADM2_EN) %>%
  rename(district = ADM2_EN)

# Check results
head(climate_df)
unique(climate_df$district)


# 5. Plot Malawi map with district labels

ggplot() +
  geom_sf(data = districts, fill = NA, color = "black") +
  geom_sf_text(data = districts, aes(label = ADM2_EN), size = 3, color = "blue") +
  theme_minimal() +
  labs(title = "Malawi Districts", caption = "Source: Malawi shapefile")



#merging climate with malaria data
setDT(malaria_df)
head(malaria_df)
head(climate_df)

#merging the climate and malaria data
# Make sure the date columns are Date type
malaria_df$date <- as.Date(malaria_df$date)
climate_df$date <- as.Date(climate_df$date)
climate_df <- climate_df %>%
  mutate(month = month.name[match(tolower(month), tolower(month.abb))])

head(climate_df)

# Join datasets by district and date
merged_df <- malaria_df %>%
  left_join(climate_df, by = c("district", "date", "year","month"))


# Checking result of merge
head(merged_df)
tail(merged_df)

#Heatmap ofincidence vs district
# Aggregate incidence by district and year (in case multiple rows per year)
malaria_agg <- malaria_df %>%
  group_by(district, year) %>%
  summarise(total_incidence = sum(incidence), na.rm = TRUE) %>%
  ungroup()

# Create heatmap
ggplot(malaria_agg, aes(x = factor(year), y = district, fill = total_incidence)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#FFF7BC", name = "Incidence") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
        ) +
  labs(
    title = "Malaria Incidence by District (2018-2024)",
    x = "Year",
    y = "District"
  )
ggsave("incidence_heatmap2.png", plot = c, width = 8, height = 12, units = "in")


# Summarize incidence per cluster/facility per district

head(malaria_df)
cluster_incidence <- malaria_df %>%
  group_by(district, cluster) %>%
  summarise(total_incidence = sum(incidence), na.rm = TRUE) %>%
  ungroup()

# Plotting cluster level incidence
ggplot(cluster_incidence, aes(x = reorder(cluster, total_incidence), y = total_incidence, fill = district)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ district, scales = "free_x") +  # one panel per district
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"  # remove legend since districts are shown in facets
  ) +
  labs(
    title = "Malaria Incidence per Cluster/Facility by District",
    x = "Cluster / Facility",
    y = "Total Incidence"
  )


#drawing a chloropleth map of malawi showing incidence oer district
malaria_agg <-  malaria_df %>%
  group_by(district, year)%>%
  summarise(total_incidence = sum(incidence, na.rm = TRUE))%>%
  ungroup()

malaria_map_data <- districts %>%
  left_join(malaria_agg, by = c("ADM2_EN" = "district"))%>%
  filter(!is.na(total_incidence)) #removing districts with NA incidence

ggplot(malaria_map_data) +
  geom_sf(aes(fill = total_incidence), color = "black", size = 0.2) +
  scale_fill_gradient(low = "#FFF7BC", high = "#D95F0E", name = "Incidence") +
  theme_minimal() +
  labs(title = "Malaria Incidence per District in Malawi (2018-2024)") +
  facet_wrap(~ year) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


head(malaria_df)


#making correlation for incidence with climate variables
# Define climate variables
climate_vars <- c("rainfall", "humidity", "temperature", "windspeed")

# Function to compute correlation and p-value safely
cor_test_safe <- function(x, y) {
  if(sum(!is.na(x) & !is.na(y)) > 1) {
    test <- cor.test(x, y, use = "complete.obs")
    tibble(correlation = test$estimate, p_value = test$p.value)
  } else {
    tibble(correlation = NA_real_, p_value = NA_real_)
  }
}

# Compute correlation and p-value per district and variable
correlation_per_district <- merged_df %>%
  group_by(district) %>%
  summarise(
    across(
      all_of(climate_vars),
      ~ list(cor_test_safe(incidence, .x)), 
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  mutate(across(all_of(climate_vars), ~ map_dfr(.x, ~.x))) %>%
  unnest(cols = everything(), names_sep = "_")


# Reshape to long format
cor_long <- correlation_per_district %>%
  pivot_longer(
    cols = matches(paste0("^(", paste(climate_vars, collapse = "|"), ")_")),
    names_to = c("climate_variable", ".value"),
    names_sep = "_"
  ) %>%
  filter(!is.na(correlation))


# Function to convert p-values to significance stars
p_to_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.1   ~ ".",
    TRUE      ~ ""
  )
}

colnames(cor_long)
cor_long <- cor_long %>%
  mutate(stars = p_to_stars(p))

# Plot heatmap with correlation and stars
ggplot(cor_long, aes(x = climate_variable, y = district, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = stars), color = "black", size = 5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Correlation of Malaria Incidence vs Climate Variables per District",
    x = "Climate Variable",
    y = "District",
    subtitle = "Significance indicated by stars: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1"
  )

#creating month lags for climate variables vs incidence
#create monthly lags 1 - 5


# 1. Define helper functions

# Safe correlation test (returns tibble even if correlation fails)
cor_test_safe <- function(x, y) {
  tryCatch({
    res <- cor.test(x, y, method = "pearson")
    tibble(correlation = unname(res$estimate),
           p_value = res$p.value)
  }, error = function(e) tibble(correlation = NA, p_value = NA))
}

# Convert p-values to significance stars
p_to_stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.1   ~ ".",
    TRUE ~ ""
  )
}


# 2. Define climate variables

climate_vars <- c("rainfall", "humidity", "temperature", "windspeed")


# 3. Create lagged climate variables (1–5 months)

lagged_df <- merged_df %>%
  group_by(district) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(across(
    all_of(climate_vars),
    list(
      lag1 = ~lag(.x, 1),
      lag2 = ~lag(.x, 2),
      lag3 = ~lag(.x, 3),
      lag4 = ~lag(.x, 4),
      lag5 = ~lag(.x, 5),
      lag6 = ~lag(.x, 6)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  ungroup()


# 4. Compute correlations for each district and lag

climate_lag_vars <- grep("rainfall|humidity|temperature|windspeed", names(lagged_df), value = TRUE)

correlation_lags <- lagged_df %>%
  group_by(district) %>%
  summarise(
    across(
      all_of(climate_lag_vars),
      ~ list(cor_test_safe(incidence, .x)), 
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  mutate(across(all_of(climate_lag_vars), ~ map_dfr(.x, ~.x))) %>%
  unnest(cols = everything(), names_sep = "_")


# 5. Reshape to long format for plotting/analysis

cor_long_lags <- correlation_lags %>%
  pivot_longer(
    cols = matches("_(lag[1-5])_"),
    names_to = c("climate_variable", "lag", ".value"),
    names_pattern = "(.*)_(lag[1-6])_(.*)"
  ) %>%
  filter(!is.na(correlation)) %>%
  mutate(stars = p_to_stars(p_value))

# Add this short line below:
cor_long_lags <- cor_long_lags %>%
  mutate(stars = ifelse(stars == "", "none", stars))


# 6. Example visualization

# Example: show correlation by lag for each climate variable in Blantyre
ggplot(cor_long_lags %>% filter(district == "Blantyre"),
       aes(x = lag, y = correlation, color = climate_variable, group = climate_variable)) +
  geom_line(size = 1) +
  geom_point(aes(shape = stars), size = 3) +
  scale_shape_manual(values = c("." = 4, "*" = 8, "**" = 15, "***" = 17, "none" = 1)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Lagged Climate–Malaria Correlations (Blantyre)",
    x = "Lag (months)",
    y = "Correlation with incidence",
    color = "Climate Variable",
    shape = "Significance"
  )


#correlation heatmaps with district facets
cor_long_lags_heat <- cor_long_lags %>%
  mutate(stars = ifelse(stars == "none", "", stars))

ggplot(cor_long_lags_heat,
       aes(x = lag, y = climate_variable, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = stars), color = "black", size = 4, fontface = "bold") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  facet_wrap(~ district, ncol = 4) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Lagged Climate–Malaria Correlation Matrices by District (1–5 Month Lags)",
    x = "Lag (months)",
    y = "Climate Variable"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold")
  )


#plotting lagged values per year
lagged_df <- lagged_df %>%
  mutate(year = year(date))

#getting correlatioms per year
correlation_lags_year <- lagged_df %>%
  group_by(district, year) %>%
  summarise(
    across(
      all_of(climate_lag_vars),
      ~ list(cor_test_safe(incidence, .x)),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  mutate(across(all_of(climate_lag_vars), ~ map_dfr(.x, ~.x))) %>%
  unnest(cols = everything(), names_sep = "_")

#reshaping to long format
cor_long_lags_year <- correlation_lags_year %>%
  pivot_longer(
    cols = matches("_(lag[1-5])_"),
    names_to = c("climate_variable", "lag", ".value"),
    names_pattern = "(.*)_(lag[1-5])_(.*)"
  ) %>%
  filter(!is.na(correlation)) %>%
  mutate(
    stars = p_to_stars(p_value),
    stars = ifelse(stars == "none", "", stars),
    lag = factor(lag, levels = paste0("lag", 1:5))
  )

#plotting facets per year
ggplot(cor_long_lags_year,
       aes(x = lag, y = climate_variable, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = stars), color = "black", size = 3, fontface = "bold") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  facet_grid(year ~ district) +  # rows = year, columns = district
  theme_minimal(base_size = 12) +
  labs(
    title = "Lagged Climate–Malaria Correlation Matrices by District and Year",
    x = "Lag (months)",
    y = "Climate Variable"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold")
  )


#trying to plot more visible facets

years <- sort(unique(cor_long_lags_year$year))

for (yr in years) {
  p <- ggplot(cor_long_lags_year %>% filter(year == yr),
              aes(x = lag, y = climate_variable, fill = correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = stars), color = "black", size = 3, fontface = "bold") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, limits = c(-1, 1), name = "Correlation") +
    facet_wrap(~ district, ncol = 4) +
    theme_minimal(base_size = 12) +
    labs(title = paste("Lagged Climate–Malaria Correlations - Year", yr),
         x = "Lag (months)",
         y = "Climate Variable") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(face = "bold"))
  
  print(p)
}


#code to autosave plots
# Get all years in your dataset
years <- sort(unique(cor_long_lags_year$year))

# Folder to save plots (adjust as needed)
output_dir <- "/Users/m1/Downloads/Research"  
dir.create(output_dir, showWarnings = FALSE)          # create folder if it doesn't exist

output_dir <- "lagged_correlations_plots"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Loop over each year and save plot
for (yr in years) {
  
  p <- ggplot(cor_long_lags_year %>% filter(year == yr),
              aes(x = lag, y = climate_variable, fill = correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = stars), color = "black", size = 3, fontface = "bold") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, limits = c(-1, 1), name = "Correlation") +
    facet_wrap(~ district, ncol = 4) +
    theme_minimal(base_size = 12) +
    labs(title = paste("Lagged Climate–Malaria Correlations - Year", yr),
         x = "Lag (months)",
         y = "Climate Variable") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(face = "bold"))
  
  # Save as PNG
  file_name <- paste0(output_dir, "/lagged_correlation_", yr, ".png")
  ggsave(file_name, plot = p, width = 20, height = 12, units = "in", dpi = 300)
}


#scatter plot of incidence vs climate variables
# Select relevant columns
plot_df <- merged_df %>%
  select(incidence, temperature, rainfall, humidity, windspeed) %>%
  pivot_longer(cols = -incidence, names_to = "climate_var", values_to = "value")

# Scatter plots
ggplot(plot_df, aes(x = value, y = incidence)) +
  geom_point(alpha = 0.3, color = "steelblue") +  # add transparency for large datasets
  geom_smooth(method = "lm", se = TRUE, color = "darkred") + # add regression line
  facet_wrap(~ climate_var, scales = "free_x") +
  labs(
    title = "Scatter Plots of Malaria Incidence vs Climate Variables",
    x = "Climate Variable Value",
    y = "Malaria Incidence"
  ) +
  theme_minimal(base_size = 14)


#checking for extreme variables
summary(merged_df$temperature)
summary(merged_df$rainfall)
summary(merged_df$humidity)
summary(merged_df$windspeed)
summary(merged_df$incidence)

#results show missing data in NA and outliers
plot_df <- merged_df %>%
  filter(!is.na(temperature), !is.na(rainfall),
         !is.na(humidity), !is.na(windspeed),
         !is.na(incidence), incidence >= 0) %>%
  pivot_longer(cols = c(temperature, rainfall, humidity, windspeed),
               names_to = "climate_var", values_to = "value") %>%
  mutate(incidence_log = log1p(incidence))  # log(incidence + 1)

# Scatter plots with regression line
ggplot(plot_df, aes(x = value, y = incidence_log)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  facet_wrap(~ climate_var, scales = "free_x") +
  labs(
    title = "Scatter Plots of Malaria Incidence vs Climate Variables",
    x = "Climate Variable Value",
    y = "Malaria Incidence"
  ) +
  theme_minimal(base_size = 14)


#Regression
lm1 <- lm(incidence ~ temperature, data = merged_df)
summary(lm1)

lm2 <- lm(incidence ~ temperature + rainfall + humidity + windspeed, data = merged_df)
summary(lm2)

lm_log <- lm(log1p(incidence) ~ temperature + rainfall + humidity + windspeed, data = merged_df)
summary(lm_log)

# Residual vs Fitted
plot(lm_log, which = 1)

# Normal Q-Q plot
plot(lm_log, which = 2)

# Scale-Location (homoscedasticity)
plot(lm_log, which = 3)

# Cook's distance for influential points
plot(lm_log, which = 4)

#multicollinearity check
vif(lm_log)


# Make a copy of your dataframe
merged_df <- merged_df %>%
  mutate(predicted_incidence = if_else(
    is.na(temperature) | is.na(rainfall) | is.na(humidity) | is.na(windspeed),
    NA_real_,  # keep NA where predictors are missing
    expm1(predict(lm_log, newdata = cur_data()))
  ))

#Key Takeaways from your model now:
#All climate variables are statistically significant after the log-transform.
#Temperature and rainfall have a negative effect on incidence.
#Humidity and windspeed have a positive effect.
#R² is still low (~1.1%), so climate alone explains a small fraction of malaria variation — which makes sense, as other factors (population, interventions, socioeconomics) likely dominate.
#No multicollinearity issues (VIF < 3 for all).







