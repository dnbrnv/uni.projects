library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

# Load data from CSV file
data <- read.csv("/Users/mainprofile/Downloads/zabka_shops.csv")

str(data)

### Create a bar chart for Top 10 Cities by Number of Shops
# Count the number of shops in each city
result <- data %>%
  group_by(city) %>%
  summarize(shop_count = n()) %>%
  arrange(desc(shop_count)) %>%
  slice_head(n = 10)

# Create a bar chart
ggplot(result, aes(x = reorder(city, -shop_count), y = shop_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "City", y = "Number of Shops", title = "Top 10 Cities by Number of Shops") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Word Cloud of Shop Count by Voivodeship
library(wordcloud)
library(RColorBrewer)

# Filter out rows with missing or insufficient voivodeship data
filtered_data <- data %>%
  filter(!is.na(voivodeship) & voivodeship != "")

# Count the number of shops in each voivodeship
result <- filtered_data %>%
  group_by(voivodeship) %>%
  summarize(shop_count = n()) %>%
  arrange(desc(shop_count))

# Set a larger plot size
options(repr.plot.width = 10, repr.plot.height = 8)

# Create a word cloud with adjusted scale
wordcloud(words = result$voivodeship, freq = result$shop_count, min.freq = 1,
          scale = c(2, 1), colors = brewer.pal(8, "Dark2"),
          random.order = FALSE, random.color = TRUE,
          max.words = 100, rot.per = 0.35,
          stopwords = NULL,
          use.r.layout = FALSE)

### Bar chart for the top 10 voivodeships
# Count the number of shops in each voivodeship
result <- filtered_data %>%
  group_by(voivodeship) %>%
  summarize(shop_count = n()) %>%
  arrange(desc(shop_count))

# Select the top 10 voivodeships
top_10_voivodeships <- head(result, 10)

# Create a bar chart for the top 10 voivodeships
ggplot(top_10_voivodeships, aes(x = reorder(voivodeship, -shop_count), y = shop_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Voivodeship", y = "Number of Shops", title = "Top 10 Voivodeships by Number of Shops") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Scatter plot for latitude and longitude with colored points by voivodeship
# Remove rows with missing latitude or longitude values
data_cleaned_geo <- na.omit(data[, c('lat', 'lng', 'voivodeship')])
ggplot(data_cleaned_geo, aes(x = lng, y = lat, color = voivodeship)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude", title = "Geographical Distribution of Shops by Voivodeship") +
  theme_minimal()

### Bubble chart for Number of Shops by Region
# Count the number of shops in each region
region_counts <- data %>%
  group_by(region) %>%
  summarize(shop_count = n()) %>%
  filter(!is.na(region) & region != "" & shop_count > 0) %>%  # Remove categories with no data or unnamed regions
  arrange(desc(shop_count))

# Define the order of regions from ds1 to ds10
region_counts$region <- factor(region_counts$region, levels = paste0("DS", 1:10))

# Create a bubble chart with filtered data and adjusted y-axis scale
ggplot(region_counts, aes(x = region, y = shop_count, size = shop_count)) +
  geom_point(color = "skyblue", alpha = 0.7) +
  labs(x = "Region", y = "Number of Shops", title = "Number of Shops by Region (Bubble Chart)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(800, 1100), breaks = seq(600, 1100, by = 50)) +  # Set y-axis limits and breaks
  scale_size_continuous(range = c(3, 10))  # Adjust the range of bubble sizes

###Pie Chart for Sales Regions
data_combined <- data %>%
  # Extract the first three characters of salesRegion
  mutate(salesRegion_combined = substr(salesRegion, 1, 3)) %>%
  # Filter out rows where salesRegion_combined is blank or empty
  filter(salesRegion_combined != "") %>%
  # Group by the combined salesRegion
  group_by(salesRegion_combined) %>%
  # Summarize the counts
  summarize(count = n())

# Create a pie chart
ggplot(data_combined, aes(x = "", y = count, fill = salesRegion_combined)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Combined Sales Region Distribution (Pie Chart)")

### Time series plot for shop open times
# Convert empty strings to NA in openTime and lat columns
data$openTime[data$openTime == ""] <- NA
data$lat[data$lat == ""] <- NA

# Filter out rows with missing values in openTime or lat
filtered_data <- data[complete.cases(data$openTime, data$lat) &
                        as.POSIXct(data$openTime, format = "%H:%M") >= as.POSIXct("3:00", format = "%H:%M") &
                        as.POSIXct(data$openTime, format = "%H:%M") <= as.POSIXct("12:59", format = "%H:%M"), ]

ggplot(filtered_data, aes(x = as.POSIXct(openTime, format = "%H:%M"), y = lat, color = as.POSIXct(openTime, format = "%H:%M"))) +
  geom_point() +
  labs(x = "Time", y = "Latitude", title = "Shop Open Times and Latitude") +
  theme_minimal()

### Time series plot for shop close times
# Convert empty strings to NA in closeTime column
data$closeTime[data$closeTime == ""] <- NA

# Filter out rows with missing values in closeTime, lat, and within the specified time range
filtered_data_closeTime <- data[complete.cases(data$closeTime, data$lat) &
                                  as.POSIXct(data$closeTime, format = "%H:%M") >= as.POSIXct("17:00", format = "%H:%M") &
                                  as.POSIXct(data$closeTime, format = "%H:%M") <= as.POSIXct("23:59", format = "%H:%M"), ]

# Time series plot for shop close times
ggplot(filtered_data_closeTime, aes(x = as.POSIXct(closeTime, format = "%H:%M"), y = lat, color = as.POSIXct(closeTime, format = "%H:%M"))) +
  geom_point() +
  labs(x = "Time", y = "Latitude", title = "Shop Close Times (18:00 - 00:00) and Latitude") +
  scale_x_datetime(labels = scales::time_format("%H:%M")) +  # Display only time without date
  theme_minimal()

###Dot Plot of Service Counts
services_data <- data %>%
  separate_rows(services, sep = ",") %>%
  filter(services != "") %>%  # Remove rows with missing or empty service types
  group_by(services) %>%
  summarize(count = n()) %>%
  filter(count > 0) %>%  # Remove categories with zero counts
  arrange(desc(count))

ggplot(services_data, aes(x = reorder(services, -count), y = count)) +
  geom_point(size = 3, color = "skyblue") +
  labs(x = "Service", y = "Count", title = "Dot Plot of Service Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Heatmap
library(leaflet)
library(leaflet.extras)

# Filter rows with missing latitude and longitude values
data <- na.omit(data[, c('lat', 'lng')])

# Create an interactive heatmap using leaflet and leaflet.extras
heatmap <- leaflet(data) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~lng,
    lat = ~lat,
    radius = 10,  # Adjust radius for heatmap points
    blur = 20,   # Adjust heatmap blur
    minOpacity = 0.5  # Adjust minimum heatmap opacity
  ) %>%
  setView(lng = 19, lat = 52, zoom = 5)  # Set the map center and zoom level

# Show the interactive heatmap
heatmap