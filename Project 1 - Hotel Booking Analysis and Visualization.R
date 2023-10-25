# Hotel Booking Analysis and Visualization

# Set working directory to location of files
getwd()
setwd("C:/Users/salex/Downloads")

# Load necessary libraries
library(ggplot2)

# ------ Data Preparation ------

# Read the dataset
hotelBk <- read.csv("HW6hotel-bookings.csv")
# Display the structure of the dataset
str(hotelBk)

# Identify and handle missing values
# Replace missing 'country' values with 'Unknown'
hotelBk$country[is.na(hotelBk$country)] <- "Unknown"

# Display number of NA values in the dataset
print(paste("Total NAs:", sum(is.na(hotelBk))))
# Display names of columns that contain NA values
na_cols <- names(hotelBk)[colSums(is.na(hotelBk)) > 0]
print(paste("Columns with NAs:", toString(na_cols)))

# Remove rows with NA values
hotelBkEd <- na.omit(hotelBk)
# Display the dimensions of the cleaned dataset
print(paste("Dimensions of cleaned data:", toString(dim(hotelBkEd))))

# Rename columns for better readability
colnames(hotelBkEd)[colnames(hotelBkEd) == "reservation_status_date"] <- "Reser-Date"

# Convert columns to appropriate data types
hotelBkEd$`Reser-Date` <- as.Date(hotelBkEd$`Reser-Date`, format = "%Y-%m-%d")
hotelBkEd$arrival_date_month <- factor(hotelBkEd$arrival_date_month, 
                                       levels = c("January", "February", "March", "April", "May", 
                                                  "June", "July", "August", "September", "October", 
                                                  "November", "December"))

# ------ Data Analysis ------

# Analyze country-related attributes
print(paste("Unique countries:", length(unique(hotelBkEd$country))))
most_bookings_country <- names(sort(table(hotelBkEd$country), decreasing = TRUE)[1])
print(paste("Country with highest bookings:", most_bookings_country))
cancellations_from_top_country <- sum(hotelBkEd$hotel == "Resort Hotel" & 
                                        hotelBkEd$country == most_bookings_country & 
                                        hotelBkEd$reservation_status == "Canceled")
print(paste("Canceled bookings from", most_bookings_country, 
            "in Resort Hotels:", cancellations_from_top_country))

# Cancellation rate for both hotel types
cancel_rate <- table(hotelBkEd$hotel, hotelBkEd$is_canceled)
cancel_rate_percent <- prop.table(cancel_rate, 1)
print(paste("Cancellation rate for City Hotel:", 
            round(cancel_rate_percent[2,2] * 100, 2), "%"))
print(paste("Cancellation rate for Resort Hotel:", 
            round(cancel_rate_percent[1,2] * 100, 2), "%"))

# Limiting the data for lead time vs cancellation to lead times < 630 days to
# avoid misleading data with outliers
hotelBkEd <- hotelBkEd[hotelBkEd$lead_time < 630,]

# Relationship between lead time and cancellations
ggplot(data = hotelBkEd, aes(x = lead_time, fill = as.factor(is_canceled))) + 
  geom_histogram(binwidth = 10, position = "fill") +
  labs(title = "Lead Time vs Cancellation", x = "Lead Time", y = "Proportion") +
  theme_minimal()

# Correcting the month abbreviations and ordering them chronologically
hotelBkEd$arrival_date_month_abb <- factor(hotelBkEd$arrival_date_month, 
                                           levels = c("January", "February", "March", "April", "May", 
                                                      "June", "July", "August", "September", "October", 
                                                      "November", "December"),
                                           labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(data = hotelBkEd, aes(x = arrival_date_month_abb)) + 
  geom_bar(aes(fill = hotel), position = "dodge") + 
  labs(title = "Bookings by Arrival Month", x = "Month", y = "Number of Bookings") +
  theme_minimal()

# Remove ADR outliers for visualization
adrIQR <- IQR(hotelBkEd$adr)
adrUpper <- quantile(hotelBkEd$adr, 0.75) + 1.5 * adrIQR
adrLower <- quantile(hotelBkEd$adr, 0.25) - 1.5 * adrIQR
hotelBkEd <- hotelBkEd[hotelBkEd$adr >= adrLower & hotelBkEd$adr <= adrUpper,]

ggplot(data = hotelBkEd, aes(x = hotel, y = adr)) + 
  geom_boxplot() + 
  labs(title = "Average Daily Rate by Hotel Type", x = "Hotel Type", y = "Average Daily Rate") +
  theme_minimal()

# Bar graph for customer types
ggplot(data = hotelBkEd, aes(x = customer_type)) + 
  geom_bar() + 
  labs(title = "Customer Type Distribution", x = "Customer Type", y = "Frequency") +
  theme_minimal()

# Distribution of stays during the weekend vs weekdays
ggplot(data = hotelBkEd, aes(x = stays_in_weekend_nights, y = stays_in_week_nights)) +
  geom_point(aes(color = hotel), alpha = 0.5) +
  labs(title = "Stays during Weekend vs Weekdays", x = "Weekend Nights", y = "Week Nights")

# Scatterplot comparing previous cancellations with non-canceled bookings
ggplot(data = hotelBkEd, aes(x = previous_cancellations, y = previous_bookings_not_canceled)) +
  geom_point(alpha = 0.5) +
  labs(title = "Previous Cancellations vs. Previous Bookings Not Canceled", 
       x = "Previous Cancellations", y = "Previous Bookings Not Canceled") +
  theme_minimal()
