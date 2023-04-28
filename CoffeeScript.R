library(readxl)
library(ggplot2)
library(dplyr)
library(gganimate)
library(gifski)
library(reshape2)

# CLEANING THE DATA

# reading in the excel data as data frames

production_df <- read_excel("Coffee Production.xlsx")
consumption_df <- read_excel("Domestic consumption.xlsx")

# converting the data frames into long format

long_production_data <- melt(production_df, id.vars = "Country", variable.name = "Crop_year", value.name = "Production")
long_consumption_data <- melt(consumption_df, id.vars = "Country", variable.name = "Crop_year", value.name = "Consumption")

# Filter out rows with missing or infinite values
long_production_data <- long_production_data %>% 
  filter(Production > 0)
long_consumption_data <- long_consumption_data %>% 
  filter(Consumption > 0)


# rounding the production and consumption values to the nearest postive whole number
long_production_data$Production <- ceiling(long_production_data$Production)
long_consumption_data$Consumption <- ceiling(long_consumption_data$Consumption)



# CREATING THE ANIMATED GRAPHICS

# PRODUCTION GRAPH
# Create the coffee production base plot
plot_production <- ggplot(data = long_production_data, aes(x = Country, y = Production, fill = Crop_year)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.5) +
  transition_manual(Crop_year) +
  labs(title = "Coffee production by country") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete() +
  labs(caption = "Data source: International Coffee Organization")


# Animate the plot
animation <- animate(plot_production, nframes = 100, width = 800, height = 600, 
                     renderer = gifski_renderer())

# Save the animation as a gif file
anim_save("coffee_production.gif", animation = animation, path = "C:/Users/Dominic/Desktop/Coffee Beans")


# CONSUMPTION GRAPH
# Create the coffee consumption base plot
plot_consumption <- ggplot(data = long_consumption_data, aes(x = Country, y = Consumption, fill = Crop_year)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.5) +
  transition_manual(Crop_year) +
  labs(title = "Coffee consumption by country") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete() +
  labs(caption = "Data source: International Coffee Organization")


# Animate the plot
animation <- animate(plot_consumption, nframes = 100, width = 800, height = 600, 
                     renderer = gifski_renderer())

# Save the animation as a gif file
anim_save("coffee_consumption.gif", animation = animation, path = "C:/Users/Dominic/Desktop/Coffee Beans")


