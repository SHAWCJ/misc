# Install these packages first, if needed. E.g. install.packages("reshape2")
lapply(c('tidyverse','reshape2','magrittr'), require, character.only = TRUE)

data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
  select(-c(1,3,4)) %>% # removes columns 1,3, and 4
  filter(Country.Region != "Cruise Ship") %>% # removes cruise ship
  group_by(Country.Region) %>% summarise_all(sum) %>% #aggregates regions together
  filter(X3.10.20 > 100) # removes countries that don't reach 100 cases
colnames(data)[1] <- "Country"
data$Country %<>% as.character() # reformats to enable the next two lines:
data[data$Country == "Korea, South",1] <- "South Korea"
data[data$Country == "United Kingdom",1] <- "UK"
data$Country %<>% as.factor()
data <- data[ data[,ncol(data)] >= as.integer(data[data$Country == "UK",ncol(data)]), ]

data <- melt(data, variable_name = 'Country') %>% # reshapes as 'long' panel
  filter(value > 99) %>% select(-"variable") # removes rows with fewer than 100 cases
colnames(data)[2] <- c('Cases')

data$days = 0
for (i in levels(data$Country)) {
  data$days[data$Country == i] = seq(0, length(data$days[data$Country == i]))
} # creates "Number of days since 100 cases" variable

uk.data <- data %>% filter(Country == "UK")
interesting.data <- data %>% filter(Country %in% c("Iran","Italy","Japan","South Korea","US"))
other.data <- data %>% filter(Country %in% c("France","Germany","Netherlands","Norway","Spain","Sweden","Switzerland"))
                                    
ggplot() + 
  geom_line(data = other.data, aes(x = days, y = Cases, group = Country), col = "grey") +
  geom_line(data = interesting.data, aes(x = days, y = Cases, group = Country, col = Country)) +
  geom_line(data = uk.data, aes(x = days, y = Cases), col = "red", size = 1.1) +
  scale_y_log10() +
  xlab("Days since reaching 100 cases")
