
library(ggplot2)
library(lubridate) # Get day of week with `wday` function

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)
##
# data structure of New York City
str(ny)

head(wash)

# data structure of Washington
str(wash)

head(chi)

# data structure Chicago
str(chi)

wash$Gender <- NA
wash$Birth.Year <- NA
wash$City <- 'Washington'
ny$City <- 'New York'
chi$City <- 'Chicago'

#Creating a function for concatenation
concatenation <- function(d1, d2) {
  return(rbind(d1, d2))
}

# Concatenating all three datasets together as "city"
city <- concatenation(ny,wash)     #city <- rbind(ny, wash)
city <- concatenation(city,chi)    #city <- rbind(city, chi)
head(city)

# Re-formatting date columns
city$Start.Time <- ymd_hms(city$Start.Time)
city$End.Time <- ymd_hms(city$End.Time)

# Extracting month from Start.Time column as new 'month' column
city$Month <- month(city$Start.Time)

# Viewing values in new field
sort(table(city$Month))

# Visualizing data with ggplot
ggplot(aes(x = Month, fill = City), data = city) +
    geom_bar(position = 'dodge') +
    scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
    ggtitle('Number of Rides in a Given Month in Each City') +
    labs(y = 'Number of Rides', x = 'Months')

# Create day hour field.
city$Hour <-strftime(city$Start.Time, format="%H")

ggplot(aes(x=Hour, fill=City), data=city) +
    geom_bar(position='dodge') +
    theme(text = element_text(size = 10)) +
    ggtitle("Number of rides for each hour of the day")

# Creating new city2 by binding 'New York City' and 'Chicago' data
# Here omitting Washington data is done due to lack of information about 'Gender' and 'Birth.Year'

city2 <- concatenation(chi,ny)      #city2 <- rbind(chi, ny)

# Count of Gender (Male and Female)
total = sort(table(city2$Gender))
print(total)

# percentage of Gender (Male and Female)
round((total / length(city2$Gender) * 100), digits = 2)

# Visualizing data with ggplot
ggplot(aes(x = Gender, fill = City), data = city2) +
    geom_bar(position = 'dodge', colour="black") +
    ggtitle('Counts of each gender') +
    scale_x_discrete(labels = c('Not mentioned', 'Female', 'Male')) +
    labs(y = 'Number of Riders', x = 'Gender')

# Count of Gender(Male and Female) in Chicago
total_chi = sort(table(city2$Gender[city2$City == 'Chicago']))
print(total_chi)

# percentage of Gender(Male and Female) in Chicago
round((total_chi / length(city2$Gender[city2$City == 'Chicago']) * 100), digits = 2)

# Count of Gender(Male and Female) in New York City
total_ny = sort(table(city2$Gender[city2$City == 'New York']))
print(total_ny)

# percentage of Gender(Male and Female) in Chicago
round((total_ny / length(city2$Gender[city2$City == 'New York']) * 100), digits = 2)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
