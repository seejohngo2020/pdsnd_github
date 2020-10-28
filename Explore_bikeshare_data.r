
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')
library(ggplot2)

head(ny)

head(wash)

head(chi)

########################################
#
# a) What is the most common month?
#
########################################

# Create a dataframe with Month, Day, Day of Week 
# and Hour of Day data for Chicago
ch <-chi[!(is.na(chi$Start.Time) | chi$Start.Time==""),]
months<-as.numeric(substring(chi$Start.Time,6,7))
days <- as.numeric(substring(chi$Start.Time,9,10))
day.of.week <- weekdays(as.Date(chi$Start.Time))
hour.of.day <- as.numeric(substring(chi$Start.Time,12,13))
chiDates.df <- data.frame(months, days,day.of.week,hour.of.day , 'Chicago')
names(chiDates.df) <- c("Month", "Day", "Dow", "Hour", "City")

# Create a dataframe with Month, Day, Day of Week 
# and Hour of Day data for New York City
nyc <-ny[!(is.na(ny$Start.Time) | ny$Start.Time==""),]
months<-as.numeric(substring(nyc$Start.Time,6,7))
days <- as.numeric(substring(nyc$Start.Time,9,10))
day.of.week <- weekdays(as.Date(nyc$Start.Time))
hour.of.day <- as.numeric(substring(nyc$Start.Time,12,13))
nydates.df <- data.frame(months, days,day.of.week,hour.of.day , 'New York')
names(nydates.df) <- c("Month", "Day", "Dow", "Hour", "City")

# combine city data into one data frame
allCities.df <- rbind(chiDates.df,nydates.df)

# Create a dataframe with Month, Day, Day of Week 
# and Hour of Day data for Washington
wsh <-wash[!(is.na(wash$Start.Time) | wash$Start.Time==""),]
months<-as.numeric(substring(wsh$Start.Time,6,7))
days <- as.numeric(substring(wsh$Start.Time,9,10))
day.of.week <- weekdays(as.Date(wsh$Start.Time))
hour.of.day <- as.numeric(substring(wsh$Start.Time,12,13))
washdates.df <- data.frame(months, days,day.of.week,hour.of.day , 'Washington')
names(washdates.df) <- c("Month", "Day", "Dow", "Hour", "City")

# add Washington data to the combined city data frame
allCities.df <- rbind(allCities,washdates.df)

# prepare data for plotting
allCities.df$Month <- factor(month.abb[allCities$Month],levels=month.abb)
allCities.df$Dow <- factor(allCities$Dow,levels=c("Monday","Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(allCities.df ,aes(Month)) + geom_bar() + facet_wrap(~City) +
  ylab("Number of Trips") +
  ggtitle("Number of Trips Per Month by City") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(allCities.df ,aes(Dow)) + geom_bar() + facet_wrap(~City) +
  ylab("Number of Trips") +
  ggtitle("Number of Trips Per Day of Week by City") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(allCities.df ,aes(Hour)) + geom_bar() + facet_wrap(~City) +
    ylab("Number of Trips") +
    ggtitle("Number of Trips by Hour of the Day by City") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) 


#################################################################
#
# a) What is the total travel time for users in different cities?
#
#################################################################

# For the Chicago dataset: Check for invalid Trip.Duration values in the dataset
# and add a city column
ch <-chi[!(is.na(chi$Trip.Duration) | chi$Trip.Duration==""),]
ch['City'] = 'Chicago'


# For the New York City dataset: Check for invalid Trip.Duration values in the dataset
# and add a city column
nyc <-ny[!(is.na(ny$Trip.Duration) | ny$Trip.Duration==""),]
nyc['City'] = 'New York'


chiAndNyc <- rbind(ch, nyc)
chiAndNyc$Gender <- as.character(chiAndNyc$Gender)

# For the Washington dataset: Check for invalid Trip.Duration values in the dataset
# and add a 'Gender', 'Birth.Year' and  'City' column to the new data set. The first two 
# columns were added with placeholder data so this new dataframe could be
# added to the previously created data frame for Chicago and New York
wsh <-wash[!(is.na(wash$Trip.Duration) | wash$Trip.Duration==""),]
wsh$Trip.Duration<-as.integer(wsh$Trip.Duration)
wsh['Gender'] = 'blank1'
wsh['Birth.Year'] = as.numeric(1234)
wsh['City'] = 'Washington'


# Combine the datasets so Chicago, New York and Washington are in the same dataset
# with a new 'City' column
allCities <- rbind(wsh,chiAndNyc)

# plo the total trip times for all three cities
ggplot(allCities ,aes(City,Trip.Duration/3600)) + 
  stat_summary(geom = "bar", fun.y = "sum") +
  ylab("Total Travel Time (hours)") +
  ggtitle("Total Travel Time by City") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

# plot the average trip times for all three cities
ggplot(allCities ,aes(City,Trip.Duration/60)) + 
  stat_summary(geom = "bar", fun.y = "mean") +
  ylab("Average Travel Time (minutes)") +
  ggtitle("Average Trip Time by City") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

##########################################################################
#
# What are the counts of each user type?
#
##########################################################################

# create an empty dataframe with columns named
counts.df <- setNames(data.frame(matrix(ncol=3,nrow = 0)), c("Category","UserCount", "City"))

#
# This function loops through the dataset summing the different user types
# Inputs: empty dataframe
#         dataset with the desired city data
#         city name
# Output: data frame with the user type categories, count of each category and city 
getCustomerAndSubcriberCounts <- function(output.df,input.df, cityName){
  customerCount <- 0
  subscriberCount <- 0
  for( i in 1:nrow(input.df)){
    if (input.df$User.Type[i] == "Customer"){
      customerCount = customerCount + 1
    }else if(input.df$User.Type[i] == "Subscriber"){
      subscriberCount = subscriberCount +1
    }
  }

  output.df[nrow(output.df)+1,] <- c("Customer", as.numeric(customerCount), cityName)
  output.df[nrow(output.df)+1,] <- c("Subscriber", as.numeric(subscriberCount), cityName)

  return (output.df)
}

# call the function with the datasets for each city
# then combine the returned dataframes into a single dataframe for plotting
chiCounts.df <- getCustomerAndSubcriberCounts(counts.df, chi, 'Chicago')
nyCounts.df <- getCustomerAndSubcriberCounts(counts.df, ny, 'New York City')
wshCounts.df <- getCustomerAndSubcriberCounts(counts.df, wash, 'Washington')
counts.df <- rbind(chiCounts.df,nyCounts.df)
counts.df <-rbind(counts.df,wshCounts.df)
counts.df$Category <-factor(counts.df$Category,levels=c("Customer","Subscriber"))

ggplot(aes(x = Category, y=as.numeric(UserCount)), data = counts.df) + 
  geom_bar(width = 0.5, stat = 'identity') + 
  facet_wrap(~City) + 
  theme_light() +
  ggtitle("User Type by City") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y=element_blank()) +
  xlab("User Type") +
  ylab("User Count") +
  geom_text(aes(label=UserCount),size = 3, position = position_stack(vjust = 1.1))

##########################################################################
#
# What are the counts of each gender (only available for NYC and Chicago)?
#
##########################################################################

# For Chicago: check for invalid entries for the 'Gender' column in the dataset
# Add a column with the city
ch <-chi[!(is.na(chi$Gender) | chi$Gender==""),]
ch['City'] = 'Chicago'

# For New York: check for invalid entries for the 'Gender' column in the dataset
# Add a column with the city
nyc<-ny[!(is.na(ny$Gender) | ny$Gender==""),]
nyc['City'] = 'New York'


# Combine both dataframes into a single one for plotting
combinedCities <- rbind(ch,nyc)

ggplot(data = combinedCities, aes(x = Gender)) + geom_bar(stat="count") +
  facet_wrap(~City) +
  theme_light() +
  ggtitle("User counts for Chicago and New York by Gender ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("User Count") +
  geom_text(stat='count',aes(label=..count..),vjust =-1)


################################################################
#
# What are the earliest, most recent, most common year of birth 
# (only available for NYC and Chicago)?
#
################################################################

#
# This function loops through the dataset summing the different user types
# Inputs: dataset with the desired city data
#  
# Output: numeric with the most common birth year
#
getMostCommonBirthYear <- function(inputCity){
  birthYearRank <- data.frame(table(inputCity$Birth.Year))
  names(birthYearRank) <- c("Year","count")
  birthYearRank <- birthYearRank[order(-birthYearRank$count),]
  head(birthYearRank)

  mostCommonBirthYear <- as.character(head(birthYearRank,1)$Year)
  mostCommonBirthYear <- as.numeric(mostCommonBirthYear)
  return(mostCommonBirthYear)
}

# For Chicago: create a new dataframe with birth years greater than 1916
# calculate the lowest, highest and most common birth years
chi <- chi[with(chi, Birth.Year > 1916 ),]
maxYear <- max(chi$Birth.Year, na.rm=T)
minYear <- min(chi$Birth.Year, na.rm=T)
mostCommonBirthYear <- getMostCommonBirthYear(chi)

# For Chicago: create a new dataframe with the birth year statistics
outputStats.df <- data.frame(c("min-year", "max-year", "common-year"), 
  c(minYear, maxYear,mostCommonBirthYear))
names(outputStats.df) <- c("Category","BirthYear")
outputStats.df["City"] <-"Chicago"


# For New York: create a new dataframe with birth years greater than 1916
# calculate the lowest, highest and most common birth years
ny <- ny[with(ny, Birth.Year > 1916 ),]
maxYear <- max(ny$Birth.Year, na.rm=T)
minYear <- min(ny$Birth.Year, na.rm=T)
mostCommonBirthYear <- getMostCommonBirthYear(ny)

# For New York: create a new dataframe with the birth year statistics
nyOutputStats.df <- data.frame(c("min-year", "max-year", "common-year"), 
    c(minYear, maxYear,mostCommonBirthYear))
names(nyOutputStats.df) <- c("Category","BirthYear")
nyOutputStats.df["City"] <-"New York City"

# Combine the Chicago and New York datasets for plotting
combinedCities <- rbind(outputStats.df, nyOutputStats.df)
combinedCities$Category <-factor(combinedCities$Category,levels=c("min-year","max-year","common-year"))

ggplot(data = combinedCities, aes(x = Category, y=BirthYear)) + geom_bar(stat='identity') +
  facet_wrap(~City) +
  theme_light() +
  ggtitle("Birth Year Statistics for Chicago and New York") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Year")+
geom_text(aes(label=BirthYear),size = 3, position = position_stack(vjust = 1.03), color='blue') 

system('python -m nbconvert Explore_bikeshare_data.ipynb')
