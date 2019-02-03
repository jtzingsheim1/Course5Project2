# Coursera Data Science Specialization Course 5 Project 2 Script


# The purpose of this script is to satisfy the requirements of the project 2
# peer-graded assignment that is part of the Reproducible Research course
# within the Data Science Specialization on Coursera. As per the instructions
# the document must use the NOAA Storm Database to determine which types of
# events are most harmful to population health and which have the greatest
# economic consequences.
#
# The input for this document is the data set which can be obtained from the url
# below. The code will download the data file into the current working
# directory. The output will be a report document that makes use of the code,
# objects, and plots created in this script.


library(dplyr)


## Part 0) Loading the data

# Check if the file exists in the current directory before downloading it again
file.name <- "ProjectData.csv.bz2"

if (!file.exists(file.name)) {
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(url, file.name)
  rm(url)
}

# Read in data
# The original dataset is 902297 observations of 37 varibles, which if loaded is
# an object more than 400 Mb. Many of the variables are not needed to address
# the questions in this assingment, so they will be excluded to make this data
# easier to work with. First a vector of the colClasses is created which will
# be used in read.csv below to specify which columns to read in.
keep.cols <- c("NULL", "character", rep("NULL", 5), "factor", rep("NULL", 14),
               rep("numeric", 3), "factor", "numeric", "factor", rep("NULL", 9))
data <- read.csv(file.name, colClasses = keep.cols)  # 902297 obs of 8 variables

rm(file.name, keep.cols)


# Part 1) Clean and preprocess the data, each of the 8 variables are addressed

# First extract the date from the BGN_DATE variable
data <- as_tibble(data) %>%
  mutate(date = as.POSIXct(BGN_DATE, format = "%m/%d/%Y %H:%M:%S")) %>%
  mutate(date = as.Date(date)) %>%  # Change from a POSIXct to a date data type
  select(-BGN_DATE)  # 902297 obs of 8 variables

# Next clean up the event types. According to the documentation from the
# National Weather Service there are 48 official event types. Checking the
# actual data shows 985 unique event types in the database, and these are due to
# inconsistencies in how the event types are written. Ideally the event types
# would be mapped to the correct intended event type, and this will be partially
# completed in the steps below.

# Create a reference table of the actual event types from the documentation. A
# "key" variable will be used to simplify the mapping in the next step, and a
# 49th event type will be added to capture the "Unmapped" EVTYPES.
actual.evtypes <- as_tibble(seq(1:49)) %>%  # A numerical sequence for the "key"
  rename(key = value) %>%  # 49 obs of 1 variable
  mutate(event.type = c("Astronomical Low Tide", "Avalanche", "Blizzard",
                        "Coastal Flood", "Cold/Wind Chill", "Debris Flow",
                        "Dense Fog", "Dense Smoke", "Drought", "Dust Devil",
                        "Dust Storm", "Excessive Heat",
                        "Extreme Cold/Wind Chill", "Flash Flood", "Flood",
                        "Frost/Freeze", "Funnel Cloud", "Freezing Fog", "Hail",
                        "Heat", "Heavy Rain", "Heavy Snow", "High Surf",
                        "High Wind", "Hurricane (Typhoon)", "Ice Storm",
                        "Lake-Effect Snow", "Lakeshore Flood", "Lightning",
                        "Marine Hail", "Marine High Wind", "Marine Strong Wind",
                        "Marine Thunderstorm Wind", "Rip Current", "Seiche",
                        "Sleet", "Storm Surge/Tide", "Strong Wind",
                        "Thunderstorm Wind", "Tornado", "Tropical Depression",
                        "Tropical Storm", "Tsunami", "Volcanic Ash",
                        "Waterspout", "Wildfire", "Winter Storm",
                        "Winter Weather", "Unmapped"))  # From NWS document

# Now cross reference the observed event types to the actual event types. The
# event types are sorted by decreasing count and then manually coded with the
# "key" variable, types that are not manually coded will be lumped into a
# standalone category "Unmapped". After mapping the first 175 manually it was
# discovered that only 1650 (or 0.18%) total events remained unmapped. In this
# way all of the data rows are still preserved, but at the expense of a small
# fraction of events being obscured into the "Unmapped" event type.
observed.evtypes <- data %>%
  group_by(EVTYPE) %>%  # 902297 obs of 8 variables
  summarize(count = n()) %>%  # 985 obs of 2 variables
  arrange(desc(count)) %>%
  mutate(key = c(19, 39, 39, 40, 14, 15, 39, 24, 29, 22, 21, 47, 47, 17, 33, 33,
                 45, 24, 14, 46, 3, 9, 26, 12, 24, 46, 16, 7, 48, 19, 13, 20,
                 23, 42, 14, 13, 4, 27, 14, 6, 22, 5, 7, 34, 30, 11, 2, 24, 34,
                 37, 16, 14, 23, 13, 24, 39, 1, 25, 14, 48, 37, 12, 4, 10, 31,
                 12, 15, 4, 48, 14, 48, 25, 17, 23, 12, 16, 12, 13, 13, 16, 30,
                 41, 36, 9, 16, 24, 39, 32, 49, 19, 17, 18, 39, 12, 39, 4, 45,
                 21, 24, 21, 48, 14, 48, 22, 22, 14, 19, 16, 24, 21, 22, 27, 14,
                 39, 24, 28, 16, 39, 13, 12, 14, 44, 27, 48, 35, 16, 4, 43, 13,
                 22, 40, 21, 48, 19, 13, 13, 22, 26, 21, 21, 22, 16, 21, 39, 12,
                 19, 19, 39, 12, 47, 36, 13, 24, 16, 21, 13, 8, 48, 25, 13, 8,
                 24, 36, 36, 39, 12, 12, 45, 9, 48, 36, 21, 23, 25, 6,
                 rep(49, 810))) %>%  # Manually mapped 175 types, 985 obs of 3
  select(-count) %>%  # EVTYPES now mapped to keys, 985 obs of 2 variables
  inner_join(actual.evtypes, by = "key") %>%  # Add actual evtypes for each key
  select(-key)  # Drop the key variable, now it is a table mapping EVTYPES

# Lastly replace the original EVTYPES with the manually mapped ones
data <- data %>%  # 902297 obs of 8 variables
  inner_join(observed.evtypes, by = "EVTYPE") %>%  # Add column of mapped evtype
  select(-EVTYPE) %>%  # Drop column of original EVTYPE
  mutate(event.type = as.factor(event.type))  # Change from chr to factor var

rm(actual.evtypes, observed.evtypes)

# Next turn fatalities and injuries back into integers, read.csv would not load
# them as integers originally. This reduces the object size in RAM.
data <- data %>%
  mutate(fatalities = as.integer(FATALITIES)) %>%  # Add column for integer vals
  select(-FATALITIES) %>%  # Drop original column
  mutate(injuries = as.integer(INJURIES)) %>%  # Add column for integer values
  select(-INJURIES)  # Drop original column

# Next the PROPDMG and PROPDMGEXP columns will be combined to create a single
# value that represents the amount of property damage. In order to achieve this
# the PROPDMGEXP values must be handled correctly. The article at the URL below
# was used as a guide in handling this variable. Near the end of the article one
# can see that for numeric values of PROPDMGEXP the value itself should be
# used as the ones digit of the final result. For simplicity the approach
# outlined at the beginning of the report was employed instead. Due to the small
# sample size and small magnitude of these substitutions the impact of this
# simplified technique is expected to be small. These steps will be repeated for
# the CROPDMG and CROPDMGEXP columns as well.
# https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html

# First create a reference table for the PROPDMGEXP values and multipliers
prop.ref <- as_tibble(levels(data$PROPDMGEXP)) %>%  # 19 obs of 1 variable, chr
  mutate(PROPDMGEXP = as.factor(value)) %>%  # Add a column converting to factor
  select(-value) %>%  # Drop the original column of chr type, with default name
  arrange(PROPDMGEXP) %>%  # Ensure rows are sorted correctly
  mutate(prop.multiplier = c(0, 0, 0, 1, rep(10, 9), 1000000000, 100, 100, 1000,
                             rep(1000000, 2)))  # Manually map the multipliers

# Next create a reference table for the CROPDMGEXP values and multipliers
crop.ref <- as_tibble(levels(data$CROPDMGEXP)) %>%  # 9 obs of 1 variable
  mutate(CROPDMGEXP = as.factor(value)) %>%  # 9 obs of 2 variables
  select(-value) %>%  # 9 obs of 1 variable
  arrange(CROPDMGEXP) %>%  # Ensure rows are sorted correctly
  mutate(crop.multiplier = c(0, 0, 10, 10, 1000000000, 1000, 1000,
                             rep(1000000, 2)))  # 9 obs of 2 variables

# Replace the PROPDMG variables
data <- data %>%  # 902927 obs of 8 variables
  inner_join(prop.ref, by = "PROPDMGEXP") %>%  # Add column with the multiplier
  mutate(prop.dmg = PROPDMG * prop.multiplier) %>%  # Add column for the damage
  select(-c(PROPDMG, PROPDMGEXP, prop.multiplier))  # Drop the unneeded columns

# Replace the CROPDMG variables
data <- data %>%  # 902927 obs of 7 variables
  inner_join(crop.ref, by = "CROPDMGEXP") %>%  # 903927 obs of 8 variables
  mutate(crop.dmg = CROPDMG * crop.multiplier) %>%  # 903927 obs of 9 variables
  select(-c(CROPDMG, CROPDMGEXP, crop.multiplier))  # 903927 obs of 6 variables

rm(prop.ref, crop.ref)


# Part 2) Explore the data

# First look at how data was collected over the years 
table.0 <- data %>%
  mutate(year = format(date, "%Y")) %>%  # Extract the year from the date
  group_by(year, event.type) %>%
  summarize %>%  # Summarize the data by unique event types in each year
  group_by(year) %>%  # Drop the event.type group
  summarize(count = n())  # Count the number of unique events in each year

# Plot the number of unique events in each year
plot(table.0$year, table.0$count, type = "l", xlab = "Year",
     ylab = "Number of Unique Event Types",
     main = "Figure 1: Number of Unique Event Types in Each Year")
# By viewing the plot above, one can see that in the 62 year history of the data
# that in the first 43 years there were three or less event types every year. A
# likely explanation for this is that the other categories of event types were
# not captured during this time. If viewed in aggregate this fact could skew the
# analysis by giving too much weight to these three event types. For the fairest
# comparison of the event types the first 43 years of data should be ignored.

# Drop the first 43 years of data
data <- data %>%  # 902297 obs of 6 variables
  filter(date > "1992-12-31") %>%  # 714738 obs of 6 variables
  arrange(date)

# Part 3) First Question: Across the US which types of events are most harmful
# with respect to population health?

# In the instructions the term "harmful" is left to interpretation - it could
# mean most fatalities, most injuries, or a blended combination of both. Since
# injuries are roughly 10x more numerous, combining the two variables would
# just obscure the fatality data. The fatality and injury data will be presented
# side-by-side.

# Create a table of the most fatal event types, sorted by total fatalities
table.1a <- data %>%  # 714738 obs of 6 variables
  group_by(event.type) %>%
  summarize(total.fatalities = sum(fatalities) / 1000) %>%  # Scale to 1000's
  arrange(desc(total.fatalities)) %>%  # Sort by descending fatalities
  slice(1:7)  # Subset only the first seven rows, the most harmful events

# Create a table of the most injurious event types, sorted by total injuries
table.1b <- data %>%  # 714738 obs of 6 variables
  group_by(event.type) %>%
  summarize(total.injuries = sum(injuries) / 1000) %>%  # 48 obs of 2 variables
  arrange(desc(total.injuries)) %>%  # Sort by descending injuries
  slice(1:7)  # 7 obs of 2 variables

# Create a figure that displays both the fatality and injury plots
par(mar = c(9.1, 4.1, 4.1, 2.1), mfrow = c(1, 2), las = 2)  # Setup plot space

# Add the plot of the fatality data
barplot(table.1a$total.fatalities, names.arg = table.1a$event.type,
        ylab = "Total Fatalities (thousands)", main = "Most Fatal Event Types")

# Add the plot of the injury data
barplot(table.1b$total.injuries, names.arg = table.1b$event.type,
        ylab = "Total Injuries (thousands)",
        main = "Most Injurious Event Types")


# Part 4) Second Question: Across the US which types of events have the greatest
# economic consequences?

# Due to the different scales of the data, combining the varibles would obscure
# the crop damage data. Instea, the property damage and crop damage data will be
# presented side-by-side.

# Create a table of the event types that are most damaging to property
table.2a <- data %>%  # 714738 obs of 6 variables
  group_by(event.type) %>%
  summarize(total.prop.dmg = sum(prop.dmg) / 1000000000) %>%  # Scale to billion
  arrange(desc(total.prop.dmg)) %>%
  slice(1:7)  # Subset only the first seven rows, the most damaging event types

# Create a table of the event types that are most damaging to crops
table.2b <- data %>%  # 714738 obs of 6 variables
  group_by(event.type) %>%
  summarize(total.crop.dmg = sum(crop.dmg) / 1000000000) %>%  # 48 obs of 2 vars
  arrange(desc(total.crop.dmg)) %>%
  slice(1:7)  # 7 obs of 2 variables

# Create a figure that displays both the property damage and crop damage plots
par(mar = c(9.1, 4.1, 4.1, 2.1), mfrow = c(1, 2), las = 2)  # Setup plot space

# Add the plot of the property damage data
barplot(table.2a$total.prop.dmg, names.arg = table.2a$event.type,
        ylab = "Total Property Damage (Billions of Dollars)",
        main = "Event Types Most Damaging to Property")

# Add the plot of the crop damage data
barplot(table.2b$total.crop.dmg, names.arg = table.2b$event.type,
        ylab = "Total Crop Damage (Billions of Dollars)", 
        main = "Event Types Most Damaging to Crops")

