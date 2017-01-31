#################
#### PART 1 #####
#################
library(dplyr)

#Load the Any Drinking data set into R (the data/any_drinking.csv file). Make sure to not treat strings as factors!
any.drinking<-read.csv("data/any_drinking.csv", stringsAsFactors = FALSE)

#Using functions from the dplyr package, extract a data frame that has the state and location columns, as well as all the columns with data from 2012.
state.and.location<-select(any.drinking, state, location, both_sexes_2012, females_2012, males_2012)

#Using functions from the dplyr package, add a column to your new data frame that has the difference between male and female drinking amounts in 2012. Save this as a variable so you can access it later!
state.and.location<-mutate(state.and.location, difference=males_2012-females_2012)

#Are there any locations where females drink more than males? Print out how many (e.g., "Females drank more than males in X locations").
females.drink.more.num<-length(state.and.location[state.and.location$females_2012>state.and.location$males_2012, "location"])
paste("Females drank more than males in", females.drink.more.num, "locations")

#What is the location in which male and female drinking percentages are the closest? Print the location (e.g., "County, State").
lowest.difference<-filter(state.and.location, difference == min(difference))
paste(lowest.difference$location,",",lowest.difference$state)

#Not really a question, but you'll want to create a new data frame that is only the state level observations from your 2012 data frame.
states.data<-group_by(state.and.location, state) %>% summarize(
   both_sexes_2012 = mean(both_sexes_2012), females_2012 = mean(females_2012), males_2012 = mean(males_2012)
)
#Make sure that this data frame contains only rows for the 50 states and the District of Columbia.

#Which state had the highest drinking rate for both sexes combined? Print the state's name.
highest.drinking.state <- filter(states.data, both_sexes_2012 == max(both_sexes_2012))$state
print(highest.drinking.state)

#Which state had the lowest drinking rate for both sexes combined? Print the state's name.
lowest.drinking.state <- filter(states.data, both_sexes_2012 == min(both_sexes_2012))$state
print(lowest.drinking.state)

#What is the difference between the combined-sexes drinking rates of the states with the highest and lowest drinking levels? Print this difference (e.g., "Range in drinking rate was X%")
high.low.state.difference<- filter(states.data, both_sexes_2012 == max(both_sexes_2012))$both_sexes_2012 -
  filter(states.data, both_sexes_2012 == min(both_sexes_2012))$both_sexes_2012

paste0("Range in drinking rate was ", high.low.state.difference,"%")
  

#################
#### PART 2 #####
#################
library(stringr)

#Load the Binge Drinking data set into R (the data/binge_drinking.csv file) Make sure to not treat strings as factors!
binge.drinking<-read.csv("data/binge_drinking.csv", stringsAsFactors = FALSE)

#Using functions from the dplyr package, extract a data frame that has only county-level observations (so no national or state rows).
county.level<-select(binge.drinking, location, everything()) %>% filter(str_detect(location, "County"))

#Confirm that you've extracted the correct data by checking that the number of observations is correct. It should only be missing the 50 states, the District of Columbia, and the National data.
states<-binge.drinking$state
str_detect(states, county.level$location)

#Using functions from the dplyr package in a single statement (line of code) add three (3) new columns to your data frame of county-level binge drinking data. These columns should include the change in binge drinking rates from 2002 to 2012 for both sexes, males, and females respectively.
county.level<-mutate(county.level,change_both_sexes = both_sexes_2012-both_sexes_2002) %>% mutate(change_in_males = males_2012-males_2002) %>% mutate(change_in_females = females_2012-females_2002)

#What was the average county-level rate of binge drinking in 2012 for both sexes? Use the summarize() function; it's fine if you just print the resulting data frame.
summarize(county.level, average_both_sexes_binge_rate_2012 = mean(both_sexes_2012))

#What was the minimum county-level rate of binge drinking in 2012 for both sexes? Use the summarize() function; it's fine if you just print the resulting data frame.
summarize(county.level, min_both_sexes_binge_rate_2012 = min(both_sexes_2012))

#What was the minimum county-level binge drinking in each state in 2012 for both sexes? What was the maximum?
group_by(binge.drinking, state) %>% summarize(min_both_sexes_binge_rate_2012 = min(both_sexes_2012))

#Save a single data frame containing these minimum and maxmum binge drinking rates to a .csv file called state_binge_drinking.csv in the data folder of your repo. Hint: you can use a single dplyr function call to create a summary table with multiple statistics!
write.csv(group_by(binge.drinking, state) %>% summarize(min_both_sexes_binge_rate_2012 = min(both_sexes_2012)), file="data/state_binge_drinking.csv")

#What is the percentage of counties that observed an increase in male binge drinking between 2002 and 2012? Print this percentage (e.g., "Male binge increase: X%") Note you should round this percentage to the nearest whole number.
male.increase.percent<-round((filter(county.level, change_in_males > 0 ) %>% nrow()) / (nrow(county.level)) * 100)
paste0("Male binge increase: ", male.increase.percent, "%")

#What is the percentage of counties that observed an increase in female binge drinking between 2002 and 2012? Print this percentage (e.g., "Female binge increase: X%").
female.increase.percent<-round((filter(county.level, change_in_females > 0 ) %>% nrow()) / (nrow(county.level)) * 100)
paste0("Female binge increase: ", female.increase.percent, "%")

#What is the percentage of counties that observed an increase in female binge drinking but a decline in male binge drinking between 2002 and 2012? Print this percentage (e.g., "Female binge increase & male decrease: X%")
female.increase.male.decline.percent<-round( (filter(county.level, change_in_females > 0 ) %>% filter(change_in_males < 0) %>% nrow()) / (nrow(county.level)) * 100)
paste0("Female increase & male decrease binge percentage: ", female.increase.male.decline.percent, "%")

#Which state had the largest median increase in county-level male binge drinking between 2002 and 2012? You'll need to perform multiple manipulations to get this information. Use a pipe to do so in a single command.
group_by(county.level, state) %>% filter(change_in_males>0) %>% summarize(median_change = median(change_in_males)) %>% filter(median_change == max(median_change))

#Considering only the counties that saw an increase in female binge drinking and a decrease in male binge drinking, which state has the largest median increase in county-level female binge drinking between 2002 and 2012? Again, use a pipe to perform the multiple manipulations required.
group_by(county.level, state) %>% filter(change_in_females > 0 ) %>% filter(change_in_males < 0) %>% summarize(median_change = median(change_in_females)) %>% filter(median_change == max(median_change))


#################
#### PART 3 #####
#################

#Modify the column names of each of the yearly drinking rates so that they include an any_ or binge_ prefix as appropriate. Thus the males_2002 column from the Any Drinking data will become any_males_2002, while the males_2002 column from from the Binge Drinking data will become binge_males_2002. Note that you will likely need to use multiple statements (lines of code) to do this. Hint: use the paste() function to modify each element in the vector of column names!
colnames(any.drinking)<- paste0("any_", colnames(any.drinking)) 
colnames(county.level)<- paste0("binge_", colnames(county.level)) 

#Using functions from the dplyr package, now create a new data frame all.drinking that is your two (raw) data frames combined together. Each location should contain the columns for both Any Drinking and Binge Drinking for that location.
all.drinking<-left_join(any.drinking, county.level, by = c("any_state" = "binge_state", "any_location" = "binge_location"))

#Considered across counties (not states!), what was the average rate of drinking that was not binge drinking in 2012? Print the rate (as a data frame is fine).
select(all.drinking, any_location, any_both_sexes_2012) %>% filter(str_detect(any_location, "County")) %>% summarize(average_any_both_sexes_2012 = mean(any_both_sexes_2012))

#Which state had the smallest amount of drinking that was not binge drinking (on average) in 2012? (That is, which state was likely to have people who binge drank if they drank at all?) Print a data frame that contains the name of the state along with its "any drinking" rate, "binge drinking" rate, and the difference between them.
mutate(all.drinking, difference_both_sexes_2012 = any_both_sexes_2012 - binge_both_sexes_2012) %>% filter(any_both_sexes_2012 == min(any_both_sexes_2012)) %>% select(any_state, binge_both_sexes_2012, any_both_sexes_2012, difference_both_sexes_2012)

#Define a function ExportStateYear that takes as arguments a state name and a year. This function should save to a .csv file all the observations for that state for that year (for both Any Drinking and Binge Drinking rates). This includes the observations for the individual counties as well as the state aggregate. The exported features (columns) should include only the location, state, and the 6 columns of data for that year. Moreover, the data should be sorted in descending order by the any_both_sexes rate for that year.
ExportStateYear <- function(state, year) {
  selected.year.data <- paste0(c("any_both_sexes_", "any_females_", "any_males_", "binge_both_sexes_", "binge_females_", "binge_males_"), year)
  state.year.dataframe<-select_(all.drinking, ~any_state, ~any_location, .dots = selected.year.data) %>% filter(any_state == state) %>% arrange_(paste0("desc(any_both_sexes_",year,")"))
  filename<-paste0("data/drinking_", state, year, ".csv")
  write.csv(state.year.dataframe, file = filename)
  
}

#Demonstrate your function works calling it twice: once to create a file for Washington state in 2010, and once to create a file for another state and year of your choice.
ExportStateYear("Washington", "2010")
ExportStateYear("Alabama", "2002")



#################
#### PART 4 #####
#################

# Your script for Part 4 goes here (and delete this comment!)
