# import needed libraries
library(dplyr)
library(lubridate)

# Load data
unemployment_data <- read.csv("/Users/noraadadurova/Desktop/Math_261A/Project_2/data/unemployment_data.csv", 
                              header = TRUE)

# showing data
head(unemployment_data)

# note that index column got duplicated in the process of inporting the data
# so drop it
unemployment_data <- unemployment_data |>
  select(-X_id)

# check that is was dropped successfully
head(unemployment_data)

# let's now check for missing values
missing_values <- sapply(unemployment_data, function(x) sum(is.na(x)))
print(missing_values)

# note: no missing values were found in the dataset

# let's now check for duplicated rows
sum(duplicated(unemployment_data))
unemployment_data[duplicated(unemployment_data), ]

# note: got one duplicated row

# so, let's get rid of the duplicate
# check number of rows before
nrow(unemployment_data)

# keep only non-duplicated rows
unemployment_data <- unemployment_data[!duplicated(unemployment_data), ]

# let's check number of rows now once we got rid of the duplicate 
# note: should be 1 less rows
nrow(unemployment_data)

# double-check there are no duplicates left
sum(duplicated(unemployment_data))

# note: great! no more duplicates

# now let's check data formate of each column
sapply(unemployment_data, class)

# so, note that Filed.week.ended column's type is "character"
# and Reflecting.Week.Ended is also type "character"
# let's change it to time-series approperiate Date type
unemployment_data$Filed.week.ended <- mdy(unemployment_data$Filed.week.ended)
unemployment_data$Reflecting.Week.Ended <- mdy(unemployment_data$Reflecting.Week.Ended)

# now let's check again the data formate of each column
sapply(unemployment_data, class)

# note: Great! We got "Date" type for the two date columns

# next, let's explore the other character columns (future categorical variables)
# first, let's convert then into categorical variables for easier analysis
unemployment_data$Area.Type <- as.factor(unemployment_data$Area.Type)
unemployment_data$Area.Name <- as.factor(unemployment_data$Area.Name)

# let's check that conversion worked
sapply(unemployment_data, class)

# note: Great! It worked
# now, let's see that categories do these columns have 
unique(unemployment_data$Area.Type)
unique(unemployment_data$Area.Name)

# new descovery: the columns "Area.Name" has only "California" state as a unique value 
# and "Area.Type" has only one unique value "State"
# hence, let's drop both these columns since they do not give any extra info

# droping columns
unemployment_data <- unemployment_data |>
  select(-Area.Type, -Area.Name)

# let's check that it worked
head(unemployment_data)

# Great! It worked!

# let's use rates intead of raw counts in order to handle population changes
# main idea let's take the number of claims in a given week 
# and divide it by the total number of covered employees in that same week
# let's do it for both initial and continued claims
unemployment_data <- unemployment_data |>
  mutate(
    Initial.Claims.Rate = Initial.Claims / Covered.Employment,
    Continued.Claims.Rate = Continued.Claims / Covered.Employment
  )

# let's check that the new columns were created succsesfully
head(unemployment_data)

# also, let's recall Math 265 and create seasonality variables
unemployment_data <- unemployment_data |>
  mutate(
    WeekOfYear = week(Filed.week.ended),
    Month = month(Filed.week.ended, label = TRUE),
    Quarter = quarter(Filed.week.ended, with_year = FALSE, fiscal_start = 1),
    Year  = year(Filed.week.ended)
  )

# let's check that the new columns were created succsesfully
head(unemployment_data)
sapply(unemployment_data, class)

# let's now save the updated dataset
path <- "/Users/noraadadurova/Desktop/Math_261A/Project_2/data/clean_unemployment_data.csv"
write.csv(unemployment_data, 
          file = path, 
          row.names = FALSE)