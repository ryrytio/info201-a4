# A4 Data Wrangling

# Loading and Exploring Data -------------------------------- (**28 points**)

# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package
library(dplyr)
library(lintr)
# Load your data, making sure to not interpret strings as factors
ks_pledges <- read.csv("kickstarter_pledge.csv", stringsAsFactors = FALSE)
View(ks_pledges)
ks_pledges <- na.omit(ks_pledges)
# To start, write the code to get some basic information about the dataframe:
# - What are the column names? 
  colnames(ks_pledges)
# - How many rows is the data frame?
  nrow(ks_pledges) #378,661 rows
# - How many columns are in the data frame?
  ncol(ks_pledges) #15 rows
# Use the `summary` function to get some summary information
 summary(ks_pledges)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
  # column name and a dataframe. If the values in the column are of type *double*,
  # the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* double and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* double and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type
get_col_info <- function(ks_col, ks_df) {
  ks_yeet <- ks_pledges %>%
    select(ks_col)
  #first condition
if ( (typeof(pull(ks_df, ks_col))) == "double") {
  ks_list <- list()

  min_pledge <- min(ks_df[[ks_col]])


  max_pledge <- max(ks_df[[ks_col]])

  avg_pledge <- mean(ks_df[[ks_col]])
  ks_list[["min_pledge"]] <- min_pledge
  ks_list[["max_pledge"]] <- max_pledge
  ks_list[["avg_pledge"]] <- avg_pledge
  return(ks_list)

} #second condition
 ks_unlist <- unlist(ks_df[ks_col], use.names = FALSE)
 if (typeof(pull(ks_df, ks_col)) != "double" &&
   length(unique(ks_unlist)) < 10) {
 ks_list2 <- list()

 n_values <- length(unique(ks_unlist))

 unique_values <- unique(ks_unlist)

 ks_list2[["n_values"]] <- n_values
 ks_list2[["unique_values"]] <- unique_values

 return(ks_list2)

 } #third condition

  ks_unlist2 <- unlist(ks_df[ks_col], use.names = FALSE)
 if (length(unique(ks_unlist2)) >= 10 ) {
 ks_list3 <- list()
 n_values2 <- length(unique(ks_unlist2))

 sample_values <- sample(ks_unlist2, 10)

 ks_list3[["n_values2"]] <- n_values2
 ks_list3[["sample_values"]] <- sample_values

 return(ks_list3)
 }
}

# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name
pledged_col_info <- get_col_info("usd.pledged", ks_pledges)

currency_col_info <- get_col_info("currency", ks_pledges)

state_col_info <- get_col_info("state", ks_pledges)
# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop
get_summary_info <- function(ks_df) {

  ks_names <- as.list(colnames(ks_df))
  summary_list <- list()

  for (col in ks_names) {
    summary_list[[col]] <- get_col_info(col, ks_df)
  }
  return(summary_list)
}


 
# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable
ks_all_data <- get_summary_info(ks_pledges)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# 1) What's the difference between goal amount and pledged amount for unsuccessful
# projects? 
# 2) Which countries had the most backers?
# 3) What was the most common project category?


# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!

# What was the name of the project(s) with the highest goal?
highest_goal_projects <- ks_pledges %>%
  filter(goal == max(goal)) %>%
  select("name")

  
# What was the category of the project(s) with the lowest goal?
minimum_goal_projects <- ks_pledges %>%
  filter(goal == min(goal)) %>%
  select("name")

# How many projects had a deadline in 2018?
finished_projects_2018 <- ks_pledges %>%
  filter(substr(deadline, 6, 9) == "2018") %>% nrow()
 
  
# What proportion or projects weren't successful? Your result can be a decimal
proportion_of_unsuccess_projects <- ks_pledges %>%
  filter(state != "successful") %>%
  select(state) %>%
  nrow() / nrow(ks_pledges)
  

# What was the amount pledged for the project with the most backers?
most_backers_pledge_amount <- ks_pledges %>%
  filter(backers == max(backers)) %>%
  select(pledged) %>%
  pull()

# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
max_failed_project_names <- ks_pledges %>%
  filter(state == "failed") %>%
  filter(state == max(state)) %>%
  select("name")

# How much total money was pledged to projects that weren't successful?
total_unsuccessful_pledge <- ks_pledges %>%
  filter(state != "successful") %>%
  select(pledged) %>%
  sum()
  
# Write (and answer) two meaningful questions of the data that can be answered
# using similar operations (`filter`, `pull`, `summarize`, `mutate`, etc.).
#
# Question 1: What is the amount pledged for the project with least backs?
least_backers_pledge_amount <- ks_pledges %>%
  filter(backers == min(backers)) %>%
  select(pledged) %>%
  pull()

# Question 2: How many projects had a deadline in 2017? 
finished_projects_2017 <- ks_pledges %>%
  filter(substr(deadline, 6, 9) == "2017") %>% nrow()

# Performing analysis by *grouped* observations ----------------- (38 Points)

# Which category had the most money pledged (total)?
most_pledged_category <- ks_pledges %>%
  group_by(category) %>%
  summarize(total = sum(pledged)) %>%
  filter(total == max(total)) %>%
  pull(category)
  

# Which country had the most backers?
country_most_backers <- ks_pledges %>%
  group_by(country) %>%
  summarize(all_backers = sum(backers)) %>%
  filter(all_backers == max(all_backers)) %>%
  pull(country)
  

# Which year had the most money pledged (hint: you may have to create a new
# column)?
year_most_pledged <- ks_pledges %>%
  mutate(year_only = substr(deadline, 6, 10)) %>%
  group_by(year_only) %>%
  summarize(sum_pledge = sum(pledged, na.rm = TRUE)) %>%
  filter(sum_pledge == max(sum_pledge)) %>%
  pull(year_only)
  
  

# What were the top 3 main categories in 2018 (as ranked by number of backers)?
top_3_categories <- ks_pledges %>%
  group_by(main_category) %>%
  mutate(year_only = substr(deadline, 6, 10)) %>%
  filter(year_only == "2018") %>%
  summarise(all_backers = sum(backers)) %>%
  arrange(-all_backers) %>%
  slice(1:3) %>%
  pull(main_category)

# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
most_common_day <- ks_pledges %>%
  group_by(launched) %>%
  mutate()

# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were successful)? This might require some creative problem solving....


# Write (and answer) two meaningful questions of the data that can be answered
# by _grouping_ the data and performing summary calculations.
#
# 1) What are the bottom 3 categories?
bottom_3_categories <- ks_pledges %>%
  group_by(main_category) %>%
  mutate(year_only = substr(deadline, 6, 10)) %>%
  filter(year_only == "2018") %>%
  summarise(all_backers = sum(backers)) %>%
  arrange(all_backers) %>%
  slice(1:3) %>%
  pull(main_category)
# 2) What year had the least money pledged?
year_least_pledged <- ks_pledges %>%
  mutate(year_only = substr(deadline, 5, 10)) %>%
  group_by(year_only) %>%
  summarize(sum_pledge = sum(pledged, na.rm = TRUE)) %>%
  filter(sum_pledge == min(sum_pledge)) %>%
  pull(year_only)