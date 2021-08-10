##################################################
## Project: case studies in networks
## Script purpose: introduction to R
## Date: 10-03-2020
## Author: David JP O'Sullivan
##################################################

rm(list = ls()) # tidy work space
gc()

# libraries, source files and data ---------------

install.packages('tidyverse')
library(tidyverse)


#####
#
# The cheat sheet are great!!!!
# https://raw.githubusercontent.com/rstudio/cheatsheets/master/rstudio-ide.pdf
####

# install.packages('devtools') # install this package
devtools::install_github('https://github.com/DavidJPOS/IntroductionToR')
# Run this for the introduction to R coding.
learnr::run_tutorial(name = 'section_1_basics_of_R', package = 'IntroductionToR') 
# Run this for the introduction to the 'Tidyverse'.
learnr::run_tutorial(name = 'section_2_basics_of_tidyverse', package = 'IntroductionToR')


# basics of r -------------------------------------------------------------

# assignment of vars
x <- 3 
x

# combining vectors
v1 <- c(1,2,3)
class(v1)

v2 <- 'This is a string'
class(v2)

v3 <- c(v1 , v2)
class(v3)

# indexing and sequences
v3[1]
v3[2:4]
v3[seq(from = 2, to = 4, by = 1)]
v3[c(T,F,T,F)]

# getting help on functions
?seq()

# logical operations
v4 = c(1,2,3,10,20,30)

v4 > 0
v4 > 20
v4 == 20
v4 != 20

## data frames 

# create a data frame
your_first_df <- data.frame(
  ID = 1:4, # create a varaible (column) called ID
  name = c("Stephen","Aoife","Pete","Sarah"), 
  female = c(F,T,F,T), # add logical for 'female' varaible
  age=c(22,33,44,55), # create a age variable
  stringsAsFactors = FALSE # other wise all 'strings' will be treated as factors
)

# subsetting - rows 1 and 2 with cols female and age
your_first_df[1:2,3:4]
your_first_df[1:2,c('female', 'age')]


# tidyverse - dplyr -------------------------------------------------------

#####
#
# The cheat sheet are great!!!!
# https://raw.githubusercontent.com/rstudio/cheatsheets/master/data-transformation.pdf
####

# piping
x = seq(from = -5, to = 5, by = 1)
log(sum(abs(x))) # ugly looking

x %>% # much, much better looking
  abs() %>% 
  sum() %>% 
  log()

# The vast majority of data manipulation can be solved using just five dplyr functions:
#   
# - Pick observations by their values (`filter()`).
# - Reorder the rows (`arrange()`) --- works well with `desc`() if you want decending order. 
# - Pick variables by their names (`select()`).
# - Create new variables by transforming existing variables (`mutate()`).
# - Collapse many values down to a single summary (`summarise()`).
# 
# The `group_by()` function allows the above operations to be split by a particular categorical/grouping variable. 


# read in flight data
?read_csv

flights <- read_csv('./data/flights_full.csv')
class(flights) # note it is both a tibble and a data frame
flights # tibble are just data frame but with nice printing and other properties
# that make them easy to use

# have a peek at the first 6 rows of the flight data
flights %>% head()

flights %>% tail() # you can also look at the last few rows

# use filter find all data in Nov, Dec, Jan
filter(flights, month %in% c(12, 11, 1))

# often we use the pipe to make our code more readable so that the above becomes
flights %>% # take the flights data frame
  filter(month %in% c(12, 11, 1)) # and retain only the months 12, 11 and 1. 

# rearrange the rows in decreasing order
flights %>% arrange(desc(dep_delay))

mutate(flights,
       gain = dep_delay - arr_delay, # add variable for how late the flight was
       hours = air_time / 60, # what was the air time in hours? 
       gain_per_hour = gain / hours 
)

# create summary for the typically delay time by month by:


flights %>% # taking the flight data frame
  group_by(month) %>% # and for every month 
  summarise( # summaries them by their
    median_delay = median(dep_delay, na.rm = TRUE), # median delay
    mean_delay = mean(dep_delay, na.rm = TRUE) # mean delay
  ) %>% # and arrange from the largest mean delay to the smallest
  arrange(desc(mean_delay))


# tidyverse - ggplot2 ----------------------------------------------------

# print the data to screen
mpg <- mpg %>% as_tibble()
mpg


# plot displ vs hwy coloring point by number of cylinders (cyl)
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(aes(color = factor(cyl)))
p

# add a trend line
p + geom_smooth()

# other types of plot (boxplots)
ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = class, y = hwy, fill = class))

ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = class, y = hwy, color = class))


p_hwm <- 
  ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = class, y = hwy, fill = class)) + 
  scale_fill_viridis_d() + 
  xlab('Class of car') + 
  ylab('Highway mileage (MPG)')

ggsave(filename = './plots/hightway_mileage.png', plot = p_hwm)

# starwars dataset example  -------------------------------------------------------------------------

# this is another example taken from: https://www.youtube.com/watch?v=nRtp7wSEtJA
# open the video and follow along

view(starwars)

starwars %>% 
  select(gender, mass, height, species) %>% # what var are we interested in?
  filter(species == 'Human') %>% # select humans
  na.omit() %>% # remove NA values
  mutate(
    height = height/100, # convert height from cm to meters
    BMI = mass/height^2 # calculate BMI 
    ) %>% 
  group_by(gender) %>% 
  summarise(
    average_BMI = mean(BMI)
  )


starwars %>% 
  select(gender, mass, height, species) %>% # what var are we interested in?
  filter(species == 'Human') %>% # select humans
  na.omit() %>% # remove NA values
  mutate(
    height = height/100, # convert height from cm to meters
    BMI = mass/height^2 # calculate BMI 
  ) %>% 
  group_by(gender) %>% 
  summarise(
    average_BMI = mean(BMI)
  )











