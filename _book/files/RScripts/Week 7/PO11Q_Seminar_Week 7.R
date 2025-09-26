########################################
# PO11Q - Worksheet Week 7
########################################

rm(list=ls())

# Set Working Directory 
setwd()

# PO11Q - Worksheet Week 5
##########################

# Read in Data

library(readxl)

EU <- read_excel("EU.xlsx", sheet="Sheet1")

head(EU)

names(EU)

str(EU)

# Nominal Variables
EU$country = factor(EU$country)

# Ordinal Variables
EU$access_fac = factor(EU$access, ordered = TRUE)

library(tidyverse)

EU <- EU %>%
  mutate(wave = recode(access_fac, '1951'="Founding", 
                       '1973'= "First",
                       '1981'= "Mediterranean",
                       '1986' = "Mediterranean",
                       '1995' = "Cold War",
                       '2004' = "Eastern",
                       '2007' = "Eastern",
                       '2013' = "Balkans"))


EU <- EU %>%
  mutate(wave = recode(access_fac, '1951'="Founding", 
                       '1973'= "First",
                       '1981'= "Mediterranean",
                       '1986' = "Mediterranean",
                       '1995' = "Cold War",
                       '2004' = "Eastern",
                       '2007' = "Eastern",
                       '2013' = "Balkans"), ordered=TRUE)

head(EU$wave_order)


EU <- EU %>% 
  mutate(wave1=cut(access, 
                   breaks=c(1950, 1951, 1973, 1986, 1995, 2007, 2013), 
                   labels=c("Founding","First",
                            "Mediterranean", 
                            "Cold War", 
                            "Eastern", 
                            "Balkans"))) 

levels(EU$wave1)



EU$wave_order1 <- ordered(EU$wave1, levels = c("Founding",
                                               "First",
                                               "Mediterranean", 
                                               "Cold War",
                                               "Eastern", 
                                               "Balkans"))

head(EU$wave_order1)






# Binary Dummy

EU <- EU %>%
  mutate(founding = recode(access_fac, '1951'="Yes", 
                           '1973' = "No",
                           '1981' = "No",
                           '1986' = "No",
                           '1995' = "No",
                           '2004' = "No",
                           '2007' = "No",
                           '2013' = "No"))

# OR, much shorter

EU <- EU %>% 
  mutate(founding = factor(ifelse(access_fac=="1951", "Yes", "No"), 
                           levels =c("Yes", "No")))

# the result is the same

str(EU$founding)



# Sub-Setting Data

## By Variable

EU2$area <- NULL

EU_pop <- select(EU2, country, pop18, access_fac, founding)

EU_pop1 <- select(EU2, -access, -GDP_2015)



## By Observation


EU_nobenelux <- slice(EU2, -1, -16, -19)

EU_benelux <- slice(EU2, 1, 16, 19)


## Keep if a variable has a certain value, e.g. 'pop18' larger than 10,000,000

EU_pop_large <- filter(EU2, pop18 > 10000000)

# Ordering Data

EU_subset <- select(EU2, country, pop18, access)

eu_order <- arrange(EU_subset, pop18)

eu_order[1:10,]

eu_order <- arrange(EU_subset, desc(pop18))
eu_order[1:10,]

eu_order <- arrange(EU_subset, access, pop18)

eu_order[1:10,]


# Grouping Data


eu_access <- group_by(EU_subset, access)

ungroup(EU_subset)

EU_subset %>% 
  group_by(access) %>% 
  summarise(avg = mean(pop18)) -> eu_popaccess

eu_popaccess


# Combining Ordering and Grouping Data


eu_popaccess_order <- arrange(eu_popaccess, desc(avg))

eu_popaccess_order


# PO11Q - Worksheet Week 7
##########################


mean(EU$pop18)

median(EU$pop18)

summary(EU$pop18)

min(EU$pop18)

max(EU$pop18)

range(EU$pop18)

sd(EU$pop18)

var(EU$pop18)

# Graphs

## Basic Graphs

EU$popmio <- EU$pop18/1000000

hist(EU$popmio, breaks=4)

hist(EU$popmio, breaks=4,
xlab="Population in million",
main="Histogram of EU Population (2018)")


boxplot(EU$popmio, horizontal = TRUE)

summary(EU$popmio)

# Advanced Graphs

library(ggplot2)

ggplot(data=EU) +
geom_histogram(mapping=aes(popmio), binwidth = 20)


ggplot(data=EU) +
geom_histogram(mapping=aes(popmio), binwidth = 20, 
boundary = 0)



ggplot(data=EU) +
geom_histogram(mapping=aes(popmio), binwidth = 20, 
boundary = 0) +
scale_x_continuous(breaks = seq(0, 100, 20)) +
labs(x="Population (in million)", y="Frequency")


####################################################################
# BASE R
####################################################################
# hist(EU$popmio, breaks = 4,
#      xlab = "Population (in million)", ylab = "Frequency") 
####################################################################




  
# Even More Advanced Graphs
  
ggplot(data = EU) + 
  geom_histogram(binwidth = 20, boundary = 0,
                 aes(x= popmio, 
                     y = (..count..)/sum(..count..)*100)) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  labs(x="Population (in million)", y="percent") 


####################################################################
# BASE R
####################################################################
# Create histogram data without plotting
# h <- hist(EU$popmio, 
#          breaks = seq(0, 100, by = 20), 
#          plot = FALSE)

# Normalize counts to percentages
# h$counts <- h$counts / sum(h$counts) * 100

# Plot histogram
# plot(h, 
#     freq = FALSE, 
#     xlab = "Population (in million)", 
#     ylab = "percent", 
#     main = "")
####################################################################






# Exercises
############

# Calculate GDP per capita in a new variable, and sort countries in descending order (for this it is ok to mix 2015 and 2018 data).

EU$gdppc <- EU$GDP_2015/EU$pop18

#or 

EU$gdppc <- with(EU, GDP_2015/pop18)

EU_arrange <- EU %>%
  arrange(desc(EU$gdppc))

####################################################################
# BASE R
####################################################################
# EU_arrange <- EU[order(-EU$gdppc),]
####################################################################


EU_arrange


# Which country is the least and which is the most densely populated?

## Population Density

EU$density <- with(EU, pop18/area)

## Sort by descending density
EU_density <- EU %>%
  arrange(desc(density))

####################################################################
# BASE R
####################################################################
# EU_density <- EU[order(-EU$density),]
####################################################################



## print all rows of the data frame
EU_density %>% print(n = nrow(.))

####################################################################
# BASE R
####################################################################
# print(EU_density, n = nrow(EU_density))
####################################################################




## or more elegantly, only showing the two observations we are interested in

EU_density$density[c(1,length(EU_density$density))]

## or even more elegantly

with(EU_density, density[c(1,length(density))])

## which countries are these?

with(EU_density, country[c(which.max(density), which.min(density))])




# Recode the pop18 variable into a categorical variable with low, medium, and high population (cut-off points are your choice).

summary(EU$pop18)

EU <- EU %>%
  mutate(popcat=
           ordered(
             cut(pop18, breaks=c(0, 3781345, 17766718, 83000000),
                 labels=c("low","medium","high"))))


####################################################################
# BASE R
####################################################################
# EU$popcat <- as.factor(ifelse(EU$pop18 <= 3781345, 'low',
# ifelse(EU$pop18 <= 17766718, 'medium', 'high')))
####################################################################


table(EU$popcat)

