# r-project1
---
title: "Project 1"
author: "BANCY NJERU"
date: "2023-09-12"
output: html_document
---


```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

```



### Introduction

The dataset contains yearly data for various countries for the period 1960-2021. 

The dataset contains the following variables:

-Country name  
-Country code  
-Year  
-Population for the year

Let's explore the data.




### Data Loading and Exploration


```{r}

library(tidyverse)
library(kableExtra)
library(janitor)
library(countrycode)

#importing the dataset and clean the column names.

pop <- read_csv("population.csv")

pop <- pop %>% 
  janitor::clean_names() %>%
  rename(population_count = value)

kable_styling (kable(head(pop, n = 5)), position = "left")


```


#### Check for missing values


```{r}

sum(is.na(pop))

```

Our dataset does not have any missing value so we can continue with exploration.



### Data Visualization

We want to see how the population of various countries has grown over time.


##### 1. How did the population of your country change (Kenya)


```{r, fig.width=12, fig.height=8, message=F}

Kenyapop <- pop %>% 
  filter(country_name == "Kenya")
  
  ggplot(Kenyapop, aes(x = year, y = population_count)) +
    geom_line() +
    labs(title = "Line Plot for Population Growth in Kenya", x = "Year", y = "Population")+
    theme_bw()
  
  
```

The population of Kenya is seen to increase rapidly over time.


##### 2. How did the population of different parts of the world change


Let's approach the question with continents as the parts of the world. We will use the countrycode package to fill continent names.

```{r, fig.width=12, fig.height=8, message=F}
pop$continent <- countrycode(sourcevar = pop[[ "country_name"]],
                            origin = "country.name",
                          destination = "continent")

# summarize per continent across years

diff_parts <- pop %>% 
  group_by(continent, year) %>% 
  summarise(mean_pop = mean(population_count, na.rm = TRUE)) %>% 
  na.omit()


ggplot(diff_parts, aes(x =  year, y =  mean_pop, color =  continent))+
  geom_line() +
  theme_bw()




```


##### 3. Which country has the highest increase/decrease in population over time


```{r, fig.width=10, fig.height=2, message=F}

population_change <- pop %>% 
  group_by(country_name) %>% 
  summarise(lastcount = population_count[ year == 2021], firstcount = population_count[year == 1960]) %>% 
  mutate(change = lastcount - firstcount)


# Find the largest and smallest values in the change column
highest_increase <- max(population_change$change)
highest_decrease <- min(population_change$change)

# Find the corresponding country names
country_with_highest_increase <- population_change$country_name[which.max(population_change$change)]
country_with_highest_decrease <- population_change$country_name[which.min(population_change$change)]

# Create a data frame with the results
results <- data.frame(
  Metric = c("Highest Increase", "Highest Decrease"),
  Value = c(highest_increase, highest_decrease),
  Country = c(country_with_highest_increase, country_with_highest_decrease)
)

# Use kable() to create a table
#kable(results, format = "html", caption = "Population Change Summary")
  kable_classic(kable(results, full_width = F, html_font = "Cambria"))



```

Bulgaria is seen to have recorded the highest decrease in population growth. Let's visualize to see the trend.

```{r, fig.width=14, fig.height=8, message=F}

Bulgariapop <- pop %>% 
  filter(country_name == "Bulgaria")

ggplot(Bulgariapop, aes(x = year, y = population_count)) +
  geom_line() +
  labs(title = "Line Plot for Population Growth in Bulgaria", x = "Year", y = "Population")+
  scale_x_continuous(breaks = seq(min(Bulgariapop$year), max(Bulgariapop$year), by = 2))+
    theme_bw()


```



The population for Bulgaria is seen to increase until 1988 when it starts to decrease drastically until 2021




##### 4. What are the tendencies of the population growth over time


```{r, fig.width=14, fig.height=8, message=F}

world_pop<- pop %>% 
  filter(country_name == "World") %>% 
  ggplot( aes(x = year, y = population_count)) +
  geom_line()


world_pop


```

The world population is seen to increase exponentially from 1960 to 2021.
