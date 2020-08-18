---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
## ✓ tibble  3.0.3     ✓ dplyr   1.0.1
## ✓ tidyr   1.1.1     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.5.0
```

```
## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```


## Loading and preprocessing the data

We unzip the archive, load a CSV file and have a look at the data.


```r
unzip("./activity.zip")
activity <- read_csv("./activity.csv")
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
glimpse(activity)
```

```
## Rows: 17,568
## Columns: 3
## $ steps    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ date     <date> 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01,…
## $ interval <dbl> 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, 105, 110,…
```

```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```


## What is mean total number of steps taken per day?

First summary statistics of steps per each day are produced:


```r
activity_per_day <- activity %>% 
  group_by(date) %>% 
  summarise(total_steps = sum(steps, na.rm = TRUE),
            mean = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(activity_per_day)
```

```
## # A tibble: 6 x 3
##   date       total_steps    mean
##   <date>           <dbl>   <dbl>
## 1 2012-10-01           0 NaN    
## 2 2012-10-02         126   0.438
## 3 2012-10-03       11352  39.4  
## 4 2012-10-04       12116  42.1  
## 5 2012-10-05       13294  46.2  
## 6 2012-10-06       15420  53.5
```

Histogram of total amount of steps taken per day:


```r
ggplot(activity_per_day, aes(x = total_steps)) +
  geom_histogram(fill = "white", color = "black") +
  theme_minimal() +
  xlab("total steps per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/day-activity-hist-1.png)<!-- -->

We see that first *peak* is at or around zero (caused mainly by the missing values), otherwise the distribution looks
quite normal with a peak around 10.000 steps per day.

Lets confirm this with summary statistics:


```r
activity_per_day %>% 
  summarise(mean = mean(total_steps, na.rm = TRUE),
            median = median(total_steps, na.rm = TRUE))
```

```
## # A tibble: 1 x 2
##    mean median
##   <dbl>  <dbl>
## 1 9354.  10395
```


## What is the average daily activity pattern?

Now we look at the activity pattern during the average day.


```r
activity_per_interval <- activity %>%
  group_by(interval) %>% 
  summarise(steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
g <- activity_per_interval %>% 
  ggplot(aes(x = interval, y = steps)) +
  geom_line() +
  theme_minimal() +
  ylab("mean steps")

g
```

![](PA1_template_files/figure-html/interval-activity-1.png)<!-- -->

To highlight the average daily pattern we can add individual datapoints in the background and use a smooth line:


```r
activity %>%
  ggplot(aes(x = interval, y = steps)) +
  geom_point(alpha = 0.2, color = "gray80") +
  geom_line(data = activity_per_interval, size = 1.2, color = "black") +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  ylab("steps")
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

```
## Warning: Removed 2304 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 2304 rows containing missing values (geom_point).
```

![](PA1_template_files/figure-html/interval-activity-enhanced-1.png)<!-- -->

We can see that on average, the highest activity is in the interval between 500-1000
with a second small peak in an interval 1500-2000.

Maximum number of steps (206.1698113) across all days 
is in an iterval 835.


```r
activity_per_interval %>% 
  arrange(desc(steps)) %>% 
  head(1)
```

```
## # A tibble: 1 x 2
##   interval steps
##      <dbl> <dbl>
## 1      835  206.
```


## Imputing missing values

There are 2304 rows with missing values for `steps` variable.
That is around 13% of missing values.


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
# round(mean(is.na(activity$steps))*100)
```

The missing values for steps are imputed using a mean value of that day where possible,
for days were all the values are missing (thus it is impossible to get a mean),
the overall mean is used.


```r
# mean steps for the whole dataset
mean_steps_all <- activity %>% 
  summarise(mean = mean(steps, na.rm = TRUE)) %>% 
  pull(mean)

# mean steps for each day
mean_steps_day <- activity_per_day %>% 
  select(date, mean) %>% 
  mutate(mean = if_else(is.na(mean), mean_steps_all, mean))

# imputing missing values
activity_imputed <- activity %>% 
  mutate(missing = if_else(is.na(steps), date, as.Date(NA))) %>% 
  left_join(mean_steps_day, by = c("missing" = "date")) %>% 
  transmute(date, interval,
            steps = if_else(is.na(steps), mean, steps))

head(activity_imputed)
```

```
## # A tibble: 6 x 3
##   date       interval steps
##   <date>        <dbl> <dbl>
## 1 2012-10-01        0  37.4
## 2 2012-10-01        5  37.4
## 3 2012-10-01       10  37.4
## 4 2012-10-01       15  37.4
## 5 2012-10-01       20  37.4
## 6 2012-10-01       25  37.4
```

Dataset with imputed values gives these results:


```r
activity_imputed_per_day <- activity_imputed %>% 
  group_by(date) %>% 
  summarise(total_steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
activity_imputed_per_day %>% 
  ggplot(aes(x = total_steps)) +
  geom_histogram(fill = "white", color = "black") +
  theme_minimal() +
  xlab("total steps per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/imputed-hist-1.png)<!-- -->


```r
activity_imputed_per_day %>% 
  summarise(mean = mean(total_steps),
            median = median(total_steps))
```

```
## # A tibble: 1 x 2
##     mean median
##    <dbl>  <dbl>
## 1 10766. 10766.
```

In the comparison with the original histogram and summary statistics we see
that imputing missing values with given means articulates the peak in the 
histogram and moves both mean and median to the center of the distribution
on the same value.

The chosen approach is definitely not the best but the overall trends seem 
to stay more less the same as in the original dataset. The midpoint is 
still around 10.000 steps but the results obtained from working with this 
dataset will tend to show higher activity (step count) than the original dataset 
because both mean and median of the imputed values are higher than the original.

## Are there differences in activity patterns between weekdays and weekends?


```r
activity_imputed %>% 
  mutate(weekday = weekdays(date),
         weekpart = if_else(weekday %in% c("Sunday", "Saturday"), "weekend", "weekday"),
         weekpart = as.factor(weekpart)) %>% 
  select(-weekday, -date) %>% 
  group_by(weekpart, interval) %>% 
  summarise(mean_steps = mean(steps)) %>% 
  ggplot(aes(x = interval, y = mean_steps)) +
  geom_line() +
  facet_wrap(vars(weekpart), nrow = 2) +
  ylab("mean steps") +
  theme_minimal()
```

```
## `summarise()` regrouping output by 'weekpart' (override with `.groups` argument)
```

![](PA1_template_files/figure-html/week-activity-1.png)<!-- -->

We can see that on the weekdays the curve starts to go up slightly earlier and after 
the interval of 1000 is not very *wiggly*, on the other hand, during the weekends, 
there are many changes throughout the day, meaning the person does more steps.
