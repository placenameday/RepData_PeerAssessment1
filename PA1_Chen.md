---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
# Chen's first Rmarkdown   
***

## Loading the essenstial package   

```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
## ✓ tibble  3.1.6     ✓ dplyr   1.0.8
## ✓ tidyr   1.2.0     ✓ stringr 1.4.0
## ✓ readr   2.1.2     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(scales)
```

```
## 
## Attaching package: 'scales'
```

```
## The following object is masked from 'package:purrr':
## 
##     discard
```

```
## The following object is masked from 'package:readr':
## 
##     col_factor
```
   
## Loading and preprocessing the data

```r
dt <- read_csv("activity.zip")
```

```
## Rows: 17568 Columns: 3
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## What is mean total number of steps taken per day?

```r
step <- dt %>% group_by(date) %>% summarise(sum = sum(steps, na.rm = TRUE))

p <- ggplot(step, aes(x = date, y=sum)) +
  geom_col(fill = "pink", colour = "brown", size = .3) +
  xlab("Date(2012)") +
  ylab("Total steps per day") +
  ggtitle("Total Number of Steps Taken per Day") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 week",labels=date_format("%m-%d"))
p
```

![](PA1_Chen_files/figure-html/steps per day-1.png)<!-- -->

```r
cat("The Mean of the total number of steps/day is ", mean(step$sum, na.rm = T),".", sep = "")
```

```
## The Mean of the total number of steps/day is 9354.23.
```

```r
cat("The Median of the total number of steps/day is ", median(step$sum, na.rm = T),".", sep = "")
```

```
## The Median of the total number of steps/day is 10395.
```
   
   
## What is the average daily activity pattern?

```r
step <- dt %>% group_by(interval) %>%
  summarise(mean = mean(steps, na.rm = TRUE)) %>%
  mutate(time = seq(0,1435,5)/60)

p <- ggplot(step,aes(x=time,y=mean)) + geom_line() +
  scale_x_continuous(breaks=seq(0, 24, 2)) +
  labs(x = "24 Hours a Day", y = "Average Number of Steps", title ="Average Daily Activity Pattern") +
  theme(plot.title = element_text(hjust = 0.5))
p
```

![](PA1_Chen_files/figure-html/daily activity pattern-1.png)<!-- -->

```r
cat("On average across all the days in the dataset **", step[which.max(step$mean),1][[1]], 
    "** interval contains the maximum number of steps.", sep = "")
```

```
## On average across all the days in the dataset **835** interval contains the maximum number of steps.
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
