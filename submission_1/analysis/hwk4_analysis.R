if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, lubridate, stringr, readxl, data.table, gdata, tidyverse, dplyr)


# import and clean data 
final.data <- read_rds("data/output/final_ma_data.rds")

final.data.clean <- final.data %>%
  filter(!is.na(avg_enrollment) & (year %in% 2010:2015) & !is.na(partc_score))



# 1. Remove all SNPs, 800-series plans, and prescription drug only plans. Provide a box and whisker plot showing the distribution of plan counts by county over time. 
final.data.clean <- final.data.clean %>%
  filter(
    snp == "No",
    !(planid >= 800 & planid < 900),
    !is.na(partc_score),
    year %in% 2010:2015,
    !is.na(avg_enrollment)
  )

## count plans by county and year 
plan.counts <- final.data.clean %>%
  group_by(fips, year) %>%
  summarise(plan_count = n(), .groups = "drop")


## boxplot of plan counts over time 
plan.counts.plot <- ggplot(plan.counts, aes(x = as.factor(year), y = plan_count)) +
  geom_boxplot() +
  labs(title = "Distribution of Plan Counts by County Over Time",
       x = "Year",
       y = "Number of Plans per County") +
  theme_minimal()
print(plan.counts.plot)




# 2. Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. 
## filter data for selected years and count plans by star rating
star.dist <- final.data.clean %>%
  filter(year %in% c(2010, 2012, 2015)) %>%
  filter(!is.na(Star_Rating))

star.dist <- star.dist %>%
  group_by(year, Star_Rating) %>%
  summarise(count = n(), .groups = "drop")

## create combined bar plot
star.dist.plot <- ggplot(star.dist, aes(x = as.factor(Star_Rating), y = count, fill = as.factor(Star_Rating))) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  facet_wrap(~ year) +  
  labs(title = "Distribution of Star Ratings by Year",
       x = "Star Rating",
       y = "Count of Plans") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") 

print(star.dist.plot)

## create seperate bar plots 
star.dist.10 <- ggplot(subset(star.dist, year == 2010), aes(x = as.factor(Star_Rating), y = count, fill = as.factor(Star_Rating))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Star Rating Distribution in 2010", x = "Star Rating", y = "Count of Plans") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
print(star.dist.10)

star.dist.12 <- ggplot(subset(star.dist, year == 2012), aes(x = as.factor(Star_Rating), y = count, fill = as.factor(Star_Rating))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Star Rating Distribution in 2012", x = "Star Rating", y = "Count of Plans") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
print(star.dist.12)

star.dist.15 <- ggplot(subset(star.dist, year == 2015), aes(x = as.factor(Star_Rating), y = count, fill = as.factor(Star_Rating))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Star Rating Distribution in 2015", x = "Star Rating", y = "Count of Plans") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
print(star.dist.15)




# 3. Plot the average benchmark payment over time from 2010 through 2015.
## filter data 
avg.benchmark <- final.data.clean %>%
  group_by(year) %>%
  summarise(avg_benchmark = mean(ma_rate, na.rm = TRUE))

## plot of average benchmark payments over time
bench.plt <- ggplot(avg.benchmark, aes(x = year, y = avg_benchmark)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  labs(
    title = "Average Benchmark Payment for MA Plans (2010–2015)",
    x = "Year",
    y = "Average Benchmark Payment ($)"
  ) +
  theme_minimal()

print(bench.plt)




# 4. Plot the average share of Medicare Advantage (relative to all Medicare eligibles) over time from 2010 through 2015. 
adv.share <- final.data.clean %>%
  filter(!is.na(avg_enrolled), !is.na(avg_eligibles)) %>%
  mutate(ma_share = avg_enrolled / avg_eligibles) %>%
  group_by(year) %>%
  summarise(avg_ma_share = mean(ma_share, na.rm = TRUE))

## Line plot of average MA share over time
adv.share.plt <- ggplot(adv.share, aes(x = year, y = avg_ma_share)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "darkgreen", size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Average Medicare Advantage Share of Medicare Eligibles (2010–2015)",
    x = "Year",
    y = "MA Share"
  ) +
  theme_minimal()

print(adv.share.plt)




#### 2010 ONLY 

# 5. Calculate the running variable underlying the star rating. Provide a table showing the number of plans that are rounded up into a 3-star, 3.5-star, 4-star, 4.5-star, and 5-star rating.
rating.2010 <- final.data.clean %>%
  filter(year == 2010, !is.na(partc_score)) %>%
  count(partc_score) %>%
  filter(partc_score %in% c(3, 3.5, 4, 4.5, 5)) %>%
  rename(`Rounded Rating` = partc_score, `Number of Plans` = n)
print(rating.2010)

library(knitr)
kable(rating.2010, col.names = c("Rounded Rating", "Number of Plans"), caption = "Number of Plans Rounded to Each Star Rating")




# 6. a) Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments. 
install.packages("rdrobust")
library(rdrobust)

ma.rd1 <- final.data.clean %>%
  filter(Star_Rating == 2.5 | Star_Rating == 3)

est_3 <- rdrobust(y = ma.rd1$avg_enrollment, x = ma.rd1$partc_score, c = 3.5,
                  h = 0.125, p = 1, kernel = "uniform", vce = "hc0",
                  masspoints = "off")

sum(is.na(ma.rd1$avg_enrollment))

# 6. b) Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results in a table.