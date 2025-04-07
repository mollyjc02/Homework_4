if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, lubridate, stringr, readxl, data.table, gdata, tidyverse, dplyr)


# import and clean data 
final.data <- read_rds("data/output/final_ma_data.rds")

final.data.clean <- final.data %>%
  filter(!is.na(avg_enrollment) & (year %in% 2010:2015) & !is.na(partc_score))
colnames(final.data.clean)


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
  labs(x = "Year",
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
  labs(x = "Star Rating",
       y = "Count of Plans") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") 

print(star.dist.plot)

## create seperate bar plots 
star.dist.10 <- ggplot(subset(star.dist, year == 2010), aes(x = as.factor(Star_Rating), y = count, fill = as.factor(Star_Rating))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Star Rating", y = "Count of Plans") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
print(star.dist.10)

star.dist.12 <- ggplot(subset(star.dist, year == 2012), aes(x = as.factor(Star_Rating), y = count, fill = as.factor(Star_Rating))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Star Rating", y = "Count of Plans") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
print(star.dist.12)

star.dist.15 <- ggplot(subset(star.dist, year == 2015), aes(x = as.factor(Star_Rating), y = count, fill = as.factor(Star_Rating))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Star Rating", y = "Count of Plans") +
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
  labs(x = "Year",
       y = "Average Benchmark Payment ($)") +
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
  labs(x = "Year",
       y = "MA Share") +
  theme_minimal()

print(adv.share.plt)





#### 2010 ONLY 
data.2010 <- final.data %>%
             filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score))
colnames(data.2010)

# 5. Calculate the running variable underlying the star rating. Provide a table showing the number of plans that are rounded up into a 3-star, 3.5-star, 4-star, 4.5-star, and 5-star rating.

### calculate raw average 
data.2010 <- data.2010 %>%
  mutate(raw.rating=rowMeans(
    cbind(breastcancer_screen, rectalcancer_screen, cv_diab_cholscreen, glaucoma_test,
          monitoring, flu_vaccine, pn_vaccine, physical_health, mental_health,
          osteo_test, physical_monitor, primaryaccess, osteo_manage,
          diab_healthy, bloodpressure, ra_manage, copd_test, bladder,
          falling, nodelays, doctor_communicate, carequickly, customer_service,                    
          overallrating_care, overallrating_plan, complaints_plan, appeals_timely,
          appeals_review, leave_plan, audit_problems, hold_times, info_accuracy,
          ttyt_available),
    na.rm=T)) %>%
    select(contractid, planid, fips, avg_enrollment, state, county, raw.rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate, plan_type) %>% 
    mutate(mkt_share = avg_enrollment/avg_eligibles, 
          HMO=str_detect(plan_type, "HMO"))
colnames(data.2010)

### how many were rounded up? 
data.2010 <- data.2010 %>%
  mutate(rounded.up = Star_Rating > raw.rating)

### table of rounded ratings 
rounded.up.table <- data.2010 %>%
  filter(rounded.up == TRUE) %>%
  filter(Star_Rating %in% c(3, 3.5, 4, 4.5, 5)) %>%
  group_by(Star_Rating) %>%
  summarise(num_rounded_up = n()) %>%
  arrange(Star_Rating)
rounded.up.table




# 6. a) Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments. 
library(rdrobust)

ma.rd1 <- data.2010 %>%
  filter(Star_Rating==2.5 | Star_Rating==3) 

ma.rd1 <- ma.rd1 %>%
  mutate(
    score = raw.rating - 2.75,
    treat = (score >= 0), 
    window2 = (score >= -0.125 & score <= 0.125),
    mkt_share = avg_enrollment / avg_eligibles, 
    ln_share = log(mkt_share), 
    score_treat = score * treat 
  )

rd1_result <- rdrobust(ma.rd1$avg_enrollment, ma.rd1$score, c = 0, h = 0.125)
rd1_result



# 6. b) Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results in a table.
ma.rd2 <- data.2010 %>%
  filter(Star_Rating==3 | Star_Rating==3.5) 

ma.rd2 <- ma.rd2 %>%
  mutate(
    score = raw.rating - 3.25,
    treat = (score >= 0), 
    window2 = (score >= -0.125 & score <= 0.125),
    mkt_share = avg_enrollment / avg_eligibles, 
    ln_share = log(mkt_share), 
    score_treat = score * treat 
  )

rd2_result <- rdrobust(ma.rd2$avg_enrollment, ma.rd2$score, c = 0, h = 0.125)
rd2_result







###rm(list = setdiff(ls(), c("plan.counts.plot", "star.dist.plot", "star.dist.10", "star.dist.12", "star.dist.15", "bench.plt", "adv.share.plt", "rating.2010", "")))
save.image("submission_1/results/hwk4_workspace.RData")
