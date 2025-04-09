if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, lubridate, stringr, readxl, data.table, gdata, tidyverse, dplyr)


# import and clean data 
final.data <- read_rds("data/output/final_ma_data.rds")

final.data.clean <- final.data %>%
  filter((year %in% 2010:2015) & !is.na(partc_score))
colnames(final.data.clean) 

# 1. Remove all SNPs, 800-series plans, and prescription drug only plans. Provide a box and whisker plot showing the distribution of plan counts by county over time. 
final.data.clean <- final.data.clean %>%
  filter(snp == "No", 
        !(planid >= 800 & planid < 900),
        !(partd == "Y" & plan_type == "PDP")
  )

## count plans by county and year 
county_plan_counts <- final.data.clean %>%
  group_by(year, state, county) %>%
  summarise(plan_count = n(), .groups = "drop")


## boxplot of plan counts over time 
plan_counts_plot <- ggplot(county_plan_counts, aes(x = as.factor(year), y = plan_count)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  coord_cartesian(ylim = c(0, 70)) +
  labs(x = "Year",
       y = "Number of Plans per County") +
  theme_minimal()
print(plan_counts_plot)




# 2. Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. 
## filter data for selected years and count plans by star rating
star_dist <- final.data.clean %>%
  filter(year %in% c(2010, 2012, 2015)) %>%
  group_by(year, Star_Rating) %>%
  summarise(count = n(), .groups = "drop") 


star_dist_plot <- ggplot(star_dist, aes(x = as.factor(Star_Rating), y = count, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Star Rating", y = "Count of Plans", fill = "Year") +
  theme_minimal()
print(star_dist_plot)




# 3. Plot the average benchmark payment over time from 2010 through 2015.
## filter data 
av_bench <- final.data.clean %>%
  group_by(year) %>%
  summarise(avg_benchmark = mean(ma_rate, na.rm = TRUE))

## plot of average benchmark payments over time
bench_plt <- ggplot(av_bench, aes(x = year, y = avg_benchmark)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  coord_cartesian(ylim = c(500, 900)) +
  labs(x = "Year",
       y = "Average Benchmark Payment ($)") +
  theme_minimal() 
print(bench_plt)



# 4. Plot the average share of Medicare Advantage (relative to all Medicare eligibles) over time from 2010 through 2015. 
avg_share <- final.data.clean %>%
  filter(avg_eligibles > 0, avg_enrolled >= 0) %>%
  mutate(ma_share = avg_enrolled / avg_eligibles) %>%
  group_by(year) %>%
  summarize(avg_ma_share = mean(ma_share, na.rm = TRUE))

## Line plot of average MA share over time
adv_share_plt <- ggplot(avg_share, aes(x = year, y = avg_ma_share)) +
  geom_line(color = "#008cff", size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Year",
       y = "MA Share") +
  theme_minimal(base_size = 14)
print(adv_share_plt) 



#### 2010 ONLY 
data_2010 <- final.data %>%
             filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score)) %>%
  distinct(contractid, planid, county, .keep_all = TRUE)
###colnames(data_2010)

# 5. Calculate the running variable underlying the star rating. Provide a table showing the number of plans that are rounded up into a 3-star, 3.5-star, 4-star, 4.5-star, and 5-star rating.

### calculate raw average 
data_2010 <- data_2010 %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen, rectalcancer_screen, cv_diab_cholscreen, glaucoma_test,
          monitoring, flu_vaccine, pn_vaccine, physical_health, mental_health,
          osteo_test, physical_monitor, primaryaccess, osteo_manage,
          diab_healthy, bloodpressure, ra_manage, copd_test, bladder,
          falling, nodelays, doctor_communicate, carequickly, customer_service,                    
          overallrating_care, overallrating_plan, complaints_plan, appeals_timely,
          appeals_review, leave_plan, audit_problems, hold_times, info_accuracy,
          ttyt_available),
    na.rm=T)) %>%
    select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, partd, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate, plan_type) %>% 
    mutate(mkt_share = avg_enrollment/avg_eligibles, 
          HMO=str_detect(plan_type, "HMO"))
colnames(data_2010)

### how many were rounded up? 
data_2010_round <- data_2010 %>%
  mutate(rounded_30=ifelse(raw_rating>=2.75 & raw_rating<3.00 & Star_Rating==3.0,1,0), 
         rounded_35=ifelse(raw_rating>=3.25 & raw_rating<3.50 & Star_Rating==3.5,1,0),
         rounded_40=ifelse(raw_rating>=3.75 & raw_rating<4.00 & Star_Rating==4.0,1,0),
         rounded_45=ifelse(raw_rating>=4.25 & raw_rating<4.50 & Star_Rating==4.5,1,0), 
         rounded_50=ifelse(raw_rating>=4.75 & raw_rating<5.00 & Star_Rating==5.0,1,0)) %>%
  group_by(Star_Rating) %>% 
  filter(Star_Rating %in% c(3, 3.5, 4, 4.5, 5)) %>% 
  summarize(count_30=sum(rounded_30), 
            count_35=sum(rounded_35), 
            count_40=sum(rounded_40), 
            count_45=sum(rounded_45),
            count_50=sum(rounded_50))%>% 
  mutate(rounded_up=count_30 + count_35 + count_40 + count_45 + count_50) %>% 
  select(Star_Rating, rounded_up)

### table of rounded ratings 
kable(data_2010_round, caption="Number of ratings that were rounded up")



# 6. a) Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments. 
rd_30 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.125), 
                         raw_rating<=(2.75+0.125), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 
summary(rd_30)

# 6. b) Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results in a table.
rd_35 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.125), 
                         raw_rating<=(3.25+0.125), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))
summary(rd_35)

# create a table from both 
### extract tidy results
tidy_30 <- tidy(rd_30)
tidy_35 <- tidy(rd_35)

### merge both into one table
table_6 <- full_join(
  tidy_30 %>% select(term, estimate, std.error) %>% rename(Estimate_3 = estimate, SE_3 = std.error),
  tidy_35 %>% select(term, estimate, std.error) %>% rename(Estimate_3.5 = estimate, SE_3.5 = std.error),
  by = "term"
)

### format table
table_6 %>%
  mutate(
    Estimate_3 = sprintf("%.4f", Estimate_3),
    SE_3 = sprintf("(%.4f)", SE_3),
    Estimate_3.5 = sprintf("%.4f", Estimate_3.5),
    SE_3.5 = sprintf("(%.4f)", SE_3.5)
  ) %>%
  select(term, Estimate_3, SE_3, Estimate_3.5, SE_3.5) %>%
  kable(col.names = c("", "3 Star", "", "3.5 Star", ""),
        caption = "Table 6: RD Estimates by Star Rating",
        align = "lcccc") %>%
  kable_styling(full_width = FALSE, position = "left")



# 7. Repeat your results for bandwidhts of 0.1, 0.12, 0.13, 0.14, and 0.15 (again for 3 and 3.5 stars).
rd_30_1 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.1), 
                         raw_rating<=(2.75+0.1), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 
summary(rd_30_1)

rd_30_12 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.12), 
                         raw_rating<=(2.75+0.12), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 
summary(rd_30_12)

rd_30_13 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.13), 
                         raw_rating<=(2.75+0.13), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 
summary(rd_30_13)

rd_30_14 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.14), 
                         raw_rating<=(2.75+0.14), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 
summary(rd_30_14)

rd_30_15 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.15), 
                         raw_rating<=(2.75+0.15), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 
summary(rd_30_15)



rd_35_1 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.1), 
                         raw_rating<=(3.25+0.1), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))
summary(rd_35_1)

rd_35_12 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.12), 
                         raw_rating<=(3.25+0.12), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))
summary(rd_35_12)

rd_35_13 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.13), 
                         raw_rating<=(3.25+0.13), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))
summary(rd_35_13)

rd_35_14 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.14), 
                         raw_rating<=(3.25+0.14), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))
summary(rd_35_14)

rd_35_15 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.15), 
                         raw_rating<=(3.25+0.15), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))
summary(rd_35_15) 


results_df <- tibble(
  Cutoff = rep(c("3 vs 2.5 Stars", "3.5 vs 3 Stars"), each = 5),
  Bandwidth = rep(c(0.10, 0.12, 0.13, 0.14, 0.15), 2),
  Estimate = c(
    tidy(rd_30_1)$estimate[2],
    tidy(rd_30_12)$estimate[2],
    tidy(rd_30_13)$estimate[2],
    tidy(rd_30_14)$estimate[2],
    tidy(rd_30_15)$estimate[2],
    tidy(rd_35_1)$estimate[2],
    tidy(rd_35_12)$estimate[2],
    tidy(rd_35_13)$estimate[2],
    tidy(rd_35_14)$estimate[2],
    tidy(rd_35_15)$estimate[2]
  ),
  SE = c(
    tidy(rd_30_1)$std.error[2],
    tidy(rd_30_12)$std.error[2],
    tidy(rd_30_13)$std.error[2],
    tidy(rd_30_14)$std.error[2],
    tidy(rd_30_15)$std.error[2],
    tidy(rd_35_1)$std.error[2],
    tidy(rd_35_12)$std.error[2],
    tidy(rd_35_13)$std.error[2],
    tidy(rd_35_14)$std.error[2],
    tidy(rd_35_15)$std.error[2]
  )
)

q7_fig <- ggplot(results_df, aes(x = Bandwidth, y = Estimate, color = Cutoff)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * SE,
                    ymax = Estimate + 1.96 * SE), width = 0.005) +
  labs(
    title = "RDD Treatment Effects Across Bandwidths",
    x = "Bandwidth (+/-)",
    y = "Estimated Treatment Effect on Market Share"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("3 vs 2.5 Stars" = "darkblue", "3.5 vs 3 Stars" = "firebrick"))

print(q7_fig) 


# 8. Examine (graphically) whether contracts appear to manipulate the running variable. In other words, look at the distribution of the running variable before and after the relevent threshold values.
library(gridExtra) 

### subset data around each cutoff
cutoff_3 <- data_2010 %>% filter(raw_rating >= 2.75 & raw_rating < 3.25)
cutoff_35 <- data_2010 %>% filter(raw_rating >= 3.25 & raw_rating < 3.75)

### plot around 3.0 cutoff
dist_3 <- ggplot(cutoff_3, aes(x = raw_rating)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  geom_vline(xintercept = 3.0, linetype = "dashed") +
  labs(title = "(a) Around 3.0 cutoff", x = "Running Variable", y = "Density") +
  theme_minimal()

### plot around 3.5 cutoff
dist_35 <- ggplot(cutoff_35, aes(x = raw_rating)) +
  geom_density(fill = "lightgreen", alpha = 0.6) +
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  labs(title = "(b) Around 3.5 cutoff", x = "Running Variable", y = "Density") +
  theme_minimal()

### combine side-by-side
dist_plot <- grid.arrange(dist_3, dist_35, ncol = 2, top = "Density of Running Variable")
print(dist_plot)


# 9. Similar to question 4, examine whether plans just above the threshold values have different characteristics than contracts just below the threshold values. Use HMO and Part D status as your plan characteristics.
### create binary indicators if not already
data_2010 <- data_2010 %>%
  mutate(HMO_binary = as.numeric(HMO),
         PartD_binary = as.numeric(partd == "Y"))

get_balance_data <- function(data, cutoff, low_star, high_star, vars) {
  data_filtered <- data %>%
    filter(raw_rating >= (cutoff - 0.125) & raw_rating <= (cutoff + 0.125)) %>%
    filter(Star_Rating %in% c(low_star, high_star)) %>%
    mutate(treat = as.numeric(Star_Rating == high_star))

  map_dfr(vars, function(v) {
    means <- data_filtered %>%
      group_by(treat) %>%
      summarize(mean = mean(.data[[v]], na.rm = TRUE), .groups = "drop")

    tibble(
      variable = v,
      mean_diff = means$mean[2] - means$mean[1]
    )
  }) %>%
    mutate(variable = recode(variable, HMO_binary = "HMO", PartD_binary = "Part D"))
}

### get data for both cutoffs
vars <- c("HMO_binary", "PartD_binary")
balance_3.0 <- get_balance_data(data_2010, 2.75, 2.5, 3.0, vars)
balance_3.5 <- get_balance_data(data_2010, 3.25, 3.0, 3.5, vars)


plot_3 <- ggplot(balance_3.0, aes(x = mean_diff, y = variable)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(title = "(a) Around 3.0 cutoff", x = "Mean Differences", y = NULL) +
  xlim(-0.4, 0.4) +
  theme_minimal(base_size = 14)

plot_35 <- ggplot(balance_3.5, aes(x = mean_diff, y = variable)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(title = "(b) Around 3.5 cutoff", x = "Mean Differences", y = NULL) +
  xlim(-0.4, 0.4) +
  theme_minimal(base_size = 14)

### arrange side by side
char_plot <- grid.arrange(plot_3, plot_35, ncol = 2, top = "Covariate Balance")



rm(list = setdiff(ls(), c("plan_counts_plot", "star_dist_plot", "bench_plt", "adv_share_plt", "data_2010_round", "table_6", "q7_fig", "dist_plot", "dist_3", "dist_35", "char_plot", "plot_3", "plot_35")))
save.image("submission_2/results/hwk4_workspace.RData")






