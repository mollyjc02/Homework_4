if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, lubridate, stringr, readxl, data.table, gdata, tidyverse, dplyr)


# import and clean data 
final.data <- read_rds("data/output/final_ma_data.rds")

final.data.clean <- final.data %>%
  filter((year %in% 2010:2015) & !is.na(partc_score))  %>%
  distinct(contractid, planid, county, .keep_all = TRUE)



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
  coord_cartesian(ylim = c(0, 50)) +
  labs(x = "Year",
       y = "Number of Plans per County") +
  theme_minimal()
print(plan_counts_plot) 




# 2. Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. 
## filter data for selected years and count plans by star rating
star_dist <- final.data.clean %>%
  filter(year %in% c(2010, 2012, 2015), !is.na(Star_Rating)) %>%
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
avg_share <- final.data %>%
  filter(avg_eligibles > 0, avg_enrolled >= 0) %>%
  mutate(ma_share = avg_enrolled / avg_eligibles) %>%
  group_by(year) %>%
  summarize(avg_ma_share = mean(ma_share, na.rm = TRUE))

## Line plot of average MA share over time
library(scales)

adv_share_plt <- ggplot(avg_share, aes(x = year, y = avg_ma_share)) +
  geom_line(color = "#008cff", size = 1) +
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
library(knitr)
kable(data_2010_round)



# 6. a) Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments. 
rd_30 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(2.75-0.125), 
                         raw_rating<=(2.75+0.125), 
                         Star_Rating %in% c(2.5, 3.0)) %>% 
                  mutate(treat=(Star_Rating==3.0), 
                  score=raw_rating-2.75))) 

# 6. b) Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results in a table.
rd_35 <- lm(mkt_share ~ treat + score, 
            data=(data_2010 %>% 
                  filter(raw_rating>=(3.25-0.125), 
                         raw_rating<=(3.25+0.125), 
                         Star_Rating %in% c(3.0, 3.5)) %>% 
                  mutate(treat=(Star_Rating==3.5), 
                  score=raw_rating-3.25)))

# create a table from both 
library(modelsummary)

### merge both into one table
models <- list(
  "3 Stars" = rd_30,
  "3.5 Stars" = rd_35)

### show clean table with modelsummary
table_6 <- modelsummary(models,
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "Adj|Log|F|AIC|BIC", # keep only obs, R2, RMSE
             title = "Table 6: RD Estimates by Star Rating",
             output = "kableExtra")
print(table_6)



# 7. Repeat your results for bandwidhts of 0.1, 0.12, 0.13, 0.14, and 0.15 (again for 3 and 3.5 stars).
library(purrr)

### define bandwidths
bandwidths <- c(0.10, 0.12, 0.13, 0.14, 0.15)

### function to fit RD model
run_rd <- function(center, treat_val, control_val, bw) {
  lm(mkt_share ~ treat + score,
     data = data_2010 %>%
       filter(raw_rating >= (center - bw),
              raw_rating <= (center + bw),
              Star_Rating %in% c(treat_val, control_val)) %>%
       mutate(treat = Star_Rating == treat_val,
              score = raw_rating - center))
}

### Estimate for 3 Stars vs 2.5
rd_30_models <- map(bandwidths, ~run_rd(center = 2.75, treat_val = 3.0, control_val = 2.5, bw = .x))
names(rd_30_models) <- paste0("rd_30_bw_", bandwidths)

### estimate for 3.5 Stars vs 3.0
rd_35_models <- map(bandwidths, ~run_rd(center = 3.25, treat_val = 3.5, control_val = 3.0, bw = .x))
names(rd_35_models) <- paste0("rd_35_bw_", bandwidths) 

models_all <- c(rd_30_models, rd_35_models)

### add labels to distinguish model sets
library(broom)

results_df <- bind_rows(
  map2_dfr(rd_30_models, bandwidths, ~{
    tidy(.x)[2, ] %>%  # Row 2 is for `treat`
      mutate(
        Cutoff = "3 vs 2.5 Stars",
        Bandwidth = .y
      )
  }),
  map2_dfr(rd_35_models, bandwidths, ~{
    tidy(.x)[2, ] %>%
      mutate(
        Cutoff = "3.5 vs 3 Stars",
        Bandwidth = .y
      )
  })
) %>%
  select(Cutoff, Bandwidth, Estimate = estimate, SE = std.error)

q7_fig <- ggplot(results_df, aes(x = Bandwidth, y = Estimate, color = Cutoff)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * SE,
                    ymax = Estimate + 1.96 * SE), width = 0.005) +
  labs(
    x = "Bandwidth (+/-)",
    y = "Estimated Treatment Effect"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("3 vs 2.5 Stars" = "darkblue", "3.5 vs 3 Stars" = "firebrick"))
print(q7_fig)



# 8. Examine (graphically) whether contracts appear to manipulate the running variable. In other words, look at the distribution of the running variable before and after the relevent threshold values.
library(gridExtra) 

### subset data around each cutoff
cutoff_3 <- data_2010 %>% filter(raw_rating > 2.5 & raw_rating < 3)
cutoff_35 <- data_2010 %>% filter(raw_rating > 3 & raw_rating < 3.5)

### plot around 3.0 cutoff
dist_3 <- ggplot(cutoff_3, aes(x = raw_rating)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  geom_vline(xintercept = 2.75, linetype = "dashed") +
  labs(title = "(a) Around 3.0 cutoff", x = "Running Variable", y = "Density") +
  theme_minimal()

### plot around 3.5 cutoff
dist_35 <- ggplot(cutoff_35, aes(x = raw_rating)) +
  geom_density(fill = "lightgreen", alpha = 0.6) +
  geom_vline(xintercept = 3.25, linetype = "dashed") +
  labs(title = "(b) Around 3.5 cutoff", x = "Running Variable", y = "Density") +
  theme_minimal()

### combine side-by-side
dist_plot <- grid.arrange(dist_3, dist_35, ncol = 2, top = "Density of Running Variable")



# 9. Similar to question 4, examine whether plans just above the threshold values have different characteristics than contracts just below the threshold values. Use HMO and Part D status as your plan characteristics.
library(cobalt)
library(patchwork)

### 3 vs 2.5 star threshold
lp_30 <- data_2010 %>%
  filter((raw_rating >= 2.75 - 0.125) & (raw_rating <= 2.75 + 0.125)) %>%
  filter(Star_Rating %in% c(2.5, 3.0)) %>%
  mutate(rounded = (Star_Rating == 3.0)) %>%
  select(HMO, partd, rounded) %>%
  filter(complete.cases(.))

plot_30 <- love.plot(
  bal.tab(lp_30 %>% select(HMO, partd), treat = lp_30$rounded),
  colors = "black", shapes = "circle", threshold = 0.1
) +
  labs(title = "(a) 3-Star Cutoff") +
  theme_bw() + theme(legend.position = "none")

### 3.5 vs 3 star threshold
lp_35 <- data_2010 %>%
  filter((raw_rating >= 3.25 - 0.125) & (raw_rating <= 3.25 + 0.125)) %>%
  filter(Star_Rating %in% c(3.0, 3.5)) %>%
  mutate(rounded = (Star_Rating == 3.5)) %>%
  select(HMO, partd, rounded) %>%
  filter(complete.cases(.))

plot_35 <- love.plot(
  bal.tab(lp_35 %>% select(HMO, partd), treat = lp_35$rounded),
  colors = "black", shapes = "circle", threshold = 0.1
) +
  labs(title = "(b) 3.5-Star Cutoff") +
  theme_bw() + theme(legend.position = "none")

### combine side-by-side
combined_plot <- plot_30 + plot_35 + plot_layout(ncol = 2)
print(combined_plot)



rm(list = setdiff(ls(), c("plan_counts_plot", "star_dist_plot", "bench_plt", "adv_share_plt", "data_2010_round", "models", "q7_fig", "dist_plot", "dist_3", "dist_35", "combined_plot", "plot_30", "plot_35")))
save.image("submission_final/results/hwk4_workspace.RData") 
