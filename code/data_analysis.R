# import needed libraries
library(dplyr)
library(ggplot2)
library(lubridate)
# install.packages("GGally")
library(GGally)
# install.packages("zoo")
library(zoo)
# install.packages("patchwork")
library(patchwork)

# Load data
unemployment_data <- read.csv("/Users/noraadadurova/Desktop/Math_261A/Project_2/data/clean_unemployment_data.csv", 
                              header = TRUE)

# let's see the data
head(unemployment_data)

# let's see summary statistics
summary(select(unemployment_data,
               Insured.Unemployment.Rate,
               Initial.Claims,
               Continued.Claims,
               Initial.Claims.Rate,
               Continued.Claims.Rate,
               Covered.Employment))

# let's check what is the range of this data
# first and last date collected
range(unemployment_data$Filed.week.ended)

# note: the result is from "1987-01-03" to "2023-01-28"
# so, it has COVID-19 and it has financial crises of 2008

# let's also find the range of the columns
# Initial.Claims
range(unemployment_data$Initial.Claims)
# form "18046" to "1058325"

# Continued.Claims
range(unemployment_data$Continued.Claims)
# from "221559" to "4808361"

# Covered.Employment
range(unemployment_data$Covered.Employment)
# from 11046965 to 17473068

# Insured.Unemployment.Rate
range(unemployment_data$Insured.Unemployment.Rate)
# from 1.50 to 27.75
# note: 27.75% seems super hight
# so, I googled and in April 2020 the overall unemployment was 16.1% in California
# hence, maybe 27.75% for insured unemplyment is not as crazy as it initially seemed

# find minimum insured unemployment rate and its date
min_iur <- unemployment_data |>
  filter(Insured.Unemployment.Rate == min(Insured.Unemployment.Rate, 
                                          na.rm = TRUE)) |>
  select(Filed.week.ended, 
         Insured.Unemployment.Rate)

min_iur

# find maximum insured unemployment rate and its date
max_iur <- unemployment_data |>
  filter(Insured.Unemployment.Rate == max(Insured.Unemployment.Rate, 
                                          na.rm = TRUE)) |>
  select(Filed.week.ended, 
         Insured.Unemployment.Rate)

max_iur

# Initial.Claims.Rate
range(unemployment_data$Initial.Claims.Rate)
# from 0.001228211 to 0.061298732

# Continued.Claims.Rate
range(unemployment_data$Continued.Claims.Rate)
# from 0.01498808 to 0.27745864

# let's plot outcome variable
# first, let's make sure that data is sorted by date
unemployment_data <- unemployment_data |>
  arrange(Filed.week.ended)

# now let's make the plot
unemployment_data$Filed.week.ended <- ymd(unemployment_data$Filed.week.ended)
ggplot(unemployment_data, 
       aes(Filed.week.ended, Insured.Unemployment.Rate, 
           group = 1)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_breaks = "5 years", 
               date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Insured Unemployment Rate Over Time",
    x = "Year",
    y = "Insured Unemployment Rate (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

# let's find the date with hiegst unemplyemnt
unemployment_data |>
  filter(Insured.Unemployment.Rate == max(Insured.Unemployment.Rate, 
                                          na.rm = TRUE)) |>
  select(Filed.week.ended, 
         Insured.Unemployment.Rate)
# googeling showed April 2020 and data shows May
# I think this is a resonable result considerign that data collection mitgh ahve delays
# also, people fillign for insurance takes some time

# let's now see the distributions of diffrent valibles 
# let's use histograms 
# Histogram for raw Initial Claims
p1 <- ggplot(unemployment_data, 
       aes(x = Initial.Claims)) +
  geom_histogram(bins = 30, 
                 fill = "steelblue", 
                 alpha = 0.7) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribution of Initial Claims", 
       x = "Initial Claims", 
       y = "Frequency") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

# Histogram for Initial Claims Rate
p2 <- ggplot(unemployment_data, 
       aes(x = Initial.Claims.Rate * 100)) +
  geom_histogram(bins = 30, 
                 fill = "coral", 
                 alpha = 0.7) +
  labs(title = "Distribution of Initial Claims Rate", 
       x = "Initial Claims Rate (%)", 
       y = "Frequency") +
  theme_minimal()

# Histogram for raw Continued Claims
p3 <- ggplot(unemployment_data, 
       aes(x = Continued.Claims)) +
  geom_histogram(bins = 30, 
                 fill = "steelblue", 
                 alpha = 0.7) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribution of Continued Claims", 
       x = "Continued Claims", 
       y = "Frequency") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, 
                                 hjust = 1))

# Histogram for Continued Claims Rate
p4 <- ggplot(unemployment_data, 
       aes(x = Continued.Claims.Rate * 100)) +
  geom_histogram(bins = 30, 
                 fill = "coral", 
                 alpha = 0.7) +
  labs(title = "Distribution of Continued Claims Rate", 
       x = "Continued Claims Rate (%)", 
       y = "Frequency") +
  theme_minimal()

# let's make them into one visualization for the paper
combined_hist <- (p1 | p2) /
  (p3 | p4)

combined_hist

# save the linear regression visulization
ggsave("/Users/noraadadurova/Desktop/Math_261A/combined_hist.png", 
       plot = combined_hist, 
       width = 8, 
       height = 5, 
       dpi = 300)

# Seasonal Boxplots
# Boxplot for Insured Unemployment Rate by Month (Raw)
ggplot(unemployment_data, 
       aes(x = Month, 
           y = Insured.Unemployment.Rate)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Seasonal Variation in Insured Unemployment Rate (Raw)", 
       x = "Month", 
       y = "Insured Unemployment Rate (%)")

# Boxplot for Initial Claims Rate by Month
initial_claims_rate_plot <- ggplot(unemployment_data, 
       aes(x = Month, 
           y = Initial.Claims.Rate * 100)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Seasonal Variation in Initial Claims Rate", 
       x = "Month", 
       y = "Initial Claims Rate (%)") + 
  theme_minimal()

initial_claims_rate_plot

# save the linear regression visulization
ggsave("/Users/noraadadurova/Desktop/Math_261A/initial_claims_rate_plot.png", 
       plot = initial_claims_rate_plot, 
       width = 8, 
       height = 5, 
       dpi = 300)

# Boxplot for Continued Claims Rate by Month
ggplot(unemployment_data, 
       aes(x = Month, 
           y = Continued.Claims.Rate)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Seasonal Variation in Continued Claims Rate", 
       x = "Month", 
       y = "Continued Claims Rate")

# all these plots show that data has a lot of outliers
# few different ways to handle them:
# 1. Keep Them for Context
# 2. Use Indicator Variables
# 3. Transformation (like logs and etc.)
# 4. Flag and Analyze Separately
# I think for now it is best to keep them and use indicator varibles to note them
# because given that the outliers seem to align with well-known events (like the COVID-19 spike)
# they represent real world sagnificant events for employment rates in California

# let's make sure that this idea about outlisers is actually correct
# let's find outliers and check their dates
# function to detect outliers using the IQR method
detect_outliers <- function(data, 
                            column) {
  q1 <- quantile(data[[column]], 
                 0.25, 
                 na.rm = TRUE)
  q3 <- quantile(data[[column]], 
                 0.75, 
                 na.rm = TRUE)
  iqr <- q3 - q1
  upper_threshold <- q3 + 1.5 * iqr
  
  # find outliers
  outliers <- data |>
    filter(data[[column]] > upper_threshold)
  
  return(outliers)
}

# apply the function to detect outliers
outliers_unemployment_rate <- detect_outliers(unemployment_data, 
                                              "Insured.Unemployment.Rate")

# show outliers with dates
outliers_unemployment_rate |> 
  select(Filed.week.ended, 
         Insured.Unemployment.Rate)

# note: got 38 values
# all of these values are in 2020
# so, they were coused by COVID-19
# and they do not look to me like mistakes

# let's check usign the same funtion other mesurments
# Initial.Claims
outliers_initial_raw <- detect_outliers(unemployment_data, 
                                        "Initial.Claims") |>
  select(Filed.week.ended, 
         Initial.Claims)

# show the outlires
outliers_initial_raw

# got 60 total outliers
# they al make sence
# 1991: Recession (unemployment spike)
# 1992: recovery year with elevated UI claims
# 1995: seasonal spike (January always has high claims)
# 2009: Great Recession
# 2010: slow recovery (claims still elevated)
# 2020: COVID-19 spike

# Initial.Claims.Rate
outliers_initial_rate <- detect_outliers(unemployment_data, 
                                         "Initial.Claims.Rate") |>
  select(Filed.week.ended, 
         Initial.Claims.Rate)

# show the outlires
outliers_initial_rate

# results:
# the 1990–1991 recession spillover
# 1995-01-14 probably sesonal spike 
# since anuary always exhibits an abnormally high level of UI claims
# 2010-01-16 Post-Great Recession unemployment lingering
# claim rate reached 0.0613 
# so, 6.13% of covered workers filed initial claims in just one week
# according to the data, that was the largest spike in the entire history of unemployment insurance
# this matches the documented UI surge in California and the rest of the U.S.

# Continued.Claims
outliers_continued_raw <- detect_outliers(unemployment_data, 
                                          "Continued.Claims") |>
  select(Filed.week.ended, 
         Continued.Claims)

# show the outlires
outliers_continued_raw

# results:
# Great Recession spikes (2009-2010)
# COVID-19 explosion (2020-2021)
# so, all identified 88 outliers correspond to well-known periods of economic stress

# Continued.Claims.Rate
outliers_continued_rate <- detect_outliers(unemployment_data, 
                                           "Continued.Claims.Rate") |>
  select(Filed.week.ended, 
         Continued.Claims.Rate)

# show the outlires
outliers_continued_rate

# results:
# the extreme spike at 2020-05-02 reaching 27.7% is a real historical fact
# California’s insured unemployment rate reached around 27–30% in spring 2020
# So, the results are 100% consistent with published labor statistics
# Also, it is overall expected that the Great Recession (2008–2010) does NOT appear here
# because during 2008–2010 unemployment rose steadily but covered employment also shrank gradually
# and there were NO sudden collapses like COVID-19 time
# so, continued claims rate did not exceed the IQR threshold during earlier recessions

# next, let's perform correlation analysis
correlation_data <- unemployment_data |>
  select(Insured.Unemployment.Rate, 
         Initial.Claims.Rate, 
         Continued.Claims.Rate)

# compute the correlation matrix
cor_matrix <- cor(correlation_data, 
                  use = "complete.obs")

# print the correlation matrix
print(cor_matrix)

# they all have very hight positive correlations 
# witch does make since considering that they all are unemployment related measures

# Seasonality and Trend Decomposition
# prepare weekly time series
iur_ts <- ts(unemployment_data$Insured.Unemployment.Rate,
             start = c(year(min(unemployment_data$Filed.week.ended)),
                       week(min(unemployment_data$Filed.week.ended))),
             frequency = 52)

# STL decomposition
iur_decomp <- stl(iur_ts, 
                  s.window = "periodic")

plot(iur_decomp, 
     main = "STL Decomposition of Insured Unemployment Rate")

# Monthly Seasonal Plot (Average IUR by Month)
unemployment_data |>
  group_by(Month) |>
  summarize(AvgIUR = mean(Insured.Unemployment.Rate, 
                          na.rm = TRUE)) |>
  ggplot(aes(x = Month, y = AvgIUR)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Insured Unemployment Rate by Month",
    x = "Month",
    y = "Average IUR (%)"
  ) +
  theme_minimal()

# Quarterly Seasonal Plot (Average IUR by Quarter)
unemployment_data |>
  group_by(Quarter) |>
  summarize(AvgIUR = mean(Insured.Unemployment.Rate, na.rm = TRUE)) |>
  ggplot(aes(x = factor(Quarter), y = AvgIUR)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Average Insured Unemployment Rate by Quarter",
    x = "Quarter",
    y = "Average IUR (%)"
  ) +
  theme_minimal()

# rolling average trend plot (52-week rolling mean)
unemployment_data$Rolling_IUR_52 <- rollmean(unemployment_data$Insured.Unemployment.Rate,
                                             k = 52, 
                                             fill = NA)

ggplot(unemployment_data, 
       aes(Filed.week.ended, 
           Rolling_IUR_52)) +
  geom_line(color = "blue") +
  labs(title = "52-Week Rolling Average of IUR")

# next, let's recall Math 265
# let's do Lag Analysis to pick what lags to use for the modeling stage
# Autocorrelation of IUR (do past values predict future values?)
acf(unemployment_data$Insured.Unemployment.Rate,
    main = "ACF of Insured Unemployment Rate")
# Partial Autocorrelation of IUR (helps decide how many lags to include)
pacf(unemployment_data$Insured.Unemployment.Rate,
     main = "PACF of Insured Unemployment Rate")
# Cross-correlation: do claims predict future unemployment?
ccf(unemployment_data$Initial.Claims.Rate,
    unemployment_data$Insured.Unemployment.Rate,
    main = "CCF: Initial Claims Rate to IUR")
ccf(unemployment_data$Continued.Claims.Rate,
    unemployment_data$Insured.Unemployment.Rate,
    main = "CCF: Continued Claims Rate to IUR")

# Compare Periods of Economic Stress
# recall: Recessions from 1990-91, 2001, Great Recession (2007-2009), and COVID-19
# shade recessions on the IUR time-series plot
recessions <- data.frame(
  start = as.Date(c("1990-07-01","2001-03-01","2007-12-01","2020-02-01")),
  end   = as.Date(c("1991-03-01","2001-11-01","2009-06-01","2020-12-26"))
)

overall_IU_with_econ_stress <- ggplot(unemployment_data, 
       aes(Filed.week.ended, 
           Insured.Unemployment.Rate)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", 
               date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_rect(data = recessions,
            aes(xmin = start, 
                xmax = end, 
                ymin = -Inf, 
                ymax = Inf),
            fill = "coral", 
            alpha = 0.3, 
            inherit.aes = FALSE) +
  labs(title = "Insured Unemployment Rate with Recession Periods Highlighted",
       x = "Year", 
       y = "IUR (%)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

overall_IU_with_econ_stress

# save the linear regression visulization
ggsave("/Users/noraadadurova/Desktop/Math_261A/overall_IU_with_econ_stress.png", 
       plot = overall_IU_with_econ_stress, 
       width = 7, 
       height = 4, 
       dpi = 300)

# compare average IUR inside vs outside recessions
unemployment_data <- unemployment_data |>
  mutate(RecessionPeriod =
           case_when(
             Filed.week.ended >= "1990-07-01" & Filed.week.ended <= "1991-03-01" ~ "1990-91",
             Filed.week.ended >= "2001-03-01" & Filed.week.ended <= "2001-11-01" ~ "2001",
             Filed.week.ended >= "2007-12-01" & Filed.week.ended <= "2009-06-01" ~ "2008",
             Filed.week.ended >= "2020-02-01" & Filed.week.ended <= "2020-12-26" ~ "2020",
             TRUE ~ "Non-Recession"
           ))

unemployment_data |>
  group_by(RecessionPeriod) |>
  summarize(AvgIUR = mean(Insured.Unemployment.Rate, 
                          na.rm = TRUE))

# compare recession vs non-recession using boxplot
recession_vs_non_recession_plot <- ggplot(unemployment_data, 
       aes(RecessionPeriod, 
           Insured.Unemployment.Rate)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "IUR Distribution During vs. Outside Recessions",
       x = "Period", 
       y = "IUR (%)") +
  theme_minimal()

recession_vs_non_recession_plot

# save the linear regression visulization
ggsave("/Users/noraadadurova/Desktop/Math_261A/recession_vs_non_recession_plot.png", 
       plot = recession_vs_non_recession_plot, 
       width = 7, 
       height = 4, 
       dpi = 300)

# compare pre and post COVID-19 patterns
# FALSE is Before March 2020
# TRUE is March 2020 and after
unemployment_data |>
  mutate(PostCOVID = Filed.week.ended >= "2020-03-01") |>
  group_by(PostCOVID) |>
  summarize(AvgIUR = mean(Insured.Unemployment.Rate, 
                          na.rm = TRUE),
            MaxIUR = max(Insured.Unemployment.Rate, 
                         na.rm = TRUE))
# results: 
# before COVID, the insured unemployment rate averaged around 3.1%
# the worst week in over 30 years (1987–2019) was 5.63% which was likely during the Great Recession
# unemployment insurance claims more than doubled on average after March 2020
# the maximum insured unemployment rate reached 27.8%, which is:
# almost 10 times higher than anything before COVID-19 spike
# consistent with official UI statistics from April to May 2020
# so, COVID-19 created an unprecedented shock
# so, the labor market experienced a massive structural break
# hence, the regression model MUST account for this (e.g., COVID dummy variable)