library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(weathercan)

# get shannon data.
# Csv file sourced for 'SHANNON AIRPORT' weather station at https://www.met.ie/ga/climate/available-data/historical-data
shannon_df <-
  read.csv("dly518.csv", skip = 24)

# get vancouver data
pre_2013_van_df <-
  weather_dl(
    station_ids = 889,
    interval = "day",
    string_as = NULL,
    start = "2010-01-01",
    end = "2013-06-12"
  )
post_2013_van_df <-
  weather_dl(
    station_ids = 51442,
    interval = "day",
    string_as = NULL,
    start = "2013-06-13",
    end = "2019-12-31"
  )
vancouver_df <- rbind(pre_2013_van_df, post_2013_van_df)


# format dates
shannon_df$date <- as.Date(shannon_df$date, format = "%d-%b-%Y")
vancouver_df$date <- as.Date(vancouver_df$date)

# filter Shannon dataframe to last ten years
shannon_df <-
  shannon_df[shannon_df$date >= "2010-01-01" &
               shannon_df$date <= "2019-12-31",]

# add consistent month and year columns
shannon_df <- shannon_df %>%
  mutate(month = month(date), year = year(date))
vancouver_df <- vancouver_df %>%
  mutate(month = month(date), year = year(date))

# standardize column names for convenience
setnames(shannon_df, "rain", "total_precip")

# get consecutive dry day count
get_consec_dry_day_count <- function(df, region_name) {
  processed_df <- df %>%
    group_by(consec_dry_id = rleid(total_precip == 0)) %>%
    mutate(consec_dry_days = if_else(total_precip == 0, row_number(), 0L)) %>%
    group_by(consec_dry_id) %>%
    top_n(1, consec_dry_days) %>%
    group_by(year) %>%
    top_n(1, consec_dry_days) %>%
    mutate(location = region_name) %>%
    select(date, year, consec_dry_days, location)
  
  return(processed_df)
}

shannon_consec_dry_day_df <-
  get_consec_dry_day_count(shannon_df, "Shannon")
vancouver_consec_dry_day_df <-
  get_consec_dry_day_count(vancouver_df, "Vancouver")
consec_dry_day_df <-
  rbind(shannon_consec_dry_day_df, vancouver_consec_dry_day_df)


get_monthly_insights <- function(df, region_name) {
  processed_df <- df %>%
    group_by(month) %>%
    summarise(
      na_count = sum(is.na(total_precip)),
      sum_precip = sum(total_precip, na.rm = T),
      dry_days = sum(total_precip == 0, na.rm = T),
      under_five_mm = sum(total_precip > 0 &
                            total_precip < 5, na.rm = T),
      five_to_ten_mm = sum(total_precip >= 5 &
                             total_precip < 10, na.rm = T),
      ten_to_fifteen_mm = sum(total_precip > 10 &
                                total_precip < 15, na.rm = T),
      fifteen_to_twenty_mm = sum(total_precip >= 15 &
                                   total_precip < 20, na.rm = T),
      twenty_plus_mm = sum(total_precip >= 20, na.rm = T)
    ) %>%
    mutate(location = region_name)
  
  return(processed_df)
}

shannon_monthly_insights_df <-
  get_monthly_insights(shannon_df, "Shannon")
vancouver_monthly_insights_df <-
  get_monthly_insights(vancouver_df, "Vancouver")
monthly_insights_df <-
  rbind(shannon_monthly_insights_df, vancouver_monthly_insights_df)




# plotting total monthly precipitation
ggplot(data = monthly_insights) +
  geom_point(mapping = aes(x = month, y = sum_precip, color = location)) +
  labs(title = "Total Monthly Precipitation, 2010-2019",
       y = "Total Precipitation (mm)",
       x = "Month") + theme_bw() +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

#plotting total dry days by month
ggplot(data = monthly_insights) +
  geom_point(mapping = aes(x = month, y = dry_days, color = location)) +
  labs(
    title = "Total Dry Days by Month, 2010-2019",
    subtitle = "'Dry' implies 0mm precipitation on a given calendar day",
    y = "Day Count",
    x = "Month"
  ) + theme_bw() +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

# plotting consecutive dry daus by year
ggplot(data = consec_dry_day_df) +
  geom_point(mapping = aes(x = year, y = consec_dry_days, color = location)) +
  labs(
    title = "Most Consecutive Dry Days by Year",
    subtitle = "'Dry' implies 0mm precipitation on a given calendar day",
    y = "Day Count",
    x = "Year"
  ) + theme_bw() +
  scale_x_continuous(
    breaks = c(2010, 2013, 2016, 2019),
    minor_breaks = c(2011, 2012, 2014, 2015, 2017, 2018)
  )


# plotting precipitation by mm range
long_format_monthly_insights_df <-
  gather(
    monthly_insights_df,
    precip_mm_range,
    precip_day_count,
    under_five_mm:twenty_plus_mm,
    factor_key = TRUE
  )

ggplot(data = long_format_monthly_insights_df) +
  geom_point(mapping = aes(x = month, y = precip_day_count, color = precip_mm_range)) +
  facet_wrap(~ location) +
  labs(
    title = "Precipitation by mm Range, 2010-2019",
    subtitle = "Days with 0mm are excluded",
    y = "Day Count",
    x = "Month"
  ) +
  theme_bw() +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  scale_colour_discrete(
    name  = "MM Ranges",
    breaks = c(
      "under_five_mm",
      "five_to_ten_mm",
      "ten_to_fifteen_mm",
      "fifteen_to_twenty_mm",
      "twenty_plus_mm"
    ),
    labels = c("< 5", ">= 5 & < 10", ">= 10 & < 15", ">= 15 & < 20", ">= 20")
  )
