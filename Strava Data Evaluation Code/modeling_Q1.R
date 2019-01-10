# Loads necessary libraries for script
library(dplyr)
library(ggplot2)

# Reads in cleaned data frame
df <- read.csv("a3clean_data.csv")

# Creates filtered data frames for females and males
# specifically
df_female <- df %>% filter(athlete.sex_F == 1)
df_male <- df %>% filter(athlete.sex_M == 1)

# Creates new filtered data frame for female bike rides,
# female runs, male bike rides, and male runs
df_female_bike <- df_female %>% filter(type_Ride == 1)
df_male_bike <- df_male %>% filter(type_Ride == 1)
df_female_run <- df_female %>% filter(type_Run == 1)
df_male_run <- df_male %>% filter(type_Run == 1)

# Creates new dataframe for filtered results by countries with
# at least 50 bike excercises
df_united_states <- df %>% filter(location_country == "United States")
df_united_kingdom <- df %>% filter(location_country == "United Kingdom")
df_australia <- df %>% filter(location_country == "Australia")
df_brazil <- df %>% filter(location_country == "Brazil")
df_france <- df %>% filter(location_country == "France")
df_canada <- df %>% filter(location_country == "Canada")
df_spain <- df %>% filter(location_country == "Spain")
df_italy <- df %>% filter(location_country == "Italy")
df_netherlands <- df %>% filter(location_country == "Netherlands")
df_germany <- df %>% filter(location_country == "Germany")

# Displays summary for each of the male and female
# data frames to add mean values to table in report
summary(df_male)
summary(df_female)
summary(df_male_bike)
summary(df_female_bike)
summary(df_male_run)
summary(df_female_run)

# Displays summary for each of the location countries
# data frames to add mean values to table in report
summary(df_united_states)
summary(df_united_kingdom)
summary(df_australia)
summary(df_brazil)
summary(df_france)
summary(df_canada)
summary(df_spain)
summary(df_italy)
summary(df_netherlands)
summary(df_germany)

# Performs a Wilcoxon test to see if median values for
# male general data frame is greater than female values
wilcox.test(df_male$average_heartrate,
            df_female$average_heartrate, alternative = "greater")
wilcox.test(df_male$average_speed_mph,
            df_female$average_speed_mph, alternative = "greater")
wilcox.test(df_male$max_speed_mph,
            df_female$max_speed_mph, alternative = "greater")
wilcox.test(df_male$total_elevation_gain,
            df_female$total_elevation_gain, alternative = "greater")
wilcox.test(df_male$distance_meters,
            df_female$distance_meters, alternative = "greater")
wilcox.test(df_male$elapsed_time,
            df_female$elapsed_time, alternative = "greater")
wilcox.test(df_male$moving_time,
            df_female$moving_time, alternative = "greater")

# Performs a Wilcoxon test to see if median values for
# male bike data frame is greater than female values
wilcox.test(df_male_bike$average_heartrate,
            df_female_bike$average_heartrate, alternative = "greater")
wilcox.test(df_male_bike$average_speed_mph,
            df_female_bike$average_speed_mph, alternative = "greater")
wilcox.test(df_male_bike$max_speed_mph,
            df_female_bike$max_speed_mph, alternative = "greater")
wilcox.test(df_male_bike$total_elevation_gain,
            df_female_bike$total_elevation_gain, alternative = "greater")
wilcox.test(df_male_bike$distance_meters,
            df_female_bike$distance_meters, alternative = "greater")
wilcox.test(df_male_bike$elapsed_time,
            df_female_bike$elapsed_time, alternative = "greater")
wilcox.test(df_male_bike$moving_time,
            df_female_bike$moving_time, alternative = "greater")

# Performs a Wilcoxon test to see if median values for
# male run data frame is greater than female values
wilcox.test(df_male_run$average_heartrate,
            df_female_run$average_heartrate, alternative = "greater")
wilcox.test(df_male_run$average_speed_mph,
            df_female_run$average_speed_mph, alternative = "greater")
wilcox.test(df_male_run$max_speed_mph,
            df_female_run$max_speed_mph, alternative = "greater")
wilcox.test(df_male_run$total_elevation_gain,
            df_female_run$total_elevation_gain, alternative = "greater")
wilcox.test(df_male_run$distance_meters,
            df_female_run$distance_meters, alternative = "greater")
wilcox.test(df_male_run$elapsed_time,
            df_female_run$elapsed_time, alternative = "greater")
wilcox.test(df_male_run$moving_time,
            df_female_run$moving_time, alternative = "greater")

# Creates overlapping speed density plot comparing
# male and female density values
speed_compare_plot <- ggplot() +
                      geom_density(data = df_female,
                                   aes(x = average_speed_mph), 
                                   color = "darkred") + 
                      geom_density(data = df_male,
                                   aes(x = average_speed_mph), 
                                   color="darkblue") +
                      # Creates dashed lines in plot to represent mean and 
                      # median values for males and females
                      geom_vline(data = df_male, 
                                 aes(xintercept = mean(average_speed_mph, 
                                                       na.rm = TRUE), 
                                     color = "Male Mean"),
                                 linetype = "dashed") +
                      geom_vline(data = df_female, 
                                 aes(xintercept = mean(average_speed_mph,
                                                       na.rm = TRUE), 
                                     color = "Female Mean"),
                                 linetype = "dashed") +
                      geom_vline(data = df_male,
                                 aes(xintercept = median(average_speed_mph,
                                                         na.rm = TRUE), 
                                     color = "Male Median"),
                                 linetype = "dashed") +
                      geom_vline(data = df_female, 
                                 aes(xintercept = median(average_speed_mph, 
                                                         na.rm = TRUE), 
                                     color = "Female Median"),
                                 linetype = "dashed") + 
                      ylab("Density") +
                      xlab("Average Speed (MPH)") + 
                      labs(title = "Density of Average Speed in MPH", 
                           subtitle = "Comparison between Male (Dark Blue) 
                                       and Female (Dark Red)")

# Produces speed compare plot so it can be exported to png from plot window
speed_compare_plot

# Creates overlapping distance density plot comparing
# male and female density values
distance_compare_plot <- ggplot() + 
                         geom_density(data = df_female,
                                      aes(x=distance_meters),
                                      color = "darkred") + 
                         geom_density(data = df_male,
                                      aes(x=distance_meters),
                                      color="darkblue") +
                         # Creates dashed lines in plot to represent mean and 
                         # median values for males and females
                         geom_vline(data = df_male,
                                    aes(xintercept = mean(distance_meters,
                                                          na.rm = TRUE),
                                        color = "Male Mean"),
                                     linetype = "dashed") +
                         geom_vline(data = df_female,
                                    aes(xintercept = mean(distance_meters,
                                                          na.rm = TRUE),
                                        color = "Female Mean"),
                                    linetype = "dashed") +
                         geom_vline(data = df_male,
                                    aes(xintercept = median(distance_meters,
                                                            na.rm = TRUE),
                                        color = "Male Median"),
                                    linetype = "dashed") +
                         geom_vline(data = df_female, 
                                    aes(xintercept = median(distance_meters,
                                                            na.rm = TRUE), 
                                        color = "Female Median"),
                                    linetype = "dashed") + 
                         ylab("Density") +
                         xlab("Distance (Miles") + 
                         labs(title = "Density of Distance in Miles", 
                              subtitle = "Comparison between Male (Dark Blue)
                                          and Female (Dark Red)")

# Produces distance compare plot so it can be exported to png from plot window
distance_compare_plot

# Filters out any values where mph is over 40 or if average speed is
# higher than max speed
df_female_bike <- df_female_bike %>% 
                  filter(max_speed_mph < 40) %>% 
                  filter(max_speed_mph > average_speed_mph)
df_male_bike <- df_male_bike %>%
                filter(max_speed_mph < 40) %>%
                filter(max_speed_mph > average_speed_mph)

# Filters out any values where run speed is over 20 mph or 
# average speed is higher than max speed
df_male_run <- df_male_run %>%
               filter(max_speed_mph < 20) %>%
               filter(max_speed_mph > average_speed_mph)
df_male_run <- df_male_run %>% 
               filter(max_speed_mph < 20) %>% 
               filter(max_speed_mph > average_speed_mph)

# Calculates NP metric based on elevation gain / total distance * average speed
np_female_bike <- ((df_female_bike$total_elevation_gain) / df_female_bike$distance) *
                   (df_female_bike$average_speed)
np_male_bike <- ((df_male_bike$total_elevation_gain) / df_male_bike$distance) *
                 (df_male_bike$average_speed)
np_male_run <- ((df_male_run$total_elevation_gain)/df_male_run$distance) * 
                (df_male_run$average_speed)
np_female_run <- ((df_female_run$total_elevation_gain)/df_female_run$distance) * 
                  (df_female_run$average_speed)

# Calculates FTP metric based on maximum speed * elevation gain / total distance
ftp_female_bike <- ((df_female_bike$total_elevation_gain) / df_female_bike$distance) * 
                    (df_female_bike$max_speed)
ftp_male_bike <- ((df_male_bike$total_elevation_gain)/df_male_bike$distance) *
                  (df_male_bike$max_speed)
ftp_male_run <- ((df_male_run$total_elevation_gain)/df_male_run$distance) *
                 (df_male_run$max_speed)
ftp_female_run <- ((df_female_run$total_elevation_gain)/df_female_run$distance) *
                   (df_female_run$max_speed)

# Converts elapsed time back to seconds
time_female_bike <- (df_female_bike$elapsed_time * 60)
time_male_bike <- (df_male_bike$elapsed_time * 60)
time_male_run <- (df_male_run$elapsed_time * 60)
time_female_run <- (df_female_run$elapsed_time * 60)

# Calculates intensity scores based on formula 
# (loosely modeled after TSS and rTSS)
df_female_bike$intensity_score <- ((time_female_bike * np_female_bike * 
                                    np_female_bike/ftp_female_bike) / 
                                    (ftp_female_bike * time_female_bike)) * 
                                    df_female_bike$distance
df_male_bike$intensity_score <- ((time_male_bike * np_male_bike *
                                  np_male_bike/ftp_male_bike) /
                                  (ftp_male_bike * time_male_bike)) *
                                  (df_male_bike$distance)
df_male_run$intensity_score <- ((time_male_run * np_male_run * 
                                 np_male_run/ftp_male_run) / 
                                 (ftp_male_run * time_male_run)) *
                                 (df_male_run$distance)
df_female_run$intensity_score <- ((time_female_run * np_female_run *
                                   np_female_run/ftp_female_run) / 
                                   (ftp_female_run * time_female_run)) *
                                   (df_female_run$distance)

# Scales the intensity scores to 0-100 scale based on the min and max values 
df_female_bike$intensity_score_scale <- ((df_female_bike$intensity_score - 
                                          min(df_male_bike$intensity_score, 
                                              na.rm = T)) / 
                                          (max(df_male_bike$intensity_score, 
                                               na.rm = T) - 
                                          min(df_male_bike$intensity_score, 
                                              na.rm = T))) * 100
df_male_bike$intensity_score_scale <- ((df_male_bike$intensity_score -
                                        min(df_male_bike$intensity_score, 
                                            na.rm = T)) / 
                                        (max(df_male_bike$intensity_score,
                                             na.rm = T) - 
                                         min(df_male_bike$intensity_score, 
                                             na.rm = T))) * 100
df_male_run$intensity_score_scale <- ((df_male_run$intensity_score - 
                                       min(df_female_run$intensity_score,
                                           na.rm = T)) / 
                                       (max(df_male_run$intensity_score,
                                            na.rm = T) - 
                                       min(df_female_run$intensity_score,
                                           na.rm = T))) * 100
df_female_run$intensity_score_scale <- ((df_female_run$intensity_score - 
                                         min(df_female_run$intensity_score, 
                                             na.rm = T)) / 
                                        (max(df_male_run$intensity_score, 
                                             na.rm = T) - 
                                         min(df_female_run$intensity_score, 
                                             na.rm = T))) * 100
  
# Displays summary for each of the male and female
# intensity scores to add mean values to table in report
summary(df_female_run$intensity_score_scale)
summary(df_male_run$intensity_score_scale)
summary(df_female_bike$intensity_score_scale)
summary(df_male_bike$intensity_score_scale)

# Conducts Wilcoxon test to see if male median values are 
# greater than female median values (Intensity Score)
wilcox.test(df_male_bike$intensity_score_scale, 
            df_female_bike$intensity_score_scale, alternative = "greater")
wilcox.test(df_male_run$intensity_score_scale, 
            df_female_run$intensity_score_scale, alternative = "greater")

# Creates overlapping intensity score (run) density plot comparing
# male and female density values
intensity_run_plot <- ggplot() + 
                      geom_density(data = df_female_run, 
                                   aes(x=intensity_score_scale), 
                                   color = "darkred") + 
                      geom_density(data = df_male_run, 
                                   aes(x=intensity_score_scale), 
                                   color="darkblue") +
                      # Creates dashed lines in plot to represent mean and 
                      # median values for males and females
                      geom_vline(data = df_male_run,
                                 aes(xintercept = mean(intensity_score_scale,
                                                       na.rm = TRUE), 
                                     color = "Male Mean"),
                                 linetype = "dashed") +
                      geom_vline(data = df_female_run,
                                 aes(xintercept = mean(intensity_score_scale, 
                                                       na.rm = TRUE), 
                                     color = "Female Mean"),
                                 linetype = "dashed") +
                      geom_vline(data = df_male_run,
                                 aes(xintercept = median(intensity_score_scale, 
                                                         na.rm = TRUE), 
                                     color = "Male Median"),
                                 linetype = "dashed") +
                      geom_vline(data = df_female_run,
                                 aes(xintercept = median(intensity_score_scale, 
                                                         na.rm = TRUE), 
                                     color = "Female Median"),
                                 linetype = "dashed") + 
                      ylab("Density") +
                      xlab("Intensity Score") + 
                      labs(title = "Density of Intensity Scores for Runs", 
                           subtitle = "Comparison between Male (Dark Blue) and 
                                      Female (Dark Red)")

# Produces intensity score plot for runs
# so it can be exported to png from plot window
intensity_run_plot 

# Creates overlapping intensity score (bike) density plot comparing
# male and female density values
intensity_bike_plot <- ggplot() + 
                       geom_density(data = df_female_bike,
                                    aes(x=intensity_score_scale), 
                                    color = "darkred") + 
                       geom_density(data = df_male_bike,
                                    aes(x=intensity_score_scale),
                                    color="darkblue") + 
                       # Creates dashed lines in plot to represent mean and
                       # median values for males and females
                       geom_vline(data = df_male_bike,
                                  aes(xintercept = mean(intensity_score_scale, 
                                                        na.rm = TRUE), 
                                      color = "Male Mean"),
                                  linetype = "dashed") +
                       geom_vline(data = df_female_bike,
                                  aes(xintercept = mean(intensity_score_scale, 
                                                        na.rm = TRUE), 
                                      color = "Female Mean"),
                                  linetype = "dashed") +
                       geom_vline(data = df_male_bike,
                                  aes(xintercept = median(intensity_score_scale,
                                                          na.rm = TRUE), 
                                      color = "Male Median"),
                                  linetype = "dashed") +
                       geom_vline(data = df_female_bike,
                                  aes(xintercept = median(intensity_score_scale, 
                                                          na.rm = TRUE), 
                                      color = "Female Median"),
                                  linetype = "dashed") + 
                       ylab("Density") +
                       xlab("Intensity Score") + 
                       labs(title = "Density of Intensity Scores for Bike Rides", 
                            subtitle = "Comparison between Male (Dark Blue) and 
                                        Female (Dark Red)")

# Produces intensity score plot for bike rides
# so it can be exported to png from plot window
intensity_bike_plot 