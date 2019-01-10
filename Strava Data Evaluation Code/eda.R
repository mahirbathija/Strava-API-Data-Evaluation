# Loads ggplot2 for plots
library(ggplot2)

# Reads data into csv
df <- read.csv("strava_activity.csv")

# Stores df into new data frame to alter some values
speed <- df

# Replaces speed values with NA that skew the graph
# Specifically values over 20 m/s or 0 m/s
speed$average_speed[speed$average_speed > 20] <- NA
speed$average_speed[speed$average_speed == 0] <- NA

# Stores df into new data frame for time
time <- df

# Replaces time values with NA that skew the graph
# Specifically values over 14400 seconds or 0 seconds
time$elapsed_time[time$elapsed_time > 14400] <- NA
time$elapsed_time[time$elapsed_time == 0] <- NA

# Stores df into new data frame for distance
dist <- df

# Replaces distance values with NA that skew the plot
# Specifically values over 80000 meters or 0 meters
dist$distance[dist$distance > 80000] <- NA
dist$distance[dist$distance == 0] <- NA

# Stores df into new data frame for kilojoules
kilo <- df

# Replaces kilojoules values with NA that skew the plot
# Specifically values over 6000 kilojoules or under 1 kilojoule
kilo$kilojoules[kilo$kilojoules > 6000] <- NA
kilo$kilojoules[kilo$kilojoules < 1] <- NA

# Omits any na values from column kilojoules
na.omit(kilo$kilojoules)

# Creates the speed density plot with adjusted values from
# speed data frame
speed_plot <- ggplot(speed, aes(x = average_speed)) +
              geom_density(color = "darkblue", fill = "lightblue") +
              ylab("Density") +
              xlab("Average Speed") +
              labs(title = "Density of Average Speed",
                   subtitle = "Measured in Meters per Second
                              (Only Displays Values < 20 m/s)") +
              # Creates dashed lines in plot to represent mean value
              geom_vline(data = speed,
                         aes(xintercept = mean(average_speed, na.rm = TRUE),
                             color = "Mean"),
                         linetype = "dashed")

# Produces speed plot so it can be exported to png from plot window
speed_plot

# Creates the heartrate density plot
heartrate_plot <- ggplot(df, aes(x = average_heartrate)) +
                  geom_density(color = "darkblue", fill = "lightblue") +
                  ylab("Density") +
                  xlab("Average Heart Rate") +
                  labs(title = "Density of Average Heart Rate",
                       subtitle = "Measured in Beats Per Minute") +
                  # Creates dashed lines in plot to represent mean value
                  geom_vline(data = df,
                             aes(xintercept =
                                 mean(average_heartrate, na.rm = TRUE),
                                 color = "Mean"),
                             linetype = "dashed")

# Produces heartrate plot so it can be exported to png from plot window
heartrate_plot

# Creates the elapsed time density plot with adjusted values from
# time data frame
time_plot <- ggplot(time, aes(x = elapsed_time)) +
             geom_density(color = "darkblue", fill = "lightblue") +
             ylab("Density") +
             xlab("Excercise Time") +
             labs(title = "Density of Excercise Time",
                  subtitle = "Measured in Seconds
                             (Only Displays Values < 4 hours)") +
             # Creates dashed lines in plot to represent mean and median values
             geom_vline(data = time,
                        aes(xintercept = mean(elapsed_time, na.rm = TRUE),
                            color = "Mean"),
                        linetype = "dashed") +
             geom_vline(data = time,
                        aes(xintercept = median(elapsed_time, na.rm = TRUE),
                            color = "Median"),
                        linetype = "dashed")

# Produces time plot so it can be exported to png from plot window
time_plot

# Creates the distance density plot with adjusted values from
# distance data frame
distance_plot <- ggplot(dist, aes(x = distance)) +
     geom_density(color = "darkblue", fill = "lightblue") +
     ylab("Density") +
     xlab("Distance Traveled") +
     labs(title = "Density of Distance Traveled",
          subtitle = "Measured in Meters
                     (Only Displays Values < 80000 meters)") +
     # Creates dashed lines in plot to represent mean and median values
     geom_vline(data = dist,
                aes(xintercept = mean(distance, na.rm = TRUE),
                    color = "Mean"),
                linetype = "dashed") +
     geom_vline(data = dist,
                aes(xintercept = median(distance, na.rm = TRUE),
                    color = "Median"),
                linetype = "dashed")

# Produces distance plot so it can be exported to png from plot window
distance_plot

# Creates the kilojoules density plot with adjusted values from
# kilojoules data frame
kilojoules_plot <- ggplot(kilo, aes(x = kilojoules)) +
     geom_density(color = "darkblue", fill = "lightblue") +
     ylab("Density") +
     xlab("Kilojoules Burned") +
     labs(title = "Density of Kilojoules Burned",
          subtitle = "Measured in Kilojoules 
                     (Only Displays Values < 6000 kilojoules)") +
     # Creates dashed lines in plot to represent mean and median values
     geom_vline(data = kilo,
                aes(xintercept = mean(kilojoules, na.rm = TRUE),
                    color = "Mean"),
                linetype = "dashed") +
     geom_vline(data = kilo,
                aes(xintercept = median(kilojoules, na.rm = TRUE),
                    color = "Median"),
                linetype = "dashed")

# Produces kilojoules plot so it can be exported to png from plot window
kilojoules_plot
