

rm(list=ls())

library(ggplot2)

data <-read.csv("CJ CMJ-Countermovement Jump-2022.09.02-10.33.20-Trial1 - CJ CMJ-Countermovement Jump-2022.09.02-10.33.20-Trial1.csv",
                skip=9)
mean(is.na(data$Z.Left))
mean(is.na(data$Z.Right))


# Change time to normalize to 0
data$Time <- data$Time - min(data$Time)

T <- nrow(data)

# re-arrange data to be able to plot left leg against right leg
data_tidy <- data.frame(Time=data$Time, Force=data$Z.Left, Leg="Left")

data_tidy <- rbind(data_tidy, data.frame(Time=data$Time, Force=data$Z.Right,
                                         Leg="Right"))

head(data_tidy)
tail(data_tidy)

plot_left <- ggplot(data, aes(x=Time, y=Z.Left)) + geom_point() +
  ylab("Force from Left Leg (Newtons)") + xlab("Time (seconds)")

plot_left

plot_right <- ggplot(data, aes(x=Time, y=Z.Right)) + geom_point() +
  ylab("Force from Right Leg (Newtons)") + xlab("Time (seconds)")

plot_right

plot_both <- ggplot(data_tidy, aes(x=Time, y=Force, color=Leg)) + geom_line() +
  ylab("Force (Newtons)") + xlab("Time (seconds)") 

plot_both

# Acceleration plot

plot_acc <- ggplot(data, aes(x=Time, y=Acceleration)) + geom_point() +
  ylab("Acceleration (m/s^2)") + xlab("Time (seconds)") 
# + geom_vline(xintercept=1375, color="orange") + geom_vline(xintercept=974, color="orange") +
#   geom_vline(xintercept=593, color="orange")

plot_acc

# Calculate the percentage difference between force from left and right legs
# in the z direction at each time
data$pct_diff <- (data$Z.Left - data$Z.Right)/pmax(data$Z.Left, data$Z.Right)*100
data$abs_pct_diff <- abs(data$Z.Left - data$Z.Right)/pmax(data$Z.Left, data$Z.Right)*100




data_tidy[data_tidy$Time >= 47.2 & data_tidy$Time <= 47.4, ]

data[data$Time >= 47.2 & data$Time <= 47.4, "Acceleration"]

# Find standing phase: acceleration 0
# Looks like rows 1 - 592, so until time 46.304
# data[2100:2600, c("Time", "Acceleration")]
print("Average absolute asymmetry during standing phase:")
standing_inds <- 1:592
avg_standing_asymmetry <- mean(data[standing_inds, "abs_pct_diff"])
print(paste(round(avg_standing_asymmetry, 4), "%", sep=""))

# Summary statistics
print(summary(data[standing_inds, "abs_pct_diff"]))
print(sd(data[standing_inds, "abs_pct_diff"]))

print(summary(data[standing_inds, "Z.left"]))
print(sd(data[standing_inds, "Z.left"]))

# Find unweighing phase: acceleration negative
# Rows 593 - 973, so time 46.305 - 46.685
print("Average absolute asymmetry during unweighting phase:")
unweighting_inds <- 593:973
avg_unweighting_asymmetry <- mean(data[unweighting_inds, "abs_pct_diff"])
print(paste(round(avg_unweighting_asymmetry, 4), "%", sep=""))

print(summary(data[unweighting_inds, "abs_pct_diff"]))
print(sd(data[unweighting_inds, "abs_pct_diff"]))

# Find breaking phase: acceleration positive
# Rows 974 - 1374, so time 46.686 - 47.086
print("Average absolute asymmetry during breaking phase:")
braking_inds <- 974:1374
avg_braking_asymmetry <- mean(data[braking_inds, "abs_pct_diff"])
print(paste(round(avg_braking_asymmetry, 4), "%", sep=""))

print(summary(data[braking_inds, "abs_pct_diff"]))
print(sd(data[braking_inds, "abs_pct_diff"]))

# Find propulsion phase: acceleration peaks and starts declining
# Rows 1375 - 1601, so time 47.087 - ???
print("Average absolute asymmetry during propulsion phase:")
propulsion_inds <- 1375:1601
avg_propulsion_asymmetry <- mean(data[propulsion_inds, "abs_pct_diff"])
print(paste(round(avg_propulsion_asymmetry, 4), "%", sep=""))

print(summary(data[propulsion_inds, "abs_pct_diff"]))
print(sd(data[propulsion_inds, "abs_pct_diff"]))

# In the air: acceleration -9.8

# Landing rows: 2155 - T
print("Average absolute asymmetry during braking phase")
landing_inds <- 2155:T
avg_landing_asymmetry <- mean(data[landing_inds, "abs_pct_diff"])
print(paste(round(avg_landing_asymmetry, 4), "%", sep=""))



df_plot <- data[c(standing_inds, unweighting_inds, braking_inds,
                  propulsion_inds), ]

plot_pct_diff <- ggplot(df_plot, aes(x=Time, y= pct_diff)) + geom_point() +
  geom_hline(yintercept=10, color="red", linetype = "dashed") +
  geom_hline(yintercept=-10, color="red", linetype = "dashed") +
  ylab("Percent Difference") + ggtitle("Percent Asymmetry in Leg Force Over Time")

plot_pct_diff

landing_plot <- data[landing_inds,]

landing_plot_pct_diff <- ggplot(landing_plot, aes(x=Time, y= pct_diff)) + geom_point() +
  geom_hline(yintercept=10, color="red", linetype = "dashed") +
  geom_hline(yintercept=-10, color="red", linetype = "dashed") +
  ylab("Percent Difference") + ggtitle("Percent Asymmetry in Leg Force Over Time")

landing_plot_pct_diff

df_tidy_plot <- data_tidy[c(standing_inds, unweighting_inds, braking_inds,
                            propulsion_inds, standing_inds + T,
                            unweighting_inds + T, braking_inds + T,
                            propulsion_inds + T), ]


plot_both_pre <- ggplot(df_tidy_plot, aes(x=Time, y=Force, color=Leg)) +
  geom_line() + ylab("Force (Newtons)") + xlab("Time (seconds)") +
  ggtitle("Force in Each Leg Over Time")
# + geom_vline(xintercept=1375, color="orange") + geom_vline(xintercept=974, color="orange") +
# geom_vline(xintercept=593, color="orange")

plot_both_pre 

# plot_both + scale_y_log10()

df_tidy_landing_plot <- data_tidy[c(landing_inds, landing_inds + T), ]

plot_both_landing <- ggplot(df_tidy_landing_plot, aes(x=Time, y=Force, color=Leg)) +
  geom_line() + ylab("Force (Newtons)") + xlab("Time (seconds)") +
  ggtitle("Force in Each Leg Over Time After Landing")
# + geom_vline(xintercept=1375, color="orange") + geom_vline(xintercept=974, color="orange") +
# geom_vline(xintercept=593, color="orange")

plot_both_landing





