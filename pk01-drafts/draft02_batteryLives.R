##########################################
# Battery consumption figures            #
# ______________________________________ #
# Matin (matinkheirkhahan@ufl.edu)       #
##########################################
library(ggplot2)
library(cowplot)

battery.plot.df <- data.frame(hour = 0:8,
                              HR.5 = rep(0, 9),
                              #HR.15 = rep(0, 9),
                              #HR.30 = rep(0, 9),
                              HR.60 = rep(0, 9),
                              AC.5 = rep(0, 9),
                              #AC.15 = rep(0, 9),
                              #AC.30 = rep(0, 9),
                              AC.60 = rep(0, 9))
battery.plot.df$HR.5 <- c(100, 100 - 25, 100 - 50, 100 - 75, 0, rep(NA, 4))
#battery.plot.df$HR.15 <- c(100, 100 - 30, 100 - 60, 100 - 90, 0,  rep(NA, 4))
#battery.plot.df$HR.30 <- c(100, 100 - 28, 100 - 56, 100 - 84, 0,  rep(NA, 4))
battery.plot.df$HR.60 <- c(100, 100 - 17, 100 - 34, 100 - 51, 100 - 68, 100 - 85, 0, rep(NA, 2))

battery.plot.df$AC.5 <- c(100, 100 - 12, 100 - 25, 100 - 37, 100 - 49, 100 - 60, 100 - 71, 100 - 85, 100 - 95)
#battery.plot.df$AC.15 <- c(100, 100 - 10, 100 - 20, 100 - 30, 100 - 40, 100 - 51, 100 - 61, 100 - 71, 100 - 81)
#battery.plot.df$AC.30 <- c(100, 100 - 5, 100 - 11, 100 - 15, 100 - 22, 100 - 30, 100 - 40, 100 - 48, 100 - 65)
battery.plot.df$AC.60 <- c(100, 100 - 6, 100 - 12.5, 100 - 19, 100 - 25, 100 - 31, 100 - 37.5, 100 - 44, 100 - 49)

plot.df <- data.frame(matrix(nrow = 0, ncol = 3)); colnames(plot.df) <- c("Hour", "Battery", "Type")
for(i in 2:ncol(battery.plot.df)){
     plot.df <- rbind(plot.df,
                      data.frame(Hour = battery.plot.df$hour,
                                 Battery = battery.plot.df[, i],
                                 Type = rep(colnames(battery.plot.df)[i], nrow(battery.plot.df))))
}

plot.df$Type <- factor(plot.df$Type, levels = colnames(battery.plot.df)[2:ncol(battery.plot.df)])
plot.df$Sensor <- factor(c(rep("HR", 18), rep("Accelerometer", 18)), levels = c("HR", "Accelerometer"))



g <- ggplot(data = plot.df, aes(x = Hour, y = Battery))
g + geom_step(aes(linetype = Type, color = Type), direction = "hv") + geom_point(aes(shape = Type), size = 2) + scale_x_continuous(breaks = 0:8) + geom_hline(yintercept = 50, linetype = 2, color = "gray") +
     scale_linetype_manual(values = c(rep("solid", 2), rep("dashed", 2))) + scale_shape_manual(values = c(rep(15, 2), rep(2, 2)))


# Shading?
heartrate.df <- plot.df[plot.df$Sensor == "HR", ]; accelerometer.df <- plot.df[plot.df$Sensor == "Accelerometer", ]
hr.df <- data.frame(Hour = 0:8, minimum = NA, maximum = NA, avg = NA); accel.df <- data.frame(Hour = 0:8, avg = NA, minimum = NA, maximum = NA)
for(hour in plot2.df$hour) {
     hr.df$minimum[hour] <- min(heartrate.df$Battery[heartrate.df$Hour == hour])
     hr.df$maximum[hour] <- max(heartrate.df$Battery[heartrate.df$Hour == hour])
     hr.df$avg[hour] <- mean(heartrate.df$Battery[heartrate.df$Hour == hour], na.rm = T)     
     accel.df$minimum[hour] <- min(accelerometer.df$Battery[accelerometer.df$Hour == hour])
     accel.df$maximum[hour] <- max(accelerometer.df$Battery[accelerometer.df$Hour == hour])
     accel.df$avg[hour] <- mean(accelerometer.df$Battery[accelerometer.df$Hour == hour], na.rm = T)
}

plot2.df <- data.frame(rbind(hr.df, accel.df), Sensor = c(rep("HR", nrow(hr.df)), rep("Accelerometer", nrow(accel.df))))
g <- ggplot(data = plot2.df, aes(x = Hour))
g + geom_step(aes(y = avg)) + geom_ribbon(aes(fill = Sensor, ymin = minimum, ymax = maximum))
