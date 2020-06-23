#----------------------------------------------------------------------------------------------------------#

# This script was written by Aaron Kamoske - aaron.kamoske@usda.gov - June, 2020

# In order to run the script, change the folder paths to the correct location (e.g. where the .csv files are),

#----------------------------------------------------------------------------------------------------------#

# load needed packages
library(dplyr)

# read in the csv files
lidar <- read.csv("D:/New_Hampshire_Lidar/Clipped_Point_Clouds/Lidar/clipHt_FtTenthAc/lidar_metrics.csv")
naip <- read.csv("D:/New_Hampshire_Lidar/Clipped_Point_Clouds/NAIP/clipHt_FtTenthAc/naip_metrics.csv")

# make sure the tile names are arranged correctly
lidar$FileTitle <- as.numeric(substring(lidar$FileTitle, 6))
naip$FileTitle <- as.numeric(substring(naip$FileTitle, 6))

lidar <- arrange(lidar, FileTitle)
naip <- arrange(naip, FileTitle)

lidar$FileTitle
naip$FileTitle

# add a column so that we can plot each point cloud individually
lidar$collect <- "blue"
naip$collect <- "blue"

lidar$collect[1:28] <- "orange"
naip$collect[1:28] <- "orange"

# set some plotting parameters
par(pty = "s", mfrow = c(2,2))

#---------------ALL PLOTS TOGETHER---------------#

##### 95th percentile ######

# run a linear regression
pc.lm.95 <- lm(lidar$Elev.P95 ~ naip$Elev.P95)
summary(pc.lm.95)

# find the correlation
e95.cor <- round(cor(lidar$Elev.P95, naip$Elev.P95), digits = 2)

# build the plot
eqn <- bquote(~~correlation == .(e95.cor))

plot(naip$Elev.P95, lidar$Elev.P95,
     xlab = "NAIP",
     ylab = "Lidar",
     main = "Height comparison of 95th percentiles",
     pch = 20,
     col = lidar$collect)
abline(pc.lm.95, col = "red")
text(25, 3, eqn, pos = 1)

##### 99th percentile ######

# run a linear regression
pc.lm.99 <- lm(lidar$Elev.P99 ~ naip$Elev.P99)
summary(pc.lm.99)

# find the correlation
e99.cor <- round(cor(lidar$Elev.P99, naip$Elev.P99), digits = 2)

# build the plot
eqn <- bquote(~~correlation == .(e99.cor))

par(pty = "s")
plot(naip$Elev.P99, lidar$Elev.P99,
     xlab = "NAIP",
     ylab = "Lidar",
     main = "Height comparison of 99th percentiles",
     pch = 20,
     col = lidar$collect)
abline(pc.lm.99, col = "red")
text(25, 3, eqn, pos = 1)

##### Points above 2m ######

# run a linear regression
p2.lm <- lm(lidar$Percentage.all.returns.above.2.00 ~ naip$Percentage.all.returns.above.2.00)
summary(p2.lm)

# find the correlation
e2.cor <- round(cor(lidar$Percentage.all.returns.above.2.00, naip$Percentage.all.returns.above.2.00), digits = 2)
e2.cor

# build the plot
eqn <- bquote(~~correlation == .(e2.cor))

plot(naip$Percentage.all.returns.above.2.00, lidar$Percentage.all.returns.above.2.00,
     xlab = "NAIP",
     ylab = "Lidar",
     main = "Percentage of all points above 2m",
     pch = 20,
     col = lidar$collect)
abline(p2.lm, col = "red")
text(80, 5, eqn, pos = 1)

##### Mean Elevations ######

# run a linear regression
el.lm <- lm(lidar$Elev.mean ~ naip$Elev.mean)
summary(el.lm)

# find the correlation
el.cor <- round(cor(lidar$Elev.mean, naip$Elev.mean), digits = 2)
el.cor

# build the plot
eqn <- bquote(~~correlation == .(el.cor))

plot(naip$Elev.mean, lidar$Elev.mean,
     xlab = "NAIP",
     ylab = "Lidar",
     main = "Mean Elevation (meters)",
     pch = 20,
     col = lidar$collect)
abline(el.lm, col = "red")
text(20, 1, eqn, pos = 1)

#---------------WINNEPESAUKEE COLLECTION---------------#

lidar.w <- lidar[lidar$collect == "orange",]
naip.w <- naip[naip$collect == "orange",]

##### 95th percentile ######

# run a linear regression
pc.lm.95 <- lm(lidar.w$Elev.P95 ~ naip.w$Elev.P95)
summary(pc.lm.95)

# find the correlation
e95.cor <- round(cor(lidar.w$Elev.P95, naip.w$Elev.P95), digits = 2)

# build the plot
eqn <- bquote(~~correlation == .(e95.cor))

plot(naip.w$Elev.P95, lidar.w$Elev.P95,
     xlab = "NAIP",
     ylab = "Lidar",
     main = "Height comparison of 95th percentiles",
     pch = 20,
     col = lidar.w$collect)
abline(pc.lm.95, col = "red")
text(23, 3, eqn, pos = 1)

##### 99th percentile ######

# run a linear regression
pc.lm.99 <- lm(lidar.w$Elev.P99 ~ naip.w$Elev.P99)
summary(pc.lm.99)

# find the correlation
e99.cor <- round(cor(lidar.w$Elev.P99, naip.w$Elev.P99), digits = 2)

# build the plot
eqn <- bquote(~~correlation == .(e99.cor))

par(pty = "s")
plot(naip.w$Elev.P99, lidar.w$Elev.P99,
     xlab = "NAIP",
     ylab = "Lidar",
     main = "Height comparison of 99th percentiles",
     pch = 20,
     col = lidar.w$collect)
abline(pc.lm.99, col = "red")
text(25, 3, eqn, pos = 1)

##### Points above 2m ######

# run a linear regression
p2.lm <- lm(lidar.w$Percentage.all.returns.above.2.00 ~ naip.w$Percentage.all.returns.above.2.00)
summary(p2.lm)

# find the correlation
e2.cor <- round(cor(lidar.w$Percentage.all.returns.above.2.00, naip.w$Percentage.all.returns.above.2.00), digits = 2)
e2.cor

# build the plot
eqn <- bquote(~~correlation == .(e2.cor))

plot(naip.w$Percentage.all.returns.above.2.00, lidar.w$Percentage.all.returns.above.2.00,
     xlab = "NAIP",
     ylab = "Lidar",
     main = "Percentage of all points above 2m",
     pch = 20,
     col = lidar.w$collect)
abline(p2.lm, col = "red")
text(80, 5, eqn, pos = 1)

##### Mean Elevations ######

# run a linear regression
el.lm <- lm(lidar.w$Elev.mean ~ naip.w$Elev.mean)
summary(el.lm)

# find the correlation
el.cor <- round(cor(lidar.w$Elev.mean, naip.w$Elev.mean), digits = 2)
el.cor

# build the plot
eqn <- bquote(~~correlation == .(el.cor))

plot(naip.w$Elev.mean, lidar.w$Elev.mean,
     xlab = "NAIP",
     ylab = "Lidar",
     main = "Mean Elevation (meters)",
     pch = 20,
     col = lidar.w$collect)
abline(el.lm, col = "red")
text(20, 1, eqn, pos = 1)

#---------------UMBAGOG COLLECTION---------------#

lidar.b <- lidar[lidar$collect == "blue",]
naip.b <- naip[naip$collect == "blue",]

##### 95th percentile ######

# run a linear regression
pc.lm.95 <- lm(lidar.b$Elev.P95 ~ naip.b$Elev.P95)
summary(pc.lm.95)

# find the correlation
e95.cor <- round(cor(lidar.b$Elev.P95, naip.b$Elev.P95), digits = 2)

# build the plot
eqn <- bquote(~~correlation == .(e95.cor))

plot(naip.b$Elev.P95, lidar.b$Elev.P95,
     xlab = "NAIP",
     ylab = "Lidar",
     main = "Height comparison of 95th percentiles",
     pch = 20,
     col = lidar.b$collect)
abline(pc.lm.95, col = "red")
text(23, 3, eqn, pos = 1)

##### 99th percentile ######

# run a linear regression
pc.lm.99 <- lm(lidar.b$Elev.P99 ~ naip.b$Elev.P99)
summary(pc.lm.99)

# find the correlation
e99.cor <- round(cor(lidar.b$Elev.P99, naip.b$Elev.P99), digits = 2)

# build the plot
eqn <- bquote(~~correlation == .(e99.cor))

par(pty = "s")
plot(naip.b$Elev.P99, lidar.b$Elev.P99,
     xlab = "NAIP",
     ylab = "Lidar",
     main = "Height comparison of 99th percentiles",
     pch = 20,
     col = lidar.b$collect)
abline(pc.lm.99, col = "red")
text(25, 3, eqn, pos = 1)

##### Points above 2m ######

# run a linear regression
p2.lm <- lm(lidar.b$Percentage.all.returns.above.2.00 ~ naip.b$Percentage.all.returns.above.2.00)
summary(p2.lm)

# find the correlation
e2.cor <- round(cor(lidar.b$Percentage.all.returns.above.2.00, naip.b$Percentage.all.returns.above.2.00), digits = 2)
e2.cor

# build the plot
eqn <- bquote(~~correlation == .(e2.cor))

plot(naip.b$Percentage.all.returns.above.2.00, lidar.b$Percentage.all.returns.above.2.00,
     xlab = "NAIP",
     ylab = "Lidar",
     main = "Percentage of all points above 2m",
     pch = 20,
     col = lidar.b$collect)
abline(p2.lm, col = "red")
text(80, 5, eqn, pos = 1)

##### Mean Elevations ######

# run a linear regression
el.lm <- lm(lidar.b$Elev.mean ~ naip.b$Elev.mean)
summary(el.lm)

# find the correlation
el.cor <- round(cor(lidar.b$Elev.mean, naip.b$Elev.mean), digits = 2)
el.cor

# build the plot
eqn <- bquote(~~correlation == .(el.cor))

plot(naip.b$Elev.mean, lidar.b$Elev.mean,
     xlab = "NAIP",
     ylab = "Lidar",
     main = "Mean Elevation (meters)",
     pch = 20,
     col = lidar.b$collect)
abline(el.lm, col = "red")
text(20, 1, eqn, pos = 1)





