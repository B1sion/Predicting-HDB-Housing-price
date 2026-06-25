library(corrplot)
library(ggplot2)
library(GGally)
library(car)
library(MASS)

setwd("C:/Users/thetr/Desktop/Work/St3131")
data_og = read.csv("hdb-resale-July-2020.csv")
head(data_og)

data= data_og
#Check that storey ranges have no overlap
unique(data$storey_range)

#Multi-Generation recoded as Executive
data$flat_type = as.character(data$flat_type)
data$flat_type[data$flat_type == "MULTI-GENERATION"] = "EXECUTIVE"
data$flat_type = as.factor(data$flat_type)

#Resale price rescaled to thousands
data$resale_price = data$resale_price / 1000

#To check how many unique blocks
data$block = as.factor(data$block)

#Only 1 month so drop
data$month = NULL

#Convert remaining lease into months left and drop unrelated fields
data$years = as.numeric(sub("(\\d+) years.*", "\\1", data$remaining_lease))
data$months = ifelse(grepl("months", data$remaining_lease),
                   as.numeric(sub(".*?(\\d+) months", "\\1", data$remaining_lease)),
                   0)
data$months_left = data$years * 12 + data$months
data$lease_commence_date = NULL
data$years = NULL
data$months = NULL
data$remaining_lease = NULL




#Convert storey_range to numeric (lowest floor)
data$lowest_floor = as.integer(sub(" TO .*", "", data$storey_range))
data$storey_range = NULL

#Reclassify region into CCR/RCR/OCR
data$reigon = data$town
data$town = NULL
data$block = NULL
data$street_name = NULL
ccr = c("BUKIT TIMAH", "CENTRAL AREA", "QUEENSTOWN")
rcr = c("BISHAN", "BUKIT MERAH", "GEYLANG", "KALLANG/WHAMPOA", "TOA PAYOH", "SERANGOON", "MARINE PARADE")
ocr = c("ANG MO KIO", "BEDOK", "BUKIT BATOK", "BUKIT PANJANG",
        "CHOA CHU KANG", "CLEMENTI", "HOUGANG", "JURONG EAST",
        "JURONG WEST", "PASIR RIS", "PUNGGOL", "SEMBAWANG",
        "SENGKANG", "TAMPINES", "WOODLANDS", "YISHUN")

data$reigon = ifelse(data$reigon %in% ccr, "CCR",
                     ifelse(data$reigon %in% rcr, "RCR", "OCR"))
data$reigon = as.factor(data$reigon)

#flat model to broder groups
data$flat_model_group = ifelse(data$flat_model %in% c("Apartment"), "apartment",
                               ifelse(data$flat_model %in% c("DBSS"), "dbss",
                                      ifelse(data$flat_model %in% c("Maisonette","Model A-Maisonette"), "maisonette",
                                             ifelse(data$flat_model %in% c("Premium Apartment", "Premium Apartment Loft"), "premium",
                                                    ifelse(data$flat_model %in% c("Standard", "Simplified", "Improved", "New Generation", "Model A"), "standard", "others")))))
data$flat_model_group = as.factor(data$flat_model_group)
data$flat_model = NULL

# Correlation matrix
correlation_plot = cor(data[c("floor_area_sqm", "lowest_floor", "months_left", "resale_price")])
colnames(correlation_plot) = rownames(correlation_plot) = c("Floor Area", "Lowest Floor", "Months Left", "Price")

corrplot(correlation_plot,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45)

# Histogram of resale price
hist(data$resale_price, freq = TRUE, main = "Histogram of Resale Price",
     xlab = "Resale Price/$1000", ylab = "Frequency", col = "steelblue")

#Boxplots
par(mfrow = c(1, 3))
par(mar = c(5, 4, 4, 2))

boxplot(resale_price ~ flat_type, data = data,
        main = "Resale Price vs Flat Type",
        xlab = "Flat Type", ylab = "Resale Price / $1000",
        col = "steelblue")

data$flat_model_group = factor(data$flat_model_group, levels = c("standard", "premium", "apartment", "maisonette", "dbss","others"))
boxplot(resale_price ~ flat_model_group, data = data,
        main = "Resale Price vs Flat Model",
        xlab = "Flat Model", ylab = "Resale Price / $1000",
        col = "steelblue")

boxplot(resale_price ~ reigon, data = data,
        main = "Resale Price vs Region",
        xlab = "Region", ylab = "Resale Price / $1000",
        col = "steelblue")

par(mfrow = c(1, 1))
par(mar = c(2, 2, 4, 2))

# Pairs plot
pairs(data[c("resale_price", "floor_area_sqm", "lowest_floor", "months_left")],
      labels = c("Price", "Floor Area", "Lowest Floor", "Months Left"),
      lower.panel = function(x, y, ...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- round(cor(x, y), 3)
        text(0.5, 0.5, r, cex = 1.5)
      },
      upper.panel = function(x, y, ...) {
        points(x, y, pch = 16, cex = 0.4, col = "steelblue")
      },
      main = "Plots of HDB Resale Data",
      gap = 0,
      label.pos = 0.5,
      cex.labels = 1.8,
      font.labels = 2,
      xaxt = "n",
      yaxt = "n")

#Multicolinearity test
x_num = data[c("floor_area_sqm", "lowest_floor", "months_left")]
cor_matrix = cor(x_num)
C = solve(cor_matrix)
VIF = diag(C)
eigenvalues = eigen(cor_matrix)$values
cond = max(eigenvalues) / min(eigenvalues)
cond
VIF

#Model fitting
m0 = lm(resale_price ~ ., data = data)
summary(m0)
Anova(m0)

#MAC
#QQ plot
qqnorm(rstandard(m0), main = "Normal QQ Plot of SR0")
qqline(rstandard(m0), col = "red")

#Residual plots
par(mfrow = c(2, 2))

plot(fitted(m0), rstandard(m0),
     main = "SR0 vs Fitted Values",
     xlab = "Fitted Values", ylab = "SR0",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$floor_area_sqm, rstandard(m0),
     main = "SR0 vs Floor Area",
     xlab = "Floor Area (sqm)", ylab = "SR0",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$lowest_floor, rstandard(m0),
     main = "SR0 vs Lowest Floor",
     xlab = "Lowest Floor", ylab = "SR0",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$months_left, rstandard(m0),
     main = "SR0 vs Months Left",
     xlab = "Months Left", ylab = "SR0",
     pch = 16, cex = 0.4, col = "steelblue")

par(mfrow = c(1, 1))


bc = boxcox(m0)
par(mar = c(5, 5, 4, 2))
plot(bc$x, bc$y, type = "l",
     xlab = expression(lambda),
     ylab = "log-Likelihood",
     main = "Box-Cox Transformation")
abline(h = max(bc$y) - qchisq(0.95, 1)/2, lty = 2)
abline(v = bc$x[which.max(bc$y)], lty = 2)

#Log transformation (lambda = 0)
m1 = lm(log(resale_price) ~ floor_area_sqm + lowest_floor + months_left +
          flat_type + flat_model_group + reigon, data = data)
summary(m1)
Anova(m1)


qqnorm(rstandard(m1), main = "Normal QQ Plot of SR1")
qqline(rstandard(m1), col = "red")

par(mfrow = c(2, 2))

plot(fitted(m1), rstandard(m1),
     main = "SR1 vs Fitted Values",
     xlab = "Fitted Values", ylab = "SR1",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$floor_area_sqm, rstandard(m1),
     main = "SR1 vs Floor Area",
     xlab = "Floor Area (sqm)", ylab = "SR1",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$lowest_floor, rstandard(m1),
     main = "SR1 vs Lowest Floor",
     xlab = "Lowest Floor", ylab = "SR1",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$months_left, rstandard(m1),
     main = "SR1 vs Months Left",
     xlab = "Months Left", ylab = "SR1",
     pch = 16, cex = 0.4, col = "steelblue")

par(mfrow = c(1, 1))


#Reciprocal square root (lambda = -0.5)
m2 = lm(resale_price^(-0.5) ~ floor_area_sqm + lowest_floor + months_left +
          flat_type + flat_model_group + reigon, data = data)
summary(m2)
Anova(m2)

qqnorm(rstandard(m2), main = "Normal QQ Plot of SR2")
qqline(rstandard(m2), col = "red")

par(mfrow = c(2, 2))

plot(fitted(m2), rstandard(m2),
     main = "SR2 vs Fitted Values",
     xlab = "Fitted Values", ylab = "SR2",
     pch = 16, cex = 0.4, col = "steelblue")

plot(m2$model$floor_area_sqm, rstandard(m2),
     main = "SR2 vs Floor Area",
     xlab = "Floor Area (sqm)", ylab = "SR2",
     pch = 16, cex = 0.4, col = "steelblue")

plot(m2$model$lowest_floor, rstandard(m2),
     main = "SR2 vs Lowest Floor",
     xlab = "Lowest Floor", ylab = "SR2",
     pch = 16, cex = 0.4, col = "steelblue")

plot(m2$model$months_left, rstandard(m2),
     main = "SR2 vs Months Left",
     xlab = "Months Left", ylab = "SR2",
     pch = 16, cex = 0.4, col = "steelblue")

par(mfrow = c(1, 1))

#WLS (weights = 1/floor_area_sqm)
m3 = lm(resale_price ~ floor_area_sqm + lowest_floor + months_left +
          flat_type + flat_model_group + reigon,
        data = data, weights = 1/data$floor_area_sqm)
summary(m3)
Anova(m3)

#Final Model (M1+M3)
m4 = lm(log(resale_price) ~ floor_area_sqm + lowest_floor + months_left +
          flat_type + flat_model_group + reigon,
        data = data, weights = 1/data$floor_area_sqm)
summary(m4)
Anova(m4)

#MAC
qqnorm(rstandard(m4), main = "Normal QQ Plot of SR4")
qqline(rstandard(m4), col = "red")

par(mfrow = c(2, 2))

plot(fitted(m4), rstandard(m4),
     main = "SR4 vs Fitted Values",
     xlab = "Fitted Values", ylab = "SR4",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$floor_area_sqm, rstandard(m4),
     main = "SR4 vs Floor Area",
     xlab = "Floor Area (sqm)", ylab = "SR4",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$lowest_floor, rstandard(m4),
     main = "SR4 vs Lowest Floor",
     xlab = "Lowest Floor", ylab = "SR4",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$months_left, rstandard(m4),
     main = "SR4 vs Months Left",
     xlab = "Months Left", ylab = "SR4",
     pch = 16, cex = 0.4, col = "steelblue")

par(mfrow = c(1, 1))

#Final Model 1st attempt (M1+M3)
m5 = lm(resale_price^(-0.5) ~ floor_area_sqm + lowest_floor + months_left +
          flat_type + flat_model_group + reigon,
        data = data, weights = 1/data$floor_area_sqm)
summary(m5)
Anova(m5)

#MAC
qqnorm(rstandard(m5), main = "Normal QQ Plot of SR5")
qqline(rstandard(m5), col = "red")

par(mfrow = c(2, 2))

plot(fitted(m5), rstandard(m5),
     main = "SR5 vs Fitted Values",
     xlab = "Fitted Values", ylab = "SR5",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$floor_area_sqm, rstandard(m5),
     main = "SR5 vs Floor Area",
     xlab = "Floor Area (sqm)", ylab = "SR5",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$lowest_floor, rstandard(m5),
     main = "SR5 vs Lowest Floor",
     xlab = "Lowest Floor", ylab = "SR5",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$months_left, rstandard(m5),
     main = "SR5 vs Months Left",
     xlab = "Months Left", ylab = "SR5",
     pch = 16, cex = 0.4, col = "steelblue")

par(mfrow = c(1, 1))
#cone observed in SR5 vs fitted, log preffered (m4)

#Comparative plots
par(mfrow = c(1, 2))

qqnorm(rstandard(m4), main = "Normal QQ Plot of SR4")
qqline(rstandard(m4), col = "red")

qqnorm(rstandard(m5), main = "Normal QQ Plot of SR5")
qqline(rstandard(m5), col = "red")

par(mfrow = c(1, 1))


par(mfrow = c(2, 4))

plot(fitted(m4), rstandard(m4),
     main = "SR4 vs Fitted Values",
     xlab = "Fitted Values", ylab = "SR4",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$floor_area_sqm, rstandard(m4),
     main = "SR4 vs Floor Area",
     xlab = "Floor Area (sqm)", ylab = "SR4",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$lowest_floor, rstandard(m4),
     main = "SR4 vs Lowest Floor",
     xlab = "Lowest Floor", ylab = "SR4",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$months_left, rstandard(m4),
     main = "SR4 vs Months Left",
     xlab = "Months Left", ylab = "SR4",
     pch = 16, cex = 0.4, col = "steelblue")

plot(fitted(m5), rstandard(m5),
     main = "SR5 vs Fitted Values",
     xlab = "Fitted Values", ylab = "SR5",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$floor_area_sqm, rstandard(m5),
     main = "SR5 vs Floor Area",
     xlab = "Floor Area (sqm)", ylab = "SR5",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$lowest_floor, rstandard(m5),
     main = "SR5 vs Lowest Floor",
     xlab = "Lowest Floor", ylab = "SR5",
     pch = 16, cex = 0.4, col = "steelblue")

plot(data$months_left, rstandard(m5),
     main = "SR5 vs Months Left",
     xlab = "Months Left", ylab = "SR5",
     pch = 16, cex = 0.4, col = "steelblue")

par(mfrow = c(1, 1))