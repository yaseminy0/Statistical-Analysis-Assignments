library(MPV)
data("table.b3")
View(table.b3)

#1
names(table.b3)<- c("mpg", "disp", "hp", "wt", "qsec", "carb", "gear", "drat", "vs", "am", "cyl", "x11")


#2A
mpv_model <- lm(mpg ~ disp, data = table.b3)
summary(mpv_model)
mpv_model
plot(table.b3$disp, table.b3$mpg,
     xlab="Displacement", ylab="MPG",
     main="MPG vs Displacement", pch=19, col="blue")
abline(mpv_model, col="red", lwd=2)

#The slope (b1) is negative, which means that as displacement increases, mpg decreases. 
#For example, when displacement increases by 1 unit, mpg decreases by approximately b1 on average.

#2B
x <- (table.b3$disp)
y <- (table.b3$mpg)
xbar <- mean(table.b3$disp)
ybar <- mean(table.b3$mpg)

b1 <- sum((x-xbar)*(y-ybar))/(sum((x-xbar)^2))
b0<- ybar - b1*xbar
b1
b0
L <- b0 + (b1*x)
L
coef(mpv_model)
c(b0_manual = b0, b1_manual = b1)
#The coefficients I calculated manually are exactly the same as those obtained from the lm() function. 
#This confirms the correctness of the formula.


#3A
pairs(table.b3)
pairs(table.b3 [, c("mpg", "disp", "hp", "wt", "qsec", "carb", "vs", "am", "cyl")],
      main = "Scatterplot Matrix of Selected Variables",
      pch = 19, col = "blue")
#3B

# I handled the missing values in two different ways:


# 1) Removing rows with NA values:
table.b3_noNA <- table.b3[!is.na(table.b3$wt), ]
View(table.b3_noNA)
# This approach is straightforward and I used it for my assignment
# because only 2 rows were removed, so the data loss was minimal.


# 2) Imputing missing values with the median:
#table.b3$wt[is.na(table.b3$wt)] <- meadian(table.b3$wt, na.rm = TRUE)
#View(table.b3)
# This way I keep all the rows, but I add "artificial" values,
# which might slightly distort the distribution.

#3C
library(MPV)
data("table.b3")
d <- table.b3_noNA
keep <- intersect(c("mpg", "disp", "hp", "wt", "qsec", "carb", "vs", "am", "cyl"),
                  names(d))
pairs(d[, keep], main = "Scatterplot Matrix")

chosen2 <- c("hp", "qsec")

# 4) multiple linear regression
m3 <- lm(mpg ~ disp + hp + qsec, data = d) 
summary(m3)
fitted_vals  <- fitted(m3)
residual_vals <- resid(m3)
head(fitted_vals); head(residual_vals)
coef(m3)

#The coefficients for displacement and horsepower are negative, indicating that as engine size or horsepower increases, fuel efficiency (mpg) decreases.
# On the other hand, the coefficient for qsec is positive, which means that as the quarter-mile time increases, mpg tends to increase. 
#This suggests that cars with larger engines and higher horsepower consume more fuel, while those with higher qsec values (slower acceleration) are generally more fuel-efficient.


#5
# y, ŷ
mpg    <- d$mpg
mpghat <- fitted(m3)   # = fitted_vals
res  <- resid(m3)    # = residual_vals

#a) RSS ve TSS 

RSS <- sum(res^2)
TSS <- sum( (mpg - mean(mpg))^2 )
#b) TSS - RSS = sum( (ŷ - ȳ)^2 )
lhs <- TSS - RSS
rhs <- sum( (mpghat - mean(mpg))^2 )
c(TSS_minus_RSS = lhs, sum_yhat_centered_sq = rhs, diff = lhs - rhs)
# c) R^2
R2_manual  <- 1 - RSS/TSS
R2_summary <- summary(m3)$r.squared
c(R2_manual = R2_manual, R2_summary = R2_summary, diff = R2_manual - R2_summary)

all.equal(lhs, rhs)
all.equal(R2_manual, R2_summary)

#After handling the missing values, I confirmed that the difference between TSS and RSS equals the variation explained by the fitted values (up to rounding error), 
#and that the manually calculated R² is the same as the R² from the model summary.







