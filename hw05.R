# MA214 -- Problem Set 5

# 1)
data <- read.csv("FCATtxt.csv")

# Using R
model <- lm(data$MATH ~ data$POVERTY)
summary(model)

# Manually
attach(data)
b_1 <- sum( (MATH - mean(MATH)) * (POVERTY - mean(POVERTY)) ) /
  sum( (POVERTY - mean(POVERTY))^2 )
b_1

s2 <- 
  (
    sum( (MATH - mean(MATH))^2 ) -
    b_1 * sum( (MATH - mean(MATH)) * (POVERTY - mean(POVERTY)) )
  ) / (length(MATH) - 2)
s <- sqrt(s2)
s

s_b_1 <- s / sqrt( sum((POVERTY - mean(POVERTY))^2) )
s_b_1

t_stat <- b_1 / s_b_1
t_stat

qt(0.99, length(MATH) - 2)

qt(0.995, length(MATH) - 2)
detach(data)

# 2)
data <- read.csv("OJUICEtxt.csv")
attach(data)
summary(lm(SweetIndex ~ Pectin))

n <- length(SweetIndex)
n

ssxx <- sum( (Pectin - mean(Pectin))^2 )
ssxx
ssyy <- sum( (SweetIndex - mean(SweetIndex))^2 )
ssyy
ssxy <- sum( (Pectin - mean(Pectin)) * (SweetIndex - mean(SweetIndex)))
ssxy

detach(data)

# 3)
data <- read.csv("BBALLtxt.csv")
attach(data)

x_bar <- mean(Resonance)
y_bar <- mean(Frequency)
ssxx <- sum( (Resonance - x_bar)^2 )
ssyy <- sum( (Frequency - y_bar)^2 )
ssxy <- sum( (Resonance - x_bar) * (Frequency - y_bar))
n <- length(Resonance)
setNames(c(x_bar, y_bar, n, ssxx, ssyy, ssxy),
         c("x_bar", "y_bar", "n","ssxx", "ssyy", "ssxy"))

b_1 <- ssxy / ssxx
s <- sqrt( (ssyy - b_1 * ssxy) / (n - 2))
s

qt(0.95, df = 22)

summary(lm(Frequency ~ Resonance))

# https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals
model <- lm(Frequency ~ Resonance)
predict(model, newdata = data.frame(Resonance=10), interval = "confidence", level = 0.90)

detach(data)
