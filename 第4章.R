身長 <- c(165.2,175.9,161.7,174.2,172.1,163.3,170.9,170.6,168.4,171.3)

mean(身長)

var(身長)


# サイコロ　大数の法則
サイコロ6 <- ceiling(runif(n = 6, min = 0, max = 6))
table(サイコロ6)

サイコロ600万 <- ceiling(runif(n = 6000000, min = 0, max = 6))
table(サイコロ600万)


# 性別の母集団分布の例
barplot(c(2/3, 1/3), names.arg = c("男性", "女性"))


# 正規分布
curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 4)
curve(dnorm(x, mean = 1, sd = 1), add = TRUE)
curve(dnorm(x, mean = 0, sd = 2), add = TRUE)

# 単純無作為抽出
標本 <- rnorm(n = 5, mean = 50, sd = 10)
hist(標本)
大標本 <- rnorm(n = 10000, mean = 50, sd = 10)
hist(大標本)

# 正規母集団の母平均の推定
標本平均 <- numeric(length = 10000)
for (i in 1:100000) {
  標本 <- rnorm(n = 10, mean = 50, sd = 10)
  標本平均[i] <- mean(標本)
}
hist(標本平均)
mean(標本平均)
var(標本平均)
hist(標本平均, freq = FALSE)
curve(dnorm(x, mean = 50, sd = sqrt(10)), add = TRUE)
標本平均_10倍 <- numeric(length = 10000)
for (i in 1:100000) {
  標本_10倍 <- rnorm(n = 100, mean = 50, sd = 10)
  標本平均_10倍[i] <- mean(標本_10倍)
}
hist(標本平均_10倍)
mean(標本平均_10倍)
var(標本平均_10倍)
hist(標本平均_10倍, freq = FALSE)
curve(dnorm(x, mean = 50, sd = sqrt(10)), add = TRUE)

# 標準分散と不変分散の標本分布
標本分散 <- numeric(length = 10000)
不偏分散 <- numeric(length = 10000)
for (i in 1:100000) {
  標本 <- rnorm(n = 10, mean = 50, sd = 10)
  標本分散[i] <- mean((標本 - mean(標本)) ^ 2)
  不偏分散[i] <- var(標本)
}
mean(標本分散)
mean(不偏分散)
sd(標本分散)
sd(不偏分散)
hist(標本分散, breaks = seq(0, 500, 10))
hist(不偏分散, breaks = seq(0, 500, 10))


# 中央値の標本分布
標本平均 <- numeric(length = 10000)
標本中央値 <- numeric(length = 10000)
for (i in 1:100000) {
  標本 <- rnorm(n = 10, mean = 50, sd = 10)
  標本平均[i] <- mean(標本)
  標本中央値[i] <- median(標本)
}
mean(標本平均)
mean(標本中央値)
sd(標本平均)
sd(標本中央値)
