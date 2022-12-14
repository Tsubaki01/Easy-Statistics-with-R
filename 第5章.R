# 図5.1
A <- rnorm(50)
B <- rnorm(50)
C <- A * 0.5 + sqrt(0.75) * B
自尊感情 <- 10 * A + 50
ソーシャルスキル <- 10 * C + 50

plot(自尊感情, ソーシャルスキル, xlim = c(0, 100), ylim = c(0, 100))
points(50, 50, cex = 45)
legend(50, 10, legend = "抽出された標本に含まれるデータ", pch = 1)
legend(10, 100, legend = "円の中全体にデータが散らばっていて相関はゼロ", pch = 1)


# 標準正規分布を用いた検定（1つの平均値の検定・母分散σ2が既知）
心理学テスト <- c(13,14,7,12,10,6,8,15,4,14,9,6,10,12,5,12,8,8,12,15)

Z分子 <- mean(心理学テスト) - 12
Z分母 <- sqrt(10 / length(心理学テスト))
Z統計量 <- Z分子 / Z分母

qnorm(0.025)
qnorm(0.975)
  # Z統計量が棄却域に入っているため、対立仮説が5%水準で有意であることが分かる

  # p値以下となる確率を直接求める方法
  pnorm(Z統計量)

  
# 図5.2
curve(dnorm(x), -3, 3)
abline(v = qnorm(0.025))
abline(v = qnorm(0.975))


# 図5.3(T分布)
curve(dt(x, 8), -5, 5)
curve(dt(x, 4), -5, 5, add = TRUE)
curve(dt(x, 2), -5, 5, add = TRUE)
curve(dt(x, 1), -5, 5, add = TRUE)


# t分布を用いた検定（1つの平均値の検定・母分散σ2が未知）
心理学テスト <- c(13,14,7,12,10,6,8,15,4,14,9,6,10,12,5,12,8,8,12,15)

t分子 <- mean(心理学テスト) - 12
t分母 <- sqrt(var(心理学テスト) / length(心理学テスト))
t統計量 <- t分子 / t分母

qt(0.025, 19) # 自由度19のt分布で下側確率0.025となるtの値を求める
qt(0.975, 19) # 自由度19のt分布で下側確率0.975となるtの値を求める

# t統計量が棄却域に入っているため、対立仮説が5%水準で有意であることが分かる
t統計量 < qt(0.025, 19)

# p値以下となる確率を直接求める方法
pt(t統計量, 19)


# 図5.4(t分布の棄却域)
curve(dt(x, 19), -3, 3)
abline(v = qt(0.025, 19))
abline(v = qt(0.975, 19))


# Rでは一行の命令でt検定を行う関数が用意されている
t.test(心理学テスト, mu = 12)


# 相関係数の検定（無相関検定）
統計テスト1 <- c(6,10,6,10,5,3,5,9,3,3,11,6,11,9,7,5,8,7,7,9)
統計テスト2 <- c(10,13,8,15,8,6,9,10,7,3,18,14,18,11,12,5,7,12,7,7)

標本相関 <- cor(統計テスト1, 統計テスト2)
サンプルサイズ <- length(統計テスト1)

t分子 <- 標本相関 * sqrt(サンプルサイズ - 2)
t分母 <- sqrt(1 - 標本相関 ^ 2)
t統計量 <- t分子 / t分母

qt(0.025, 18) # 自由度18のt分布で下側確率0.025となるtの値を求める
qt(0.975, 18) # 自由度18のt分布で下側確率0.975となるtの値を求める

# t統計量が棄却域に入っているため、対立仮説が5%水準で有意であることが分かる
t統計量 > qt(0.975, 19)

# p値以上となる確率を直接求める方法
2 * pt(t統計量, 18, lower.tail = FALSE)

# Rでは一行の命令で無相関検定を行う関数が用意されている
cor.test(統計テスト1, 統計テスト2)


# 図5.5(無相関検定 t分布の棄却域)
curve(dt(x, 18), -3, 3)
abline(v = qt(0.025, 18))
abline(v = qt(0.975, 18))


# 図5.6(カイ二乗分布)
curve(dchisq(x, 2), 0, 20)
curve(dchisq(x, 1), 0, 20, add = TRUE)
curve(dchisq(x, 4), 0, 20, add = TRUE)
curve(dchisq(x, 8), 0, 20, add = TRUE)

curve(dchisq(x, 50), 0, 100)


# 独立性の検定（カイ二乗検定）
期待度数イチイチ <- 12 * 14 / 20
期待度数ニイチ <- 12 * 6 / 20
期待度数イチニ <- 8 * 14 / 20
期待度数ニニ <- 8 * 6 / 20

期待度数 <- c(期待度数イチイチ, 期待度数ニイチ, 期待度数イチニ, 期待度数ニニ)
観測度数 <- c(10,2,4,4)

カイ二乗要素 <- (観測度数 - 期待度数) ^ 2 / 期待度数
カイ二乗 <- sum(カイ二乗要素)

qchisq(0.95, 1) # 自由度1のカイ二乗分布で下側確率0.95となるx^2の値を求める

# 検定統計量x^2が棄却域に入っていないため、5%水準で有意な関連が無いことがわかる
カイ二乗 > qchisq(0.95, 1)

# p値以上となる確率を直接求める方法
pchisq(カイ二乗, 1, lower.tail = FALSE)

# Rでは一行の命令でカイ二乗検定を行う関数が用意されている
数学 <- c("嫌い","嫌い","好き","好き","嫌い","嫌い","嫌い","嫌い","嫌い","好き","好き","嫌い","好き","嫌い"
        ,"嫌い","好き","嫌い","嫌い","嫌い","嫌い")
統計 <- c("好き","好き","好き","好き","嫌い","嫌い","嫌い","嫌い","嫌い","嫌い","好き","好き","好き","嫌い","好き","嫌い","嫌い","嫌い","嫌い","嫌い")
クロス集計表 <- table(数学, 統計)
chisq.test(クロス集計表, correct = FALSE)



# 図5.7(カイ二乗検定の棄却域)
curve(dchisq(x, 1), 0, 6)
abline(v = qchisq(0.05, 1, lower.tail = FALSE))


# サンプルサイズの検定結果への影響について
履修A <- matrix(c(16,12,4,8), 2, 2)
履修A
rownames(履修A) <- c("文系", "理系")
colnames(履修A) <- c("履修した", "履修しない")
履修A

chisq.test(履修A, correct = FALSE)

履修B <- matrix(c(160,120,40,80), 2, 2)
履修B
rownames(履修B) <- c("文系", "理系")
colnames(履修B) <- c("履修した", "履修しない")
履修B

chisq.test(履修B, correct = FALSE)
