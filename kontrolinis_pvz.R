# 1 uzduotis
lentele <- matrix(c(13, 7, 9, 11), nrow = 2, byrow = TRUE,
                  dimnames = list(Grupe = c("A", "B"),  c("Taip", "Ne")))
lentele
t1 <- chisq.test(lentele, correct = FALSE)
t1
chi2 <- t1$statistic
chi2
n <- sum(lentele)
sqrt(chi2 / n) # phi

a <- 13; b <- 7; c <- 9; d <- 11
phi1 <- abs(a*d - b*c) / sqrt((a+b)*(c+d)*(a+c)*(b+d))  # phi pagal formulę
phi1
# phi = 0,20, todėl ryšys tarp grupės ir atsakymo yra silpnas.

# 2 uzduotis
mat <- c(56, 75, 45, 71, 62, 64, 58, 80, 76, 61)
lie <- c(66, 70, 40, 60, 65, 56, 59, 77, 67, 63)

n <- length(mat)
mat_rang <- rank(mat)
lie_rang <- rank(lie)
r <- sum((mat_rang - (n+1)/2) * (lie_rang - (n+1)/2)) / sqrt(sum((mat_rang - (n+1)/2)^2) * sum((lie_rang - (n+1)/2)^2))
r
cor(mat_rang, lie_rang, method = "spearman")

# Spirmeno koeficientas r=0,67, todėl ryšys tarp matematikos ir lietuvių kalbos įvertinimų yra vidutiniškai stiprus.

# 3 uzduotis

x <- c(2, 5, 3, 1, 4, 6)  # gydytojas 1
y <- c(1, 6, 4, 3, 2, 5)  # gydytojas 2

n <- length(x)
tau <- cor(x, y, method = "kendall")

# Z statistika pagal uždavinio formulę
Z <- 3 * tau * sqrt(n * (n - 1)) / sqrt(2 * (2 * n + 5))
Z   # 1.315

# palyginame su kritine reikšme
qnorm(0.975)# 1.96
abs(Z) > qnorm(0.975) # FALSE, todėl H0 neatmetame

# tau=0,47, todėl gydytojų nuomonės iš dalies sutampa. Tačiau |Z|=1,32 < 1,96, todėl H0 neatmetame.



### 4 uzduotis
# 1
lenta1 <- margin.table(Titanic, margin = c("Class", "Survived"))
lenta11 <- addmargins(lenta1)
lenta11
# Iš viso plaukė 2201 žmogus: išgyveno 711, žuvo 1490. Daugiausia buvo įgulos narių (885), mažiausiai 2 klasės keleivių (285).

# 2
lenta2 <- margin.table(Titanic, margin = "Sex")
lenta2
# vyrų buvo 1731, moterų 470. Vyrų buvo daugiau apie 3,7 karto.

# 3
lenta3 <- margin.table(Titanic, margin = c("Sex","Survived"))
lenta3_1 <- prop.table(lenta3, margin = "Sex")
lenta3_1
# vyro tikimybė išgyventi 0,21 , moters 0,73.Moterys išgyveno maždaug 3,5 karto dažniau nei vyrai.

# 4
lenta3 <- margin.table(Titanic, margin = c("Sex", "Survived"))

sansas_v <- lenta3["Male", "Yes"] / lenta3["Male", "No"]       # 367/1364
sansas_m <- lenta3["Female", "Yes"] / lenta3["Female", "No"]   # 344/126

sansas_v           # 0.269
sansas_m           # 2.730
sansas_v / sansas_m   # 0.0986  (šansų santykis, OR)

# vyro šansas išgyventi 0,27. Žūti buvo labiau tikėtina nei išgyventi. Moters šansas 2,73. Vienai žuvusiai moteriai teko 2,7
# išgyvenusios. OR = 0,10, todėl vyrų šansai išgyventi buvo apie 10 kartų mažesni nei moterų. Lytis ir išgyvenamumas stipriai susiję.

# 5
lenta3 <- margin.table(Titanic, margin = c("Sex", "Survived"))

test <- chisq.test(lenta3, correct = FALSE)
test
qchisq(0.95, df = 1)
test$statistic    # χ²

# H0 (lytis ir išgyvenamumas nepriklausomi) atmetame, nes chi2 = 456,9 > 3,84. Ryšio stiprumas phi = 0,46 (vidutinis).

# 6
lenta1 <- margin.table(Titanic, margin = c("Survived", "Class"))
lenta1

barplot(lenta1, beside = TRUE, legend.text = TRUE,
        col = c("black", "white"),
        main = "Išgyvenamumas pagal klasę (dažniai)",
        xlab = "Klasė", ylab = "Žmonių skaičius")
prop.table(lenta1, margin = 2)

# grafikas rodo, kad tik 1 klasėje išgyvenusiųjų (203) daugiau nei žuvusiųjų (122); 2 klasėje, 3 klasėje ir įgulos tarpe žuvusiųjų daugiau.
# Išgyvenusiųjų dalis mažėja nuo 62 % (1 klasė) iki 41 % (2 klasė), 25 % (3 klasė) ir 24 % (įgula), todėl išgyvenamumas priklauso nuo klasės.

# 7
lenta7 <- margin.table(Titanic, margin = c("Sex", "Age", "Survived"))
ftable(lenta7)

# sąlyga: amžius ir išgyvenamumas (2 ir 3 dimensijos), todėl margin = c(2, 3)
p7 <- prop.table(lenta7, margin = c(2, 3))
ftable(p7)

# tik moterų eilutė
ftable(p7["Female", , ])
# tarp suaugusių išgyvenusiųjų moterys sudarė 48 %, o tarp žuvusiųjų tik 7,6 %. Tarp vaikų: 49 % išgyvenusiųjų ir 33 %
# žuvusiųjų. Lytis susijusi su išgyvenamumu abiejose amžiaus grupėse, bet suaugusiems ryšys daug stipresnis.

# 8
lenta8 <- margin.table(Titanic, margin = c("Age", "Survived", "Sex"))
ftable(lenta8)

vyrai   <- lenta8[, , "Male"]
moterys <- lenta8[, , "Female"]

t_v <- chisq.test(vyrai,   correct = FALSE)
t_m <- chisq.test(moterys, correct = FALSE)
t_v; t_m

X2 <- t_v$statistic + t_m$statistic
X2
pchisq(X2, df = 2, lower.tail = FALSE)
qchisq(0.95, df = 2)  # kritinė reikšmė 5.99

# H0 (amžius ir išgyvenamumas nepriklausomi, kai lytis žinoma) atmetame, nes chi2 = 26,18 > 5,99.
