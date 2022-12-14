galaxy = read.csv("galaxy3.csv");
str(galaxy)

% 1.1
galaxy$Year[which.min(galaxy$CPI_energy)]
galaxy$Month[which.min(galaxy$CPI_energy)]


% 1.2
median(galaxy$SamsungGalaxySales)


% 1.3
rok2011 = subset(galaxy, Year == 2011)
str(rok2011)
mean(rok2011$Unemployment)


% 1.4
tapply(galaxy$Unemployment, galaxy$Year, mean)


% 1.5
nad8perc = subset(galaxy, Unemployment > 8)
str(nad8perc)


% 1.6
set.seed(100)
trenovacia = subset(galaxy, Year <= 2012)
testovacia = subset(galaxy, Year > 2012)
str(trenovacia)
str(testovacia)


% 2.1
model1 = lm(SamsungGalaxySales ~ Unemployment + CPI_all + CPI_energy + GoogleQueries, data = trenovacia)
summary(model1)


% 2.2
% ziadny, Pr(>|t|) musi byt mensie ako 0.10


% 2.3
model2 = lm(SamsungGalaxySales ~ Unemployment + CPI_all + CPI_energy + GoogleQueries + Month, data = trenovacia)
summary(model2)
% model je horsi pretoze jeho Adjusted R-squared je mensie (hladim na Adjusted R-squared, nie na R-squared)


% 2.4
trenovaciaOpravena = trenovacia;
trenovaciaOpravena$Month = as.factor(trenovaciaOpravena$Month)
str(trenovaciaOpravena)

testovaciaOpravena = testovacia;
testovaciaOpravena$Month = as.factor(testovaciaOpravena$Month)
str(testovaciaOpravena)

model3 = lm(SamsungGalaxySales ~ Unemployment + CPI_all + CPI_energy + GoogleQueries + Month, data = trenovaciaOpravena)
summary(model3)


% 2.5
% lepsi, jeho Adjusted R-squared je vacsie


% 2.6
% 12 parametrov
% 11 premennych -> (Intercept) je iba iba parametrom, ale nie je premennou


% 2.7
% 2 parametre


% 2.8
cor(galaxy)
% s Year, s Unemployment, s GoogleQueries, s CPI_all
% na cviceni vravel, ze aj (-1; -0.6) sa pocita ale v odpovedi su iba 3


% 2.9
model4 = lm(SamsungGalaxySales ~ Unemployment + CPI_all + CPI_energy + Month, data = trenovaciaOpravena)
model4Test = predict(model4, newdata = testovaciaOpravena)

sum((model4Test - testovaciaOpravena$SamsungGalaxySales)^2)
% z nejakeho dovodu nefunguje


% 2.10
model5 = lm(SamsungGalaxySales ~ Unemployment + CPI_all + CPI_energy + Month, data = trenovaciaOpravena)
summary(model5)


% 2.11
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
strom1 = rpart(SamsungGalaxySales ~ Unemployment + CPI_all + CPI_energy + Month, data = trenovaciaOpravena)
prp(strom1)


% 2.12
predictStrom1 = predict(strom1, newdata = testovaciaOpravena)
sseStrom1 = sum((predictStrom1 - testovaciaOpravena$SamsungGalaxySales)^2)
mseStrom1 = sseStrom1 / length(predictStrom1)
mseStrom1


% 2.13
strom2 = rpart(SamsungGalaxySales ~ Unemployment + CPI_all + CPI_energy + Month, data = trenovaciaOpravena, minbucket = 3)
prp(strom2)


% 2.14
predictStrom2 = predict(strom2, newdata = testovaciaOpravena)
sseStrom2 = sum((predictStrom2 - testovaciaOpravena$SamsungGalaxySales )^2)
mseStrom2 = sseStrom2 / length(predictStrom2)
mseStrom2


% 2.16
set.seed(100)
install.packages("randomForest")
library(randomForest)
nahodnyLes1 = randomForest(SamsungGalaxySales ~ Unemployment + CPI_all + CPI_energy + Month, data = trenovaciaOpravena, nodesize = 1, ntree = 20000)
predictTestNahodnyLes1 = predict(nahodnyLes1, newdata = testovaciaOpravena)

sseNahodnyLes1 = sum((predictTestNahodnyLes1 - testovaciaOpravena$SamsungGalaxySales)^2)
mseNahodnyLes1 = sseNahodnyLes1 / length(predictTestNahodnyLes1)
rmseNahodnyLes1 = sqrt(mseNahodnyLes1)
rmseNahodnyLes1


% 3.1
nizkePredaje = subset(trenovaciaOpravena, LowSales == 0)
str(nizkePredaje)
trenovaciaOpravena


% 3.2
install.packages("caTools")
library(caTools)
max(nrow(trenovaciaOpravena[trenovaciaOpravena$LowSales == TRUE, ]), nrow(trenovaciaOpravena[trenovaciaOpravena$LowSales == FALSE, ])) / nrow(trenovaciaOpravena)


% 3.3
logReg1 = glm(LowSales ~ Unemployment + CPI_all + CPI_energy + GoogleQueries, data = trenovaciaOpravena, family = binomial)
summary(logReg1)




% 3.4
predictTestLogReg1  = predict(logReg1, type = "response", newdata = testovaciaOpravena)
table(testovaciaOpravena$LowSales, predictTestLogReg1 > 0.15)
12 / (2 + 12)


% 3.5
stromClass1 = rpart(LowSales ~ Unemployment + CPI_all + CPI_energy + GoogleQueries, data = trenovaciaOpravena, method = "class")

predictTrainStromClass1 = predict(stromClass1, type = "class")
table(trenovaciaOpravena$LowSales, predictTrainStromClass1)

predictTestStromClass1 = predict(stromClass1, type = "class", newdata = testovaciaOpravena)
table(testovaciaOpravena$LowSales, predictTestStromClass1)


% 3.6
set.seed(1)
trenovaciaOpravena$LowSales = as.factor(trenovaciaOpravena$LowSales)
testovaciaOpravena$LowSales = as.factor(testovaciaOpravena$LowSales)

nahodnyLesClass1 = randomForest(LowSales ~ Unemployment + CPI_all + CPI_energy + GoogleQueries, data = trenovaciaOpravena, ntree = 20000)

predictTrainRF1 = predict(nahodnyLesClass1)
table(trenovaciaOpravena$LowSales, predictTrainRF1)

predictTestRF1 = predict(nahodnyLesClass1, newdata = testovaciaOpravena)
table(testovaciaOpravena$LowSales, predictTestRF1)