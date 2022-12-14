% Nacitanie dat
galaxy = read.csv("Cvičenia/11. Cvičenie/galaxy3.csv")

galaxy
str(galaxy)
summary(galaxy)
nrow(galaxy)


% 1.

% 1.1 V ktorom mesiaci a roku boli najnižšie ceny energií v USA?
galaxy$Month[which.min(galaxy$CPI_energy)]
galaxy$Year[which.min(galaxy$CPI_energy)]
% V 6. mesiaci v roku 2010

% 1.2 Aký je medián predajov mobilov Samsung Galaxy?
Prosím výsledok uveďte na 1 desatinné miesto.
median(galaxy$SamsungGalaxySales, na.rm = TRUE)
% Median = 15624.5

% 1.3 Aký bol priemer nezamestnanosti v roku 2011?
Prosím výsledok uveďte na 4 desatinné miesta.
rok2011 = subset(galaxy, Year == 2011)
mean(rok2011$Unemployment)
tapply(galaxy$Unemployment, galaxy$Year, mean)
% Priemer nezamestnanosti v roku 2011 = 8.9333

% 1.4 V ktorom roku bola v priemere najnižšia mesačná nezamestnanosť?
tapply(galaxy$Unemployment, galaxy$Year, mean)
% V roku = 2014

% 1.5 V koľkých mesiacoch (pozorovaniach) bola nezamestnanosť v USA nad 8 percent?
nad8Percent = subset(galaxy, Unemployment > 8)
nrow(nad8Percent)
% V 32 mesiacoch

% 1.6 Nastavte seed na 100. Rozdeľte dáta na dve množiny,
trénovaciu a testovaciu. Nech v trénovacej množine sú dáta
do roku 2012 (vrátane) a nech v testovacej množine sú ostatné dáta.
Koľko dát je trénovacej a testovacej množine?
set.seed(100)
train = subset(galaxy, Year <= 2012)
test = subset(galaxy, Year > 2012)
nrow(train)
nrow(test)
% V trenovacej je 36 dat, v testovacej je 14 dat


% 2

% 2.1 Vytvorte lineárny regresný model, kde závislá premenná
bude počet predaných mobilov a nezávislé premenné budú nezamestnanosť,
celkové CPI, CPI pre energie, dotazy v Google. Model natrénujte
na trénovacej množine. Aká je hodnota koeficientu determinácie daného modelu?
Výsledok uveďte na 4 desatinné miesta.
linReg = lm(SamsungGalaxySales ~ Unemployment + CPI_all + CPI_energy + GoogleQueries, data = train)
summary(linReg)
% Koeficient determinacie = 0.4282

% 2.2 Koľko parametrov je štatisticky významných hladine významnosti alfa = 0,10?
% 0 parametrov

% 2.3 Vytvorte druhý lineárny regresný model, kde závislá premenná bude
počet predaných mobilov a nezávislé premenné budú nezamestnanosť,
celkové CPI, CPI pre energie, dotazy v Google a mesiac. Model natrénujte
na trénovacej množine. Aká je hodnota koeficientu determinácie daného modelu?
Výsledok uveďte na 4 desatinné miesta. Je model lepší alebo horší ako model1?
linReg2 = lm(SamsungGalaxySales ~ Unemployment + CPI_all + CPI_energy + GoogleQueries + Month, data = train)
summary(linReg2)
% Koeficient determinacie = 0.4344
% Hladim na adjusted R^2 => druhy model je horsi
% Adjusted R^2 hladi aj na pocet premennych v modeli

% 2.4 Vytvorte tretí lineárny regresný model, kde závislá premenná bude
počet predaných mobilov a nezávislé premenné budú nezamestnanosť,
celkové CPI, CPI pre energie, dotazy v Google a mesiac. Premennú mesiac
však pretypujte na správny dátový typ. Model natrénujte na trénovacej množine.
Aká je hodnota koeficientu determinácie daného modelu? Výsledok uveďte na 4 desatinné miesta.
trainFactor = train
trainFactor$Month = as.factor(trainFactor$Month)
linReg3 = lm(SamsungGalaxySales ~ Unemployment + CPI_all + CPI_energy + GoogleQueries + Month, data = trainFactor)
summary(linReg3)
% Koeficient determinacie = 0.8193

% 2.5 Je model zostrojený v otázke 2.4 lepší než model zostrojený v otázke 2.1, resp. 2.3? Prečo?
% Lepsi ako oba predchadzajuce modely, ma vyssi adjusted R^2

% 2.6 Koľko parametrov je štatisticky významných na hladine významnosti
alfa = 0,10 pri modeli z otázky 2.4?
% 12 parametrov

% 2.7 Koľko parametrov je štatisticky významných na hladine významnosti
alfa = 0,001 pri modeli z otázky 2.4?
% 2 parametre

% 2.8 S ktorými premennými je vysoko korelovaná premenná CPI_energy?
(za vysokú koreláciu považujte hodnotu nad 0,6). Premennú mesiac uvažujte
iba v pôvodnom dátovom type.
cor(galaxy)
% S premennymi Year, GoogleQueries, CPI_all, Unemployment
% Do poctu sa pocitaju aj zaporne korelacie

% 2.9 Vytvorte ešte jeden model, ktorý bude mať všetky nezávislé premenné
z otázky 2.4 okrem premennej GoogleQueries (dotazy v Google). Aká je suma
štvorcov chýb tohto modelu na testovacej množine?
linReg4 = lm(SamsungGalaxySales ~ Unemployment + CPI_all + CPI_energy + Month, data = trainFactor)
summary(linReg4)
SSE4 = sum(linReg4$residuals^2)
SSE4

predictTestL = predict(linReg4)
SSETL = sum((predictTestL.fittedValues - trainFactor$SamsungGalaxySales)^2)
SSETL
% SSE = 367 467 641

% 2.10 Aká je hodnota koeficientu determinácie na trénovacej množine pri
modeli z otázky 2.9? Výsledok uveďte na 4 desatinné miesta.
% Koeficient determinacie = 0.3755
0.818

% 2.11 Natrénujte model rozhodovacieho stromu, kde závislá premenná bude
SamsungGalaxySales, nezávislé premenné budú Unemployment, CPI_all, CPI_energy,
MonthFaktor. Hodnotu minbucket, resp. cp nechajte na defaulte. Koľko splitov je v danom strome?
library(rpart)
library(rpart.plot)
trainFactor = train
trainFactor$Month = as.factor(trainFactor$Month)
Strom = rpart(SamsungGalaxySales~ Unemployment + CPI_all + CPI_energy + Month, data = trainFactor)
prp(Strom)
% Pocet splitov = 2

% 2.12 Aká je hodnota MSE modelu rozhodovacieho stromu z problému 2.11 na testovacej množine?
testFactor = train
testFactor$Month = as.factor(testFactor$Month)
predictTest = predict(Strom, newdata = testFactor)
SSE = sum((predictTest - testFactor$SamsungGalaxySales)^2)
MSE = SSE / length(predictTest)
MSE
% ine
% MSE = 6 336 636

% 2.13 Zostrojte druhý model rozhodovacieho stromu, ktorý bude mať rovnakú
závislú a rovnaké nezávislé premenné ako model z otázky 2.10, minbucket však
nastavte na hodnotu 3. Koľko splitov je teraz v strome?
Strom2 = rpart(SamsungGalaxySales~ Unemployment + CPI_all + CPI_energy + Month, data = trainFactor, minbucket = 3)
prp(Strom2)
% Pocet splitov = 5

% 2.14 Aká je hodnota MSE pre model z otázky 2.13 na testovacej množine?
SSE2 = sum((predictTest - testFactor$SamsungGalaxySales)^2)
MSE2 = SSE2 / length(predictTest)
MSE2



















