% Stevens

Stevens = read.csv("Cvičenia/5. Cvičenie/stevens.csv")
set.seed(1000)
library(caTools)

split = sample.split(Stevens$Reverse, SplitRatio = 0.7)
train = subset(Stevens, split == TRUE)
test = subset(Stevens, split == FALSE)

library(rpart)
library(rpart.plot)

Strom = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", cp = 0.0001)
prp(Strom)

predictTrain = predict(Strom25, type = "class")
table(train$Reverse, predictTrain)


% Boston

boston = read.csv ("Cvičenia/6. Cvičenie/boston.csv")

% MEDV je medianova cena nehnutelnosti v danej lokalite
str(boston)

% Zemepisna dlzka, zemepisna sirka
plot(boston$LON, boston$LAT)

% Miesta pri rieke
% Pred tym ako zavolam funkciu points, musim mat uz zavolany plot
points(boston$LON[boston$CHAS == 1], boston$LAT[boston$CHAS == 1], col = "blue", pch = 19)

% Lokality, kde je nadpriemerna miera kriminality vykreslit zelenou farbou
points(boston$LON[boston$CRIM > mean(boston$CRIM)], boston$LAT[boston$CRIM > mean(boston$CRIM)], col="green")

% Cervenou nehnutelnosti, kde medianova cena je viac ako median medianov
points(boston$LON[boston$MEDV > median(boston$MEDV)], boston$LAT[boston$MEDV > median(boston$MEDV)], col="red")
% Viac ako median je polovica, menej ako median je polovica


% Linearna regresia

Lregresia = lm(MEDV ~ LAT + LON, data = boston)
summary(Lregresia)
% LON je statisticky vyznamny parameter
% LAT nie je statisticky vyznamny parameter
% Miera vysvetlenia je 0.1072 => 10 % variability ceny vieme vysvetlit tymto modelom
% Tento model nie je dobry

plot(boston$LAT, boston$MEDV)
% Slaba miera korelacia, LAT na cenu nevplyva
cor(boston$LAT, boston$MEDV)

plot(boston$LON, boston$MEDV)
% Slaba miera korelacia, avsak vacsia ako pri LAT
% Slaba negativna korelacia
cor(boston$LON, boston$MEDV)


% Predikcie
predictTrain = predict(Lregresia)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV > median(boston$MEDV)], boston$LAT[boston$MEDV > median(boston$MEDV)], col="red")
% Do pch mozem dat aj nejaky znak
points(boston$LON[predictTrain > median(boston$MEDV)], boston$LAT[predictTrain > median(boston$MEDV)], col="blue", pch = "$")
% Tento model rozdelil mapu na dve casti, jednu drahu, jednu lacnu
% Model si mysli, ze cim viac ideme na zapad, tym su nehnutelnosti drahsie


% Rozhodovaci strom

library(rpart)
library(rpart.plot)

% Regresny problem
% Cena nie je kategoricka premenna ale numericka
% Ak nezadam argument method, tak sa to berie ako regresia
% Nezadany moze byt aj minbucket, R zvoli nejaku hodnotu
Lstrom = rpart(MEDV ~ LAT + LON, data = boston)
prp(Lstrom)
% Strom vyuziva premenne opatovne

predictTrainStrom = predict(Lstrom)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV > median(boston$MEDV)], boston$LAT[boston$MEDV > median(boston$MEDV)], col="red", pch = 18)
points(boston$LON[predictTrainStrom > median(boston$MEDV)], boston$LAT[predictTrainStrom > median(boston$MEDV)], col="purple", pch = "$")

% Iny sposob zobrazenia stromu
% Teraz vidim aj desatinne miesta
plot(Lstrom)
text(Lstrom)

% 1. Split, ktory strom vykona
abline(v = -71.07)
% 2. Split
abline(h = 42.28)
% Teraz uz vidime, ze vpravo hore strom predikuje cenu 22.99

% Modelovanie nelinearnych zavislosti

str(boston)


% Nastavit seed na 123
% Rozdelit data na 2 casti: 8 (trenovacia) ku 2 (testovacia)
% Natrenovat model LR, kde zavisla premenna je MEDV
% Nezavisle premenne su LAT, LON, CRIM, ZN, INDUS, CHAS, NOX, RM, AGE, DIS, TAX, PTRATIO 
% Vypocitat SEEtest, MSEtest, RMSEtest

% To iste urobit na strome
% Vypocitat SSEtest, MSEtest, RMSEtest

% MAPEtest na oboch

set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.8)

train = subset(boston, split == TRUE)
test = subset(boston, split == FALSE)

modelA = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + TAX + PTRATIO, data = train)
predictL = predict(modelA, newdata = test)
% Residuals mam iba na train mnozine

sseA = sum((predictL - test$MEDV)^2)
sseA

mseA = sseA / length(predictL)
mseA

rmseA = sqrt(mseA)
rmseA

mapeA = (sum(abs(predictL - test$MEDV)/test$MEDV)) / length(predictL) * 100
mapeA

StromA = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + TAX + PTRATIO, data = train)
predictTest = predict(StromA, newdata = test)

sseB = sum((predictTest - test$MEDV)^2)
sseB

mseB = sseB / length(predictTest)
mseB

rmseB = sqrt(mseB)
rmseB

mapeB = (sum(abs(predictTest - test$MEDV)/test$MEDV)) / length(predictTest) * 100
mapeB


% Test
library(rpart)
library(rpart.plot)
library(caTools)

Stevens = read.csv("Cvičenia/5. Cvičenie/stevens.csv")

split = sample.split(Stevens$Reverse, SplitRatio = 0.7)
train = subset(Stevens, split == TRUE)
test = subset(Stevens, split == FALSE)

Strom = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", cp = 0.0001)
prp(Strom)

predictTrain = predict(Strom, type = "class")
table(train$Reverse, predictTrain)

predictTest = predict(Strom, type = "class", newdata = test)
table(test$Reverse, predictTest)