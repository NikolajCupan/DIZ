% Nacitaine dat
Stevens = read.csv("Cvičenia/5. Cvičenie/stevens.csv")

% 1. Odstranenie NA zaznamov (na.omit)
% 2. Rozdelenie na trenovaciu a testovaciu mnozinu
% 3. Tvorba modelu
% 4. Predikcie na trenovacej mnozine
% 5. Predikcie na testovacej mnozine

library(caTools)
set.seed(1000)

% Rozdelenie dat
split = sample.split(Stevens$Reverse, SplitRatio = 0.7)

% Vytvorenie mnozin
train = subset(Stevens, split == TRUE)
test = subset(Stevens, split == FALSE)

% Vytvorenie modelu rozhodovacieho stromu
% Zavisla premenna nadobuda hodnoty 0 alebo 1
% -> kategoricka premenna => klasifikacia
% -> Avsak dalo by sa to urobit aj s numerickou premennou

% Potrebne packages
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

% method -> druh stromu
% minbucket -> nepovinna premenna
            -> minimalny pocet pozorovani v liste
            -> v kazdom liste musi byt minimalne 25 pozorovani
Strom25 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 25)

summary(Strom25)

% Zobrazenie stromu
prp(Strom25)

% Mensi minbucket -> rozvetvenejsi strom
Strom5 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 5)
prp(Strom5)

% Malo rozvetveny strom
Strom100 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 100)
prp(Strom100)

table(train$LowerCourt)


predictTrain = predict(Strom25, type = "class")

% Niektore indexy chybaju -> su v testovacej mnozine
% Zobrazuje predikovane rozhodnutie sudcu
predictTrain

table(train$Reverse, predictTrain)

% Celkova presnost
(118 + 172) / (118 + 62 + 44 + 172)

% Specificita
118 / (118 + 62)

% Sensitivita
172 / (44 + 172)


% Minbucket 5
predictTrain5 = predict(Strom5, type = "class")
table(train$Reverse, predictTrain5)

total5 = (121 + 193) / (121 + 59 + 23 + 193)
specificita5 = 121 / (121 + 59)
sensitivita5 = 193 / (23 + 193)
ss5 = (specificita5 + sensitivita5) / 2

total5
specificita5
sensitivita5
ss5


% Minbucket 1
Strom1 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 1)

predictTrain1 = predict(Strom1, type = "class")
table(train$Reverse, predictTrain1)

total1 = (124 + 196) / (124 + 56 + 20 + 196)
specificita1 = 124 / (124 + 56)
sensitivita1 = 196 / (20 + 196)
ss1 = (specificita1 + sensitivita1) / 2

total1
specificita1
sensitivita1
ss1

% Minbucket 100
Strom100 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 100)

predictTrain100 = predict(Strom100, type = "class")
table(train$Reverse, predictTrain100)

total100 = (119 + 145) / (119 + 61 + 71 + 145)
specificita100 = 119 / (119 + 61)
sensitivita100 = 145 / (71 + 145)
ss100 = (specificita100 + sensitivita100) / 2

total100
specificita100
sensitivita100
ss100


% Testovacia mnozina
% Minbucket 1
predictTest = predict(Strom1, type = "class", newdata = test)
table(test$Reverse, predictTest)

totalTest1 = (47 + 69) / (47 + 30 + 24 + 69)
specificitaTest1 = 47 / (47 + 30)
sensitivitaTest1 = 69 / (24 + 69)
ssTest1 = (specificitaTest1 + sensitivitaTest1) / 2

totalTest1
specificitaTest1
sensitivitaTest1
ssTest1

% Oproti testovacej mnozine sa kvalita znizila o ~13 %
% Model nie je pretrenovany


% Nahodny les
install.packages("randomForest")
library(randomForest)

% Zmena datoveho typu
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)

set.seed(1)

% nodesize -> obdoba minbucket
% ntree -> pocet stromov v lese
nahodnyLes = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)
% Zavisla premenna je Reverse, nadobuda hodnoty 0 alebo 1
% Reverse ma typ int, preto si randomForest() mysli, ze robim numericku regresiu

predictTestRF = predict(nahodnyLes, newdata = test)
table(test$Reverse, predictTestRF)

totalRF = (50 + 62) / (50 + 27 + 31 + 62)
specificitaRF = 50 / (50 + 27)
sensitivitaRF = 62 / (31 + 62)
ssRF = (specificitaRF + sensitivitaRF) / 2

totalRF
specificitaRF
sensitivitaRF
ssRF

% RF je vseobecne lepsi ako jeden Strom
% Kvalita RF zavisi od vyberu parametrov


% Kolko percent pripadov z konzervativnych sudov sudca zvratil
konzervativny = subset(Stevens, Stevens$LowerCourt == "conser")
konzervativnyZvratil = subset(konzervativny, konzervativny$Reverse == 1)

nrow(konzervativnyZvratil) / nrow(konzervativny) * 100

% Iny sposob
tapply(Stevens$Reverse, Stevens$LowerCourt, mean)


% Kolko pripadov, ktore sudca Stevens riesil v 1998 sa tykalo ustavy
rok98 = subset(Stevens, Stevens$Term == 1998)
ustava = subset(rok98, rok98$Unconst == 0)

nrow(ustava)


% Iny nahodny les
% Mensi CP -> komplexnejsi strom
% Vacsi CP -> menej komplexny strom
% Mozno urobit prezentaciu o CP
StromIny = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", cp = 0.00011)
prp(StromIny)


% Test

Stevens = read.csv("Cvičenia/5. Cvičenie/stevens.csv")
set.seed(1000)

split = sample.split(Stevens$Reverse, SplitRatio = 0.7)
train = subset(Stevens, split == TRUE)
test = subset(Stevens, split == FALSE)

library(rpart)
library(rpart.plot)

Strom25 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 25)
prp(Strom25)

predictTrain = predict(Strom25, type = "class")
table(train$Reverse, predictTrain)


str(Stevens)
Trom = rpart(Reverse ~ LowerCourt + Issue, data = Stevens, method = "class", minbucket = 25)
predictTrom = predict(Trom, type = "class")
table(Stevens$Reverse, predictTrom)