% Nacitanie dat
Framingham = read.csv("Cvičenia/4. Cvičenie/framingham.csv")

% Informacie o datach
str(Framingham)
summary(Framingham)

% Riesenie NA v datach
% - vymazem ich
% - nahradim ich

% Odstranenie vsetkych NA zaznamov
Framingham = na.omit(Framingham)

% Kolko muzov a kolko zien ma v TenYearCHD hodnotu 1
% 1. Sposob
% 1. Hodnota je stlpec
% 2. Hodnota je riadok
table(Framingham$male, Framingham$TenYearCHD)

% 2. Sposob
Muzi = subset(Framingham, Framingham$male == 1 )
Zeny = subset(Framingham, Framingham$male != 1)

MuziCHD = subset(Muzi, Muzi$TenYearCHD == 1)
ZenyCHD = subset(Zeny, Zeny$TenYearCHD == 1)

str(MuziCHD)
str(ZenyCHD)

% Kolko percent muzov a kolko percent zien ma TenYearCHD 1
tapply(Framingham$TenYearCHD, Framingham$male, mean)
nrow(MuziCHD) / nrow(Muzi)
nrow(ZenyCHD) / nrow(Zeny)

% Kolko cigariet vyfajci najsilnejsi fajciar a kolko ma rokov
Framingham$age[which.max(Framingham$cigsPerDay)]
Framingham$cigsPerDay[which.max(Framingham$cigsPerDay)]


% Model logistickej regresie
% 1. Ocistenie datasetu -> pomocou na.omit()
% 2. Rozdelenie dat na 2 mnoziny -> trenovaciu a testovaciu
%    -> rozdelime nahodne

% Nainstalovanie baliku
install.packages("caTools")
library(caTools)

% Nastavenie nasady
set.seed(100)

% 1. Rozdelenie dat
% -> 1. Parameter znaci, podla ktorej premennej sa data rozdeluju
% -> 2. Parameter znaci pomer, v ktorom sa data rozdelia
%    30 % : 70 %
% 2. Odhad parametrov modelu logistickej regresie
% 3. Predikcie na mnozine train
     -> test na "necisto"
% 4. Presnost na testovacej mnozine
split = sample.split(Framingham$TenYearCHD, SplitRatio = 0.7)
split

% Trenovacia mnozina
train = subset(Framingham, split == TRUE)

% Testovacia mnozina
test = subset(Framingham, split == FALSE)

% Pocet riadkov
nrow(train)
nrow(test)

% Model
% "." znamena vsetky premenne okrem TenYearCHD
LogReg = glm(TenYearCHD ~ ., data = train, family =  binomial)

% Informacie o modeli
% AIC je metrika, ktora sluzi na hodnotenie modelu
summary(LogReg)

% 4 parametre su vyznamne na hladine vyznamnosti 0.001
% 6 parametrov je vyznamnych na hladine vyznamnosti 0.05
% 5 premennych je vyznamnych na hladine vyznamnosti 0.05
% Premenna a parameter nie je to iste
% Intercept je len parametrom, ale nie je premennou

% "response" predikuje pravdepodobnost
predictTrain = predict(LogReg, type = "response")
predictTrain


% Klasifikacna matica
% Hovori o tom ako je model presny
% 0.5 je threshold value
table(train$TenYearCHD, predictTrain > 0.5)

% Celkova kvalita modelu
(2159 + 34) / (2159 + 12 + 356 + 34)

% Specificita
2159 / (2159 + 12)

% Sensitivita
34 / (356 + 34)


% Threshold value 0.7
table(train$TenYearCHD, predictTrain > 0.7)

% Celkova kvalita modelu
(2170 + 6) / (2170 + 1 + 384 + 6)

% Specificita
2170 / (2170 + 1)

% Sensitivita
6 / (384 + 6)


% Threshold value 0.3
table(train$TenYearCHD, predictTrain > 0.3)

% Celkova kvalita modelu
(1998 + 111) / (1998 + 173 + 279 + 111)

% Specificita
1998 / (1998 + 173)

% Sensitivita
111 / (279 + 111)


% Pouzijeme mean (0.15) a median (0.12) ako hodnotu t
summary(predictTrain)

% Threshold value 0.15 -> mean
table(train$TenYearCHD, predictTrain > 0.15)
(1452 + 266) / (1452 + 719 + 124 + 266)
1452 / (1452 + 719)
266 / (124 + 266)

% Threshold value 0.12 -> median
table(train$TenYearCHD, predictTrain > 0.12)
(1229 + 300) / (1229 + 942 + 90 + 300)
1229 / (1229 + 942)
300 / (90 + 300)

% Threshold value 0.20 -> treti kvartil
table(train$TenYearCHD, predictTrain > 0.20)
(1732 + 210) / (1732 + 439 + 180 + 210)
1732 / (1732 + 439)
210 / (180 + 210)

% Threshold value 0.05
table(train$TenYearCHD, predictTrain > 0.05)
(399 + 373) / (399 + 1772 + 17 + 373)
399 / (399 + 1772)
373 / (17 + 373)
% Tento model zabezpeci, ze vacsina ludi, ktori dostanu infarkt su vopred diagnostikovani
% Jeho nevyhodou je to, ze vela ludi identifikuje, ze infarkt dostanu hoci ho nedostanu

% Threshold value 0.8
table(train$TenYearCHD, predictTrain > 0.80)
(2170 + 2) / (2170 + 1 + 388 + 2)
2170 / (2170 + 1)
2 / (388 + 2)
% Takyto model identifikuje len tych, ktori su velmi nachylni na to, ze dostanu infarkt
% Prioritizuje pacientov, ktori su najviac rizikovi


% Vyber t:
% 1. Programatorsky -> skusam velky pocet hodnot
% 2. Analyticky -> vychadzam zo summary
% 3. Ine


% Testovanie modelu
% Testovacia mnozina
predictTest = predict(LogReg, type = "response", newdata = test)

% Pouzivam najlepsie t, ktore som si vypocital na testovacej mnozine
table(test$TenYearCHD, predictTest > 0.15)
(638 + 114) / (638 + 292 + 53 + 114)
638 / (638 + 292)
114 / (53 + 114)


% Test

Framingham = read.csv("Cvičenia/4. Cvičenie/framingham.csv")
str(Framingham)
summary(Framingham)
Framingham = na.omit(Framingham)
library(caTools)
set.seed(100)

split = sample.split(Framingham$TenYearCHD, SplitRatio = 0.7)
train = subset(Framingham, split == TRUE)
test = subset(Framingham, split == FALSE)

LogReg = glm(TenYearCHD ~ ., data = train, family =  binomial)
predictTrain = predict(LogReg, type = "response")

table(train$TenYearCHD, predictTrain > 0.5)

predictTest = predict(LogReg, type = "response", newdata = test)
table(test$TenYearCHD, predictTest > 0.15)