% Nacitanie premennej
vina = read.csv("Cvičenia/3. Cvičenie/wine.csv")

% Informacie o atributoch
str(vina)

% Linearna regresia
% y = b0 + b1 * x1
% b0, b1 -> nezavisle premenne
% x1 -> zavisla premenna

% Slaba zavislost
% Nie je vidiet, ze by data boli ulozene v priamke
% Mozno povedat, ze starsie vina su o trochu drahsie
% Prva hodnota je x (nezavisla premenna)
% Druha hodnota je y (zavisla premenna)
plot(vina$Age, vina$Price)

% Este slabsia zavislost
plot(vina$WinterRain, vina$Price)

% Stredna zavislost
% Krivka klesa => nepriama umera (cim viac zrazok, tym mensia cena)
% Jedno rastie a druhe klesa
plot(vina$HarvestRain, vina$Price)

% Stredna zavislost
% Krivka rastie => priama umera (cim viac AGST, tym vyssia cena)
% Jedno rastie, aj druhe rastie
plot(vina$AGST, vina$Price)

% Z toho vieme:
% y = b0 + b1 * AGST (y reprezentuje cenu)
% cena = 2 + 4 * 17.10
% cena1 odhadovana = 70.40
% cena1 skutocna = 7.50
% chyba = 62.9^2
% Suma chyb SSE -> chceme, aby bola minimalna (suma stvorcov chyb)

% lm -> linearny model
% a ~ b -> a je zavisle od b
% Netreba davat vina$<Atribut>, nakolko tam mam argument data
% Z tohto argumentu vyplyva s akymi datami pracujem
model1 = lm(Price ~ AGST, data = vina)


% Informacie o tomto modeli
summary(model1)

% Vo vystupe call znazornuje, co volam
% Residuals -> reziduum, deskriptivna statistika o chybach

% Coefficients -> koeficienty b0,..., bn (v tomto pripade mame iba b0 a b1)
% (Intercept) Estimate = b0 => riadok, ktory sa viaze ku konstante
% AGST Estimate = b1 => riadok, ktory sa viaze ku parametru AGST
% Estimate -> najlepsie mozne hodnoty parametrov

% => y = -3.41 + 0.63 * AGST
% Napriklad: -3.41 + 0.63 * 17.1167 = 7.37 (17.1167 je 1. hodnota AGST)
% Skutocnost: 7.50
% Chyba: 7.50 - 7.36 = 0.14

% Std. Error = Standard Error -> viaze sa k vzorke
% Hovori o tom ako vierohodny je odhad
% Cim vyssi je Std. Error, tym je model horsi
% Vyjadruje ako daleko sme od priemeru
% Std. Error = Std. deviation / sqrt(n)
% Standardna chyba koeficientu v tejto tabulke (<= nie som si isty, ci je to spravna informacia)


% Statisticke testovanie hypotez (STH)
% H0, H1 -> snazim sa vyvratit pravdivost H0
% Vypocitana statistika > kriticke hranice => prijimam H1
% Vypocitana statistika < kriticke hranice => prijimam H0

% H0 => parameter je statisticky nevyznamny || nezavislost parametrov
% H1 => parameter je statisticky vyznamny || zavislost parametrov

% Kriticke hranice (hladina vyznamnosti, alfa) -> urcujeme si ju my

% p-value -> p-hodnota
% p-hodnota > alfa => prijimam H0
% p-hodnota < alfa => prijimam H1
% PR(>|t|) -> p-hodnota pre jednotlive koeficienty

% Napriklad:
% p-hodnota 0.000335 < 0.1 => prijimam H1
% p-hodnota 0.000335 < 0.05 => prijimam H1
% p-hodnota 0.000335 < 0.01 => prijimam H1
% p-hodnota 0.000335 < 0.001 => prijimam H1

% Parameter AGST je silno statisticky vyznamny
% * za AGST riadkom predstavuju, ze je silno vyznamnmy
% Signif. codes, p-hodnota
% ***, [0, 0.001]
% **, (0.001, 0.01]
% *, (0.01, 0.05]
% ., (0.05, 0.1]
%  , (0.1, 1]

% F-statistic:
% ak = 0 => H0
% ak != 0 => H1


% Druhy model
model2 = lm(Price ~ AGST + HarvestRain, data = vina)

% Prehlad o modeli
summary(model2)

% Oba parametre su vyznamne na vsetkych 4 hladinach vyznamnosti
% Miera vysvetlenia je 0.7074
% 70 % variability ceny viem vysvetlit pomocou tychto dvoch premennych

% Adjusted R-squared: 0.6808


% SSE

% Vycislene chyby
model2$residuals

SSE1 = sum(model1$residuals^2)
SSE1

SSE2 = sum(model2$residuals^2)
SSE2

% Cim mensi SSE, tym lepsi model
% SSE1 > SSE2 => model2 je lepsi


% Treti model
model3 = lm(Price ~ AGST + HarvestRain + Age + FrancePop, data = vina)
summary(model3)

% model3 je este lepsi ako model2
% Avsak z Pr(>|t|) vidime, ze Age a FrancePop su zbytocne
SSE3 = sum(model3$residuals^2)
SSE3


% Stvrty model
model4 = lm(Price ~ AGST + HarvestRain + Age, data = vina)
summary(model4)
% Teraz uz je parameter Age vyznamny na 2 hladinach alfa

SSE4 = sum(model4$residuals^2)
SSE4


% Nepriama umera
plot(vina$FrancePop, vina$Age)

% Korelacia:
% +1 => perfektny pozitivny linearny vztah (priama umera)
% -1 => perfektny negativny linearny vztah (nepriama umera)
%  0 => ziaden linearny vztah

% V tomto pripade silna negativna korelacia
cor(vina$FrancePop, vina$Age)
% Chcem aby zavislost medzi zavislou a nezavislou premennou bola co najvyssia
% Chcem aby zavislost medzi dvomi nezavislymi premennymi bola co najnizsia
% (tieto 2 riadky treba overit z prednasky)

% To co je viac ako 0.6 => nedavat do modelu
cor(vina)


% Piaty model
model5 = lm(Price ~ AGST + HarvestRain + Age + WinterRain, data = vina)
summary(model5)

% SSE
sse(vina$Price, model5$fitted.values)
SSE5 = sum(model5$residuals^2)
SSE5

% MSE
mse(vina$Price, model5$fitted.values)
MSE5 = SSE5 / length(model5$residuals)
MSE5

% RMSE
rmse(vina$Price, model5$fitted.values)
RMSE5 = sqrt(MSE5)
RMSE5

% MPE
MPE5 = sum(model5$residuals / vina$Price) / length(model5$residuals)
MPE5

% MAPE
mape(vina$Price, model5$fitted.values)
MAPE5 = sum(abs(model5$residuals / vina$Price)) / length(model5$residuals)
MAPE5


% Iny sposob na vypocet MPE
sum((vina$Price - model5$fitted.values) / vina$Price) / length(model5$residuals)

% Iny sposob na vypocet MAPE
sum((abs(vina$Price - model5$fitted.values)) / vina$Price) / length(model5$residuals)


% Test
str(vina)
modelT = lm(Price ~ HarvestRain + Age, data = vina)
summary(modelT)

SSET = sum(modelT$residuals^2)
SSET

RMSE = sqrt(mean((vina$Price - modelT$fitted.values)^2))
RMSE

install.packages("Metrics")
library(Metrics)

rmse(vina$Price, modelT$fitted.values)


% Chyby

sse5 = sum(model5$residual^2)
sse5

mse5 = sse5 / length(model5$residuals)
mse5

rmse5 = sqrt(mse5)
rmse5

mape5 = (sum(abs(model5$residual)/vina$Price)) / length(model5$residuals)
mape5