% Nacitanie dat
who = read.csv("Cvičenia/8. Cvičenie/WHO.csv")
str(who)

plot(who$GNI, who$FertilityRate)

install.packages("ggplot2")
library(ggplot2)

% ggplot funguje na principe vrstvenia
% Body grafu su osobitna vrstva (geom_point())
% Vrstvy sa na seba pridavaju pomocou +
% Ako 1. parameter uvadzam dataset
gg_graf = ggplot(who, aes(x = GNI, y = FertilityRate))
gg_graf + geom_point()
gg_graf + geom_line()

% Zmena vzhladu
porodnostHNP = gg_graf +
geom_point(color = "blue", size = 2, shape = 17) +
ggtitle("Zavislost porodnosti na HNP") +
xlab("hruby narodny prijem") +
ylab("porodnost") +
theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), axis.title = element_text(size = 10, face = "bold")) +
scale_x_continuous(breaks = seq(0, 100000, 10000), limits = c(0, 100000)) +
scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10))

% Ulozenie grafu do suboru
pdf("mojGG_graf.pdf")
print(porodnostHNP)
dev.off()


% Triedim podla kategorickej premennej -> Region
ggplot(who, aes(x = GNI, y = FertilityRate, color = Region)) +
geom_point()

% Triedim podla numerickej premennej -> LifeExpectancy => gradient
ggplot(who, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) +
geom_point() +
theme(legend.position = "bottom", legend.direction = "horizontal")

% Linearna regresia -> priamka
% Tu je logaritmicka zavislost
ggplot(who, aes(x = FertilityRate, y = Under15)) +
geom_point()

% Ak chcem linearna zavislost:
% -> umocnim Under15
% -> zlogaritmujem FertilityRate

% Zlogaritmujem aj druhu premennu
ggplot(who, aes(x = log(FertilityRate), y = Under15)) +
geom_point()

% Vytvorit model linearnej regresie
% Pouzijem logaritmus FertilityRate
model = lm(formula = Under15 ~ log(FertilityRate), data = who)
summary(model)
% Model je dobry (viem podla p-hodnoty celeho modelu, p-hodnot jednotlivych premennych, miery vysvetlenia)

SSE = sum(model$residuals^2)
SSE

MSE = SSE / length(model$residuals)
MSE

% Graficke zobrazenie linearnej regresie
% Rovnaka ako v modeli
% Siva oblast okolo priamky je interval spolahliovsti (menim pomocou level)
ggplot(who, aes(x = log(FertilityRate), y = Under15)) +
geom_point() +
stat_smooth(method = "lm", level = 0.99)

% Co je IS
% Preco je v strede uzsi ako na kraji


kradeze = read.csv("Cvičenia/8. Cvičenie/kradeze.csv")
str(kradeze)

% Zmena znakovej sady
Sys.setlocale("LC_ALL", "Slovak")

% V ktory den v tyzdni bol najvyssi pocet kradezi
table(kradeze$DenTyzdna)

dniPocty = as.data.frame(table(kradeze$DenTyzdna))
dniPocty

ggplot(dniPocty, aes(x = Var1, y = Freq)) +
geom_point()

% group = 1 znamena, ze chcem iba jednu ciaru
ggplot(dniPocty, aes(x = Var1, y = Freq, group = 1)) +
geom_line()

% Zmena usporiadania hodnot na osi x
% Dni s diakritikou musim zadavat cez level a index
dniPocty$Var1 = factor(dniPocty$Var1, ordered = TRUE, levels = c("pondelok", "utorok", "streda", levels(dniPocty$Var1)[6], "piatok", "sobota", levels(dniPocty$Var1)[1]))

ggplot(dniPocty, aes(x = Var1, y = Freq, group = 1)) +
geom_line()

% Kontingencna tabulka
table(kradeze$DenTyzdna, kradeze$Hodina)

denHodinaPocty = as.data.frame(table(kradeze$DenTyzdna, kradeze$Hodina))
denHodinaPocty

denHodinaPocty$Var1 = factor(denHodinaPocty$Var1, ordered = TRUE, levels = c("pondelok", "utorok", "streda", levels(denHodinaPocty$Var1)[6], "piatok", "sobota", levels(denHodinaPocty$Var1)[1]))

ggplot(denHodinaPocty, aes(x = Var2, y = Freq, color = Var1, group = Var1)) +
geom_line(size = 1)

% Heat mapa
ggplot(denHodinaPocty, aes(x = Var2, y = Var1)) +
geom_tile(aes(fill = Freq)) +
scale_fill_gradient(low = "white", high = "red", name = "teplotna mapa")