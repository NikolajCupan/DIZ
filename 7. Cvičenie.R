% Nacitanie dat
% header = FALSE znaci, ze uz na 1. riadku sa nachadzaju data a nie nadpis
flower = read.csv("Cvičenia/7. Cvičenie/flower.csv", header = FALSE)

str(flower)

% Uprava datasetu
kvetMatica = as.matrix(flower)
str(kvetMatica)

% Jeden dlhy vektor s 2500 prvkami
% Kazdy prvok znaci intenzitu bielej farby
% 0 -> cierna farba
% 1 -> biela farba
% Vektor moze byt aj viacrozmerny
kvetVektor = as.vector(kvetMatica)
kvetVektor

% Velkost matice vzdialenosti je 2500^2
% Pocitam vzdialenost kazdeho prvu ku kazdemu prvku
% Pocet vypoctov je (n*(n-1))/2

% Matica vzdialenosti
% Pouzivame vzdusnu vzdialenost (euklidova vzdialenost)
vzdialenosti = dist(kvetVektor, method = "euclidian")


% Hierarchicke zhlukovanie
% Na zaciatku kazdy pixel je samostatnym zhlukom (v tomto pripade 2500 zhlukov)
% Na konci mam iba 1 zhluk (vsetky pixely su v jednom rovnakom zhluku)

% ward.D1, ward.D2 -> mozno urobit prezentaciu o nich za BB
zhlukyPodlaIntenzity = hclust(vzdialenosti, method = "ward.D2")

% Dendrogram -> graficke zobrazenie zhlukovania
plot(zhlukyPodlaIntenzity)

% Podla dendrogramu pouzijeme 3 zhluky
% Zobrazenie, ktore zhluky to su
rect.hclust(zhlukyPodlaIntenzity, k = 3, border = "red")

% Cislo zobrazuje, do ktoreho kazdy pixel patri
kvetZhluky = cutree(zhlukyPodlaIntenzity, k = 3)
kvetZhluky

% Kolko pixelov patri, do ktoreho zhluku
table(kvetZhluky)

% Priemerna intenzita pixelov v jednotlivych zhlukoch
tapply(kvetVektor, kvetZhluky, mean)


% Zobrazenie obrazku
% Ukazka zobrazenia roznych farieb za BB
dim(kvetZhluky) = c(50, 50)
image(kvetZhluky, axes = FALSE, col = rainbow(3))

% Povodny obrazok
image(kvetMatica, axes = FALSE, col = gray(seq(0, 1, length = 256)))

% Porovnanie oboch obrazkov
par(mfrow = c(1, 2))


% Druhy dataset
zdravy = read.csv("Cvičenia/7. Cvičenie/healthy.csv", header = FALSE)
str(zdravy)

% 1. Pretypovanie na maticu
% 2. Pretypovanie na vektor
% 3. Matica vzdialenosti

zdravyMatica = as.matrix(zdravy)
zdravyVektor = as.vector(zdravyMatica)
zdravyVzdialenosti = dist(zdravyVektor, method = "euclidian")
% Nedokazem vytvorit maticu vzdialenosti, nakolko by bola prilis velka (134 miliard prvkov)
% Pouzijem inu metodu -> k-means zhlukovanie


% k-means zhlukovanie

% 1. Urcim pocet zhlukov
k = 5
% 2. Vyber seedu
% Vstupuje tam nahoda
set.seed(1)

% Volim maximalny pocet iteracii, nakolko by to mohlo trvat prilis dlho, kym by to skonvergovalo
KM = kmeans(zdravyVektor, centers = k, iter.max = 1000)

% clusters -> zobrazuje, do ktoreho zhluku patri dany pixel
% centers -> priemerna intenzita pixelov v jednotlivych zhlukoch
str(KM)

% Vyberiem iba informaciu o zhlukoch
zdravyZhluky = KM$cluster
% Prevediem na maticu
dim(zdravyZhluky) = c(nrow(zdravyMatica), ncol(zdravyMatica))

% Vykreslenie obrazkov
par(mfrow = c(1, 2))
image(zdravyZhluky, axes = FALSE, col = rainbow(5))
image(zdravyMatica, axes = FALSE, col = gray(seq(0, 1, length = 256)))


% Treti dataset
% Toto by uz na 2. teste nemalo byt
tumor = read.csv("Cvičenia/7. Cvičenie/tumor.csv", header = FALSE)
tumorMatica = as.matrix(tumor)
tumorVektor = as.vector(tumorMatica)

% Logiku, ktoru som pouzil na predchadzajucom datasete, chcem pouzit aj na tomto
install.packages("flexclust")
library(flexclust)

KM.kcca = as.kcca(KM, zdravyVektor)

tumorZhluky = predict(KM.kcca, newdata = tumorVektor)

dim(tumorZhluky) = c(nrow(tumorMatica), ncol(tumorMatica))
image(tumorZhluky, axes = FALSE, col = rainbow(5))
image(zdravyZhluky, axex = FALSE, col = rainbow(5))


% Test
flower = read.csv("Cvičenia/7. Cvičenie/flower.csv", header = FALSE)
str(flower)

kvetMatica = as.matrix(flower)
kvetVektor = as.vector(kvetMatica)

vzdialenosti = dist(kvetVektor, method = "euclidian")
zhlukyPodlaIntenzity = hclust(vzdialenosti, method = "ward.D2")

table(kvetZhluky)
tapply(kvetVektor, kvetZhluky, mean)

dim(kvetZhluky) = c(50, 50)
image(kvetZhluky, axes = FALSE, col = rainbow(3))


zdravy = read.csv("Cvičenia/7. Cvičenie/healthy.csv", header = FALSE)
str(zdravy)

zdravyMatica = as.matrix(zdravy)
zdravyVektor = as.vector(zdravyMatica)

KM = kmeans(zdravyVektor, centers = k, iter.max = 1000)
str(KM)

zdravyZhluky = KM$cluster
dim(zdravyZhluky) = c(nrow(zdravyMatica), ncol(zdravyMatica))

par(mfrow = c(1, 2))
image(zdravyZhluky, axes = FALSE, col = rainbow(5))
image(zdravyMatica, axes = FALSE, col = gray(seq(0, 1, length = 256)))