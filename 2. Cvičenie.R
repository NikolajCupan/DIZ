% Nacitanie suboru
WHO = read.csv("Cvičenia/2. Cvičenie/WHO.csv")

% Prva hodnota: x
% Druha hodnota: y
% y je zavisla premenna
plot(WHO$GNI, WHO$FertilityRate)

plot(WHO$FertilityRate, WHO$GNI)

% GNI na Slovensku
WHO$GNI[WHO$Country == "Slovakia"]

% Iba Afrika
Afrika = subset(WHO, Region=="Africa")

% Priemerna porodnost v Afrike
mean(Afrika$FertilityRate)

% Krajina, v ktorej je najkratsia dlzka dozitia a kolko to je
WHO$Country[which.min(WHO$LifeExpectancy)]
WHO$LifeExpectancy[which.min(WHO$LifeExpectancy)]

% Priemerna porodnost v Afganistane
WHO$FertilityRate[WHO$Country == "Afghanistan"]

% Najnizsia gramotnost, kde a kolko to je
WHO$Country[which.min(WHO$LiteracyRate)]
WHO$LiteracyRate[which.min(WHO$LiteracyRate)]

% Gramotnost v Nigerii
WHO$LiteracyRate[WHO$Country == "Nigeria"]

% Gramotnost v Afrike
mean(Afrika$LiteracyRate, na.rm = TRUE)

% Na ktorom svetadieli je najvyssia umrtnost deti do 5 rokov a kolko to je
tapply(WHO$ChildMortality, WHO$Region, mean, na.rm = TRUE)

% Na ktorom svetadieli je najvyssia minimalna gramotnost
% Najnizsia gramotnost a z toho najvyssia
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm = TRUE)

% Europa
Europa = subset(WHO, WHO$Region == "Europe")

% Europska krajina s najnizsou gramotnostou
Europa$Country[which.min(Europa$LiteracyRate)]
Europa$LiteracyRate[which.min(Europa$LiteracyRate)]

% Ktora krajina v Europe ma najvyssiu porodnost a kolko to je
Europa$Country[which.max(Europa$FertilityRate)]
Europa$FertilityRate[which.max(Europa$FertilityRate)]

% V ktorej africkej krajine sa ludia dozivaju najmenej a kolko to je
Afrika$Country[which.min(Afrika$LifeExpectancy)]
Afrika$LifeExpectancy[which.min(Afrika$LifeExpectancy)]

% Guinea
WHO$LifeExpectancy[WHO$Country == "Guinea"]

% Liberia
WHO$LifeExpectancy[WHO$Country == "Liberia"]

% Krajiny, ktore sa vymykaju tvrdeniu, ze v krajinach s vysokym GNI
% je nizka porodnost
Outliers = subset(WHO, WHO$GNI > 10000 & WHO$FertilityRate > 2.5)

% Kolko takych krajin je
str(Outliers)

% Co su to za krajiny
Outliers$Country

% Vektor s atributmi, ktore chcem vidiet
Outliers[c("Country", "GNI", "FertilityRate")]

% GNI a porodnost na Slovensku
WHO$GNI[WHO$Country == "Slovakia"]
WHO$FertilityRate[WHO$Country == "Slovakia"]

% Histogram
hist(WHO$CellularSubscribers)

% Kolko percent ludi na Slovensku ma mobilny telefon
WHO$CellularSubscribers[WHO$Country == "Slovakia"]

% Krabicovy graf
boxplot(WHO$LifeExpectancy)

% Rozdelene podla regionov
boxplot(WHO$LifeExpectancy ~ WHO$Region)

% Pomenovanie jednotlivych osi a grafu
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "krajina", ylab = "dozitie", main = "graf")

% Najmensia dlzka dozitia v Europe
Europa$Country[which.min(Europa$LifeExpectancy)]

% Nacitanie inych dat
USDA = read.csv("Cvičenia/2. Cvičenie/USDA.csv")

% Ktora potravina ma najvyssie mnozstvo sodika a kolko to je
USDA$Sodium[which.max(USDA$Sodium)]
USDA$Description[which.max(USDA$Sodium)]

% Kolko sodika ma kaviar
USDA$Sodium[USDA$Description == "CAVIAR"]

% Priemerne mnozstvo sodika vo vsetkych potravinach
mean(USDA$Sodium, na.rm = TRUE)

% Standardna odchylka sodika vo vsetkych potravinach
sd(USDA$Sodium, na.rm = TRUE)

% Graf znazornujuci zavislost medzi bielkovinami a tukmi
% Nepriama zavislost
plot(USDA$Protein, USDA$TotalFat)

% Mnozstvo cukru
boxplot(USDA$Sugar)

% Nova premenna
% as.numeric nahradi T/F s 1/0
NadpriemernyObsahBielkovin = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
NadpriemernyObsahSodika = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
NadpriemernyObsahTukov = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))

NadBielkovinoveP = subset(USDA, USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
NadSodikoveP = subset(USDA, USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
NadTukoveP = subset(USDA, USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))

% Priemerne mnozstvo zeleza v nadpriemerne bielkovinovych potravinach
mean(NadBielkovinoveP$Iron, na.rm = TRUE)

% Priemerne mnozstvo vitaminu C v nadpriemerne sodikovych potravinach
mean(NadSodikoveP$VitaminC, na.rm = TRUE)

% Kolko potravin je nadpriemerne sodikovych a zaroven nadpriemerne tucnych
% Za pomoci prikazu table
table(NadpriemernyObsahSodika, NadpriemernyObsahTukov)