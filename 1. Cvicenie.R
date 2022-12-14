% Toto je komentar
7 * 8
3 + 10

% Vytvorenie vektora
Mena = c("Nikolaj", "Michal", "Fero")
Vek = c(20, 30, 25)

% Vytvorenie tabulky z vektorov
Databaza = data.frame(Mena, Vek)

% Nacitanie suboru
% Absolutna cesta
WHO = read.csv("Cvičenia/1. Cvičenie/WHO.csv")

% Informacie o atributoch v danej tabulke
str(WHO)

% Statisticke informacie o datach/premennych
% 2 rozne sposoby, podla toho, ci to su numericke/kategoricke data
summary(WHO)

% Nezadane (NA) hodnoty mozu byt nahradene niecim inym,
% napriklad priemerom, medianom,...
% Tzv. inputacia

% Vytvorenie podmnoziny
Europa = subset(WHO, Region=="Europe")

% Vytvorenie csv suboru
% Absolutna cesta
write.csv(Europa, "Cvičenia/1. Cvičenie/Europa.csv")

% Vymazanie z pamati
rm(Europa)

% Nepojde, pretoze to je len atribut v data sete
Under15

% Musim zadefinovat, v ktorom data sete sa to nachadza
% V podstate citam atribut, ktory sa nachadza v tejto tabulke
WHO$Under15

% Priemer
mean(WHO$Under15)

% Standardna/smerodajna odchylka
sd(WHO$Under15)

% Summary, ale len o danom atribute
summary(WHO$Under15)

% Najvyssia hodnota premennej Over60
% Vrati index
which.max(WHO$Over60)

% Ak chcem vediet nazov krajiny, ktora ma tuto hodnotu
WHO$Country[86]
WHO$Country[which.max(WHO$Over60)]

% Krajina s najvyssou umrtnostou deti na svete
WHO$Country[which.max(WHO$ChildMortality)]

% Krajina s najvyssou dlzkou dozitia
WHO$Country[which.max(WHO$LifeExpectancy)]
% Kolko to je
WHO$LifeExpectancy[which.max(WHO$LifeExpectancy)]

% Najviac percent ludi nad 60 rokov
WHO$Country[which.max(WHO$Over60)]
% Kolko to je
WHO$Over60[which.max(WHO$Over60)]

% Priemerna umrtnost deti v Afrike
Afrika = subset(WHO, Region == "Africa")
mean(Afrika$ChildMortality)

% Priemerna porodnost v Afrike
mean(Afrika$FertilityRate)

% Dlzka dozitia na Slovensku
WHO$LifeExpectancy[WHO$Country == "Slovakia"]

% Gramotnost na Ukrajine
WHO$LiteracyRate[WHO$Country == "Ukraine"]

% Percento dievcat, ktore nastupia do 1. rocnika v Pakistane
WHO$PrimarySchoolEnrollmentFemale[WHO$Country == "Pakistan"]

% 1. Na ktoru premennu hladi
% 2. Podla coho rozdeli
% 3. Co pocita
tapply(WHO$Over60, WHO$Region, mean)

% 4. Odstranenie NA zaznamov
tapply(WHO$PrimarySchoolEnrollmentFemale, WHO$Region, mean, na.rm = TRUE)

% Porodnost na Kube
WHO$FertilityRate[WHO$Country == "Cuba"]

% HND v Mexiku
WHO$GNI[WHO$Country == "Mexico"]