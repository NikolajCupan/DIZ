% Nacitanie suboru
enron = read.csv("Cvičenia/10. Cvičenie/energy_bids.csv", stringsAsFactors = FALSE)
str(enron)

table(enron$responsive)



% Textova analytika
% 1. Vytvorenie korpusu
% 2. Preprocessing
% 3. DTM matica
% 4. Baseline model
% 5. Vizualizacia cez ggplot


% 1. Vytvorenie korpusu
library("tm")
Korpus = Corpus(VectorSource(enron$email))


% 2. Preprocesing
writeLines(as.character(Korpus[[1]]))

% Male pismena 
Korpus = tm_map(Korpus, tolower)

% Odstranenie interpunkcie
Korpus = tm_map(Korpus,removePunctuation)

% Odstranenie cisel 
Korpus = tm_map(Korpus,removeNumbers)

% Odstranenie neplnovyznamovych slov
Korpus = tm_map(Korpus,removeWords,stopwords("english"))

% Odstranenie viecerych medzier
Korpus = tm_map(Korpus,stripWhitespace)

% Orezanie slov na slovny zaklad, bag of words -> vsetky slova v zakladnom tvare
install.packages("SnowballC")
library("SnowballC")
Korpus = tm_map(Korpus,stemDocument)


% 3. DTM matica (Document term matrix)
dtm = DocumentTermMatrix(Korpus)
dtm

dtm = removeSparseTerms(dtm, 0.97)
% Mensie cislo -> odstranenie viac slov -> ostane menej slov


% 4. Baseline model
frekvencia = colSums(as.matrix(dtm))

% Utriedenie
frekvencia = sort(frekvencia, decreasing = TRUE)
frekvencia


% 5. Vizualizacia
slova = as.data.frame(as.matrix(dtm))
str(slova)

slova$responsive = enron$responsive
str(slova)


% Spracovanie
library(caTools)
set.seed(1)
split = sample.split(slova$responsive, SplitRatio = 0.7)

train = subset(slova, split == TRUE)
test = subset(slova, split == FALSE)

str(train)
nrow(train)
nrow(test)


% Strom  
library(rpart)
library(rpart.plot)
Strom = rpart(responsive ~., data = train, method = "class")


% Predict train (spec, senz, total)
predictTrain = predict(Strom, type = "class" )
predictTrain
table(train$responsive, predictTrain)

% Celkova presnost
(476 + 71) / (476 + 25 + 26 + 71)

% Specificita
476 / (476 + 25)

% Sensitivita
72 / (26 + 72)


% Predict test (spec, senz, total)
predictTest = predict(Strom, type = "class", newdata = test )
predictTest
table(test$responsive, predictTest)

% Celkova presnost
(194 + 27) / (194 + 21 + 15 + 27)

% Specificita
194 / (194 + 21)

% Sensitivita
27 / (15 + 27)