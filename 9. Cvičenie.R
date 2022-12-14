% Nacitanie dat
tweets = read.csv("Cvičenia/9. Cvičenie/tweets.csv", stringsAsFactors = FALSE)

str(tweets)

% Metadata - dalsie data o tweetoch (kto, kde,...)

table(tweets$handle)


% Kolko tweetov je od Trumpa
trump = subset(tweets, tweets$handle == "realDonaldTrump")
onlyTrump = subset(trump, trump$is_retweet == "False")
str(onlyTrump)

table(tweets$handle, tweets$is_retweet)

% Kolko znakov sa nachadza v retweetnutych tweetoch od Hillary
hillary = subset(tweets, handle == "HillaryClinton" & is_retweet == "True")
sum(nchar(hillary$text))


% Textova analytika
% 0. Nainstalovanie balikov (tm = text mining, SnowballC -> sluzi na orezanie slov na slovny zaklad)
% 1. Korpus
% 2. Preprocessing
% 3. DTM (Document-Term matrix)
% 4. Baseline text analytics
% 5. Vizualizacia (ggplot)

install.packages("tm")
library(tm)

install.packages("SnowballC")
library(SnowballC)


% 1. Korpus
% Korpus ma typ list (nie dataframe)
TrumpKorpus = Corpus(VectorSource(onlyTrump$text))


% 2. Preprocessing
% Predspracovanie textu => rychlejsie a efektivnejsie spracovanie textu
% Text musime upravit do vhodnej podoby
% Vsetky pismena transformujeme na male
% Orezanie na slovny zaklad -> Stemming
% Odstranenie neplnovyznamovych slov (pomocou zoznamu neplnovyznamovych slov) -> Stopwords
% Odstranenie interpunkcie, diakritiky, niektorych cisel

% Taketo spracovanie vsak nikdy nie je uplne presne

% Zobrazenie textu
% "1" znaci cislo tweetu
writeLines(as.character(TrumpKorpus[[1]]))
inspect(TrumpKorpus[1:3093])

% Vsetky pismena na male
TrumpKorpus = tm_map(TrumpKorpus, tolower)

% Odstranenie interpunkcie
TrumpKorpus = tm_map(TrumpKorpus, removePunctuation)

% Odstranenie cisel
TrumpKorpus = tm_map(TrumpKorpus, removeNumbers)

% Odstranenie neplnovyznamovych slov
TrumpKorpus = tm_map(TrumpKorpus, removeWords, stopwords("english"))

% Odstranenie prebytocnych medzier (vznikli kvoli predchadzajucim operaciam)
TrumpKorpus = tm_map(TrumpKorpus, stripWhitespace)

% Orezanie na slovny zaklad
TrumpKorpus = tm_map(TrumpKorpus, stemDocument)


% 3. DTM
% Riadky matice = dokumenty (v nasom pripade to je pocet tweetov)
% Pocet riadkov DTM = pocet tweetov (t. j. 3093)
% Term (stlpce) su jednotlive slova v tweetoch
% Slova, ktore sa nachadzaju vo vsetkych tweetoch dohromady -> staci aby slovo bolo spomenute jedenkrat
%	-> t. j. unikatne slova
% Stlpec -> frekvencia slova v danom tweete
%	   -> 0 = nebolo spomenute, 1 = bolo spomenute jedenkrat, n = bolo spomenute n-krat
dtm = DocumentTermMatrix(TrumpKorpus)

% Pocet dokumentov, pocet unikatnych slov
% V matici je velmi vela prvkov nulovych
dtm


% 4. Baseline text analytics
% Odstranim nevyznamne slova, t. j. tie, ktore boli pouzite malokrat => nie su vyznamne
% Zvolim hranicu -> mensia hranica => vyhodi viac slov
dtm = removeSparseTerms(dtm, 0.99)
% 3093 * (1 - 0.99) = 3093 * 0.01 = 30.55
%	-> vyhodi slova, ktore sa vyskytli menej ako 31-krat
%	-> tie co sa vyskytli 31-krat alebo viackrat ponecha
%	-> 2 vyskyty = v 2 roznych tweetoch
%	-> 1 vyskyt = 2-krat v jednom tweete

% Najviac pouzivane slova
frekvencie = colSums(as.matrix(dtm))
frekvencie

% Zoradenie slov
sort(frekvencie, decreasing = TRUE)

slova = data.frame(slovo = names(frekvencie), pocetnost = frekvencie)
slova
str(slova)

noveSlova = slova
noveSlova = subset(noveSlova, noveSlova$pocetnost > 100)
noveSlova
str(noveSlova)


% 5. Vizualizacia
install.packages("ggplot2")
library(ggplot2)

ggplot(data = noveSlova, aes(x = reorder(slovo, pocetnost), y = pocetnost)) +
geom_bar(stat = "identity", fill = rainbow(n = 39)) +
coord_flip()