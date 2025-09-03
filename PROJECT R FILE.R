#############
# PROIECT ICE
#############

# Import fisierul csv "International_Education_Costs", care contine date despre costurile de educatie pentru studentii internationali

date_proiect <- read.csv("D:/Downloads (D)/International_Education_Costs.csv")
View(date_proiect)

boxplot(date_proiect$Duration_Years) # fara outlieri
boxplot(date_proiect$Tuition_USD) # fara
boxplot(date_proiect$Living_Cost_Index) # multi outlieri
boxplot(date_proiect$Rent_USD) # un outlier
boxplot(date_proiect$Visa_Fee_USD) # un outlier

# Gestionarea outlierilor
# Selectez doar coloanele numerice
numeric_vars <- sapply(date_proiect, is.numeric)
date_numerice <- date_proiect[, numeric_vars]
date_fara_outlieri <- date_proiect
# Pentru fiecare coloana numerica, aplic regula IQR si setez outlierii pe NA
for (var in names(date_numerice)) {
  Q1 <- quantile(date_fara_outlieri[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(date_fara_outlieri[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  date_fara_outlieri[[var]][date_fara_outlieri[[var]] < (Q1 - 1.5 * IQR) |
                              date_fara_outlieri[[var]] > (Q3 + 1.5 * IQR)] <- NA
}
# Elimin toate randurile care au cel puțin un NA (adica cel putin un outlier)
date_fara_outlieri <- na.omit(date_fara_outlieri)

dim(date_proiect)           # Dimensiunea initiala
dim(date_fara_outlieri)     # Dupa eliminarea outlierilor

date_proiect <- date_fara_outlieri
dim(date_proiect)  

# Statistici descriptive
install.packages("psych")
library(psych)

summary(date_proiect[-c(1,2,3,4,5)])
describe(date_proiect[-c(1,2,3,4,5)])

# Matricea de corelatie
matrice_corelatie <- cor(date_proiect[-c(1,2,3,4,5)])

# Frevente
table(date_proiect$Country)
table(date_proiect$Level)
table(date_proiect$Country, date_proiect$Level)

# Reprezentari grafice
par(mfrow=c(1,3))
hist(date_proiect$Tuition_USD, main = "Distribuția taxelor de școlarizare", xlab = "Tuition (USD)", col = "skyblue")
hist(date_proiect$Rent_USD, main = "Distribuția chiriei lunare", xlab = "Rent (USD)", col = "lightgreen")
hist(date_proiect$Living_Cost_Index, main = "Distribuția costului de trai", xlab = "Living Cost Index", col = "lightpink")

barplot(sort(table(date_proiect$Country), decreasing = TRUE), las=2, col="orange", main="Număr de programe pe țară")
barplot(sort(table(date_proiect$Level), decreasing = TRUE), col="steelblue", main="Distribuția programelor pe niveluri")

####################
# Clusterizare Fuzzy
####################

# Elimin variabilele categoriale/de tip string
date_proiect1 <- date_proiect[,-c(1,2,3,4,5)]
View(date_proiect1)

install.packages("e1071") 
library(e1071)

# Voi clusteriza/grupa datele in 3 clustere (3 categorii privind costul educatiei: accesibil, mediu, scump)

set.seed(123)
rez <- cmeans(date_proiect1, 3, 100, m=2, method = "cmeans")
rez

# Centroizii clusterelor (mediile clusterelor):

# Primul cluster prezinta o durata medie a studiilor de 2.73 ani (aproximativ 3 ani), in timp ce clusterul 2 are o durata medie de 
# 3.13 ani, iar clusterul 3 o durata de 3.13 ani => duratele medii sunt de aproximativ 3 ani pentru toate clusterele identificate

# In ceea ce priveste taxele de scolarizare, clusterul 1 prezinta cea mai mica valoare medie, de 3667.928 USD, urmata de clusterul 2 
# cu valoarea de 29077.257 USD, iar, in cele din urma, cea mai mare valoare medie o prezinta clusterul 3, fiind de 46979.804 USD

# Indicele cheltuielilor de trai (mancare, transport, utilitati) pentru clusterul 1 este de 59.54, urmat de clusterul 2 cu 67.14 si 
# clusterul 3 cu valoarea medie de 75.41

# Chiria medie in cazul clusterului 1 este de 673.318 USD, pentru clusterul 2 este de 1191.627 USD, iar pentru clusterul 3 este de 1776.425 USD

# Pentru clusterul 1 costul mediu al vizei (in USD) este de 130.476 USD, pentru clusterul 2 este de 299.422 USD, iar pentru clusterul 3 este de 224.7908 USD

# Analizand aceste rezultate, putem ajunge la urmatoarele concluzii:
# Clusterul 1 - Educatie accesibila/cu costuri reduse - include tari cu cost total redus al educatiei internationale
# Clusterul 2 - Educatie cu costuri medii - reprezinta un compromis intre cost si calitate, cu taxe si costuri moderate
# Clusterul 3 - Educatie foarte scumpa/cu costuri ridicate - include tari in care costurile sunt foarte ridicate, reflectand un sistem educational "premium"

# Interpretare Memberships (gradele de apartenenta la fiecare cluster):
# Pentru prima observatie:
# Gradul de apartenenta la primul cluster este de 0.023, pentru al doilea cluster este de 0.090, iar pentru al treilea cluster este de 0.88
# In concluzie, prima observatie (Programul de Master in Computer Science la Harvard, in Cambridge, USA) apartine celui de-al treilea cluster
# Deci acest program educational face parte din categoria cu costuri ridicate, fiind un sistem educational "premium"

# Reprezentarea grafica a observatiilor intr-un sistem de axe xOy, unde Tuition_USD va fi pe Ox si Living_Cost_Index pe Oy

plot(date_proiect1$Tuition_USD, date_proiect1$Living_Cost_Index, col = rez$cluster)
points(x = rez$centers[,"Tuition_USD"], 
       y = rez$centers[,"Living_Cost_Index"],
       col =1:3, pch=9, cex=2)
text(x=date_proiect$Tuition_USD, y =date_proiect1$Living_Cost_Index,
     col=rez$cluster, cex = 0.6, pos = 4, offset = 0.5)

# Centroizii au fost reprezentati prin romburi
# Observatiile reprezentate cu negru au taxe de scolarizare scazute si costurile de trai moderate in majoritatea cazurilor, 
# existand si anumite observatii cu costuri de tari foarte ridicate (cele mai ridicate din intreg graficul) => clusterul 1
# Observatiile reprezentate cu rosu au taxe de scolarizare moderate si costuri de trai medii => clusterul 2
# Observatiile reprezentate cu verde au cele mai mari taxe de scolarizare si costuri de trai mai mari decat in cazul celorlalte 
# 2 clustere => clusterul 3

# Reprezentarea grafica a variabilelor in functie de cluster
date_proiect$Cluster <- rez$cluster

boxplot(Tuition_USD ~ Cluster, data = date_proiect, main = "Taxe în funcție de cluster", col=2:4)
boxplot(Living_Cost_Index ~ Cluster, data = date_proiect, main = "Cost de trai în funcție de cluster", col=2:4)

# Ordonarea crescatoare a observatiilor dupa cluster
ordine <- order(rez$cluster)
ordine

# Afisarea denumirii fiecarei universitati si clusterul din care face parte
df_clustere <- data.frame(date_proiect$University[ordine], rez$cluster[ordine])
View(df_clustere)

# Afisarea gradelor de apartenenta pentru fiecare observatie
df_membership <- data.frame(date_proiect$University, rez$membership)
View(df_membership)

# Afisarea gradelor de apartenenta la cele 3 clustere pentru primele 3 observatii
rez$membership[1:3,]

# Observam ca toate cele 3 observatii apartin clusterului 3 - educatie foarte scumpa, premium

# Verificarea clusterizarii
library(cluster)
sil <- silhouette(rez$cluster, dist(date_proiect1))
plot(sil, border = NA)

####################
# Regresie logistica
####################

# 1. Regresia logistica binomiala 

# Pentru a clasifica tarile in high cost/low cost (dpdv al educatiei), voi crea o variabila binara (variabila tinta)
# O voi denumi High_Cost si va depinde de indicele costului de trai (Living_Cost_Index)
# Daca acesta este mai mare decat mediana, atunci high cost (1), altfel low cost (0)

# Voi folosi mediana ca prag
mediana <-  median(date_proiect$Living_Cost_Index, na.rm = TRUE)
mediana

# Creez variabila binara
date_proiect$High_Cost <- ifelse(date_proiect$Living_Cost_Index > mediana, 1, 0)

table(date_proiect$High_Cost)
# 399 de tari sunt low cost, 395 de tari sunt high cost

install.packages("ggplot2")
library(ggplot2)

# Reprezentarea grafica a datelor
grafic <- ggplot(data=date_proiect, aes(x=date_proiect$Tuition_USD, y=date_proiect$Rent_USD, col=High_Cost))
grafic <- grafic+geom_point(aes(size=5))
grafic
# Variabila dependenta este High_Cost, ce poate avea valorile 0 (low cost) si 1 (high cost)
# Variabilele independente sunt: Tuition_USD (taxa de scolarizare) si Rent_USD (costul chiriei)
# Observam, pe baza graficului, o evolutie a variabilei High_Cost de la 0 la 1, putem observa ca exista un raport echilibrat intre
# numarul de programe educationale scumpe si accesibile

# Transformarea variabilei dependente in variabila de tip factor
date_proiect$High_Cost <- factor(date_proiect$High_Cost)

install.packages("caTools")
library(caTools)
set.seed(88)

# Impartirea setului de date in antrenare si testare
# 75% antrenare
# 25% testare

impartire <- sample.split(date_proiect$High_Cost, SplitRatio = 0.75)
impartire

# Definirea setului de antrenare
set_antrenare <- subset(date_proiect, impartire == TRUE)
set_antrenare

# Definirea setului de testare
set_testare <- subset(date_proiect, impartire == FALSE)
set_testare

# Refac graficul anterior
grafic <- ggplot(data=date_proiect, aes(x=date_proiect$Tuition_USD, y=date_proiect$Rent_USD, col=High_Cost))
grafic <- grafic+geom_point(aes(size=5))
grafic

# Aplicarea regresiei logistice binomiale, utilizand glm
# Voi alege ca variabile explicative taxa de scolarizare si costul chiriei
model_regresie <- glm(High_Cost~Tuition_USD+Rent_USD, data=set_antrenare, family=binomial())
summary(model_regresie)

# Interpretare:

# Ecuatia regresiei logistice binomiale:
# logit(p) = -6.2355035 - 0.0001371 × Tuition_USD + 0.0092349 × Rent_USD
# unde p reprezinta probabilitatea ca tara sa fie scumpa din punct de vedere al educatiei (High_Cost = 1)
# 1-p = probabilitatea ca tara sa fie ieftina din punct de vedere al educatiei (High_Cost = 0)


# Varianta reziduala este de 375.12, fiind mai mica decat devianta nula (824.83), asadar modelul actual este mai bun decat modelul nul

# Toate variabilele sunt semnificative dpdv statistic 

# Impactul variabilelor analizate asupra sanselor ca o tara sa fie scumpa dpdv al programelor educationale pentru studenti internationali
exp(coef(model_regresie))

# Interpretare:
# Sansele ca o tara sa fie scumpa scad cu aproximativ 0.0137% ((0.999862874-1)*100) relativ la sansele ca tara 
# sa fie ieftina, daca taxa de scolarizare creste cu o unitate (1 USD)
# Sansele ca o tara sa fie scumpa cresc cu aproximativ 0.927% ((1.009277665-1)*100) relativ la sansele ca tara 
# sa fie ieftina, daca chiria creste cu o unitate (1 USD)

# Factorizarea categoriilor
contrasts(date_proiect$High_Cost)
# Se atribuie valoarea 0 tarilor ieftine si 1 tarilor scumpe

# Previzionarea probabilitatilor ca o tara sa fie scumpa/ieftina, utilizand setul de testare
probabilitati <- predict(model_regresie, set_testare, type='response')
probabilitati
# Probabilitatea ca prima tara sa fie scumpa este 0.98, pe cand probabilitatea ca a doua tara sa fie scumpa este 0.99

# Voi defini un vector de valori pentru "0" (ieftina) corespunzator numarului de observatii din setul de antrenare
# Elementul 0 este convertit la 1 atunci cand probabilitatea previzionata ca o tara sa fie scumpa este mai mare de 50%
predictie <- rep("0", nrow(set_antrenare))
predictie[probabilitati>.5] = "1"

# Matricea de confuzie pt setul de antrenare
table(predictie, set_antrenare$High_Cost)

# Interpretare:
# Setul de antrenare contine 138+161=299 tari ieftine si 138+158=296 tari scumpe
# Din cele 296 tari scumpe, 158 au fost clasificate in mod corect ca fiind scumpe, iar 138 au fost clasificate eronat ca fiind scumpe
# Din cele 299 tari ieftine, 138 au fost clasificate corect, iar 161 au fost clasificate in mod eronat ca fiind ieftine
# Pe diagonala principala se regasesc observatiile previzionate corect, si anume 138+158=296 observatii
# Acuratetea clasificarii se determina ca raportul dintre suma elementelor de pe diagonala principala si totalul elementelor:
(138+158)/(138+138+161+158)
# Acuratetea modelului este de 49.7% (adica 49.7% din elementele din setul de antrenare au fost clasificate in mod corect)

# Voi introduce inca 2 tari si voi previziona daca acestea sunt scumpe sau ieftine
predictie_noua <- predict(model_regresie, newdata=data.frame(Tuition_USD=c(10000,1000), Rent_USD=c(1000,650)), type='response')
predictie_noua

# Evaluez daca cele doua tari sunt scumpe, comparand probabilitatile previzionate cu 0.5
predictie_noua[1] <= 0.5
# Prima tara este scumpa
predictie_noua[2] <=0.5
# A doua tara este ieftina

# Predictia pe setul de testare
predictie1 <- rep("0", nrow(set_testare))
predictie1[probabilitati>.5] = "1"

# Matricea de confuzie pt setul de testare
table(predictie1, set_testare$High_Cost)
# In setul de testare avem 8+91=99 tari scumpe si 84+16=100 tari ieftine
# Din cele 99 tari scumpe, 91 au fost etichetate corect, iar 8 incorect
# Din cele 100 tari ieftine, 84 au fost etichetate corect, iar 16 incorect
mean(predictie1==set_testare$High_Cost)
# Acuratetea este de 87.9% pentru setul de testare (aproximativ 87% din observatii au fost etichetate corect)

# Indicatorii matricei de confuzie:
#Specificitate: TN/(TN+TP)
91/(91+84) # Modelul clasificator recunoaste in proportie de 52% toate cazurile negative observate
#Senzitivitate: TP/(TP+FN)
84/(84+8) # Modelul clasificator recunoaste in proportie de 91% toate cazurile pozitive observate

# Curba ROC
install.packages("ROCR")
library(ROCR)

p <- predict(model_regresie, newdata = set_testare, type='response')
pr <- prediction(p, set_testare$High_Cost)
prf <- performance(pr, measure = "tpr", x.measure="fpr")
plot(prf)
# Graficul se apropie de coltul din stanga sus, ceea ce este ideal

auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc
# Valoarea de 0.94 a AUC indica o clasificare excelenta, existand 94% sanse ca modelul sa poata distinge clasa pozitiva de cea negativa

#################################
# Regresie logistica multinomiala
#################################

# Variabila tinta categoriala (multinomiala) utilizata va fi Level, cu 3 valori posibile: "Master", "Bachelor", "PhD"
unique(date_proiect$Level)

  # Transform variabila Level in variabila factor
date_proiect$LevelF <- factor(date_proiect$Level)

# Voi seta ca nivel de referinta valoarea "Bachelor"
date_proiect$out <- relevel(date_proiect$LevelF, ref="Bachelor")

# Aplicam modelul de regresie logistica multinomiala
install.packages("nnet")
library(nnet)

model_regresie_multi <- multinom(out~Tuition_USD+Duration_Years, data=date_proiect, trace=FALSE)
summary(model_regresie_multi)

# Devianta reziduala este eroarea ramasa in model, iar valoarea acesteia trebuie sa fie cat mai mica
# In acest caz, este de 416.4454

# Conform tabelului de mai sus, se pot determina ecuatiile urmatoare pentru determinarea probabilitatilor:
# ln[P(Master)/P(Bachelor)] = 44.44995 - 0.0000337*Tuition_USD -15.562459*Duration_Years (1)
# Interpretare: Logaritmul probabilitatii ca un program sa fie de Master raportata la probabilitatea ca un un program sa fie de Bachelor se numeste "log odds"
# Coeficientul negativ -0.0000337 indica faptul ca variabila "Tuition_USD" are un impact negativ asupra raportului
# O crestere cu o unitate a taxei de scolarizare conduce la scaderea logaritmului log odds cu 0.0000337
# De asemenea, coeficientul negativ de -15.562 arata ca variabila "Duration_Years" are un impact negativ, o crestere cu o unitate a duratei programului educational
# determinand o scadere a logaritmului cu 15.562

# ln[P(PhD)/P(Bachelor)] = -16.18454 - 0.0000401*Tuition_USD + 4.220675*Duration_Years (2)
# Interpretare: 
# Coeficientul negativ -0.0000401 indica un impact negativ al taxei de scolarizare asupra raportului, o crestere cu o unitate a taxei de scolarizare 
# determinand o scadere a log cu 0.0000401
# Coeficientul pozitiv 4.22 indica un impact pozitiv asupra raportului al duratei, o crestere cu o unitate a duratei determinand o crestere a log cu 4.22

exp(coef(model_regresie_multi))
# Interpretare:

# Sansele ca un program sa fie de Master sunt cu 0.00338% mai mici decat sansele ca programul sa fie de Bachelor, daca taxa de scolarizare creste cu o unitate (1 USD)
# Sansele ca un program sa fie de Master sunt cu 99.99% mai mici decat sansele ca programul sa fie de Bachelor, daca durata prorgamului creste cu o unitate (1 an)

# Sansele ca un program sa fie de PhD sunt cu 0.00402% mai mici decat sansele ca programul sa fie de Bachelor, daca taxa de scolarizare creste cu o unitate (1 USD)
# Sansele ca un program sa fie de PhD sunt cu 6707.94% mai mari decat sansele ca programul sa fie de Bachelor, daca durata prorgamului creste cu o unitate (1 an)
# adica pt fiecare an in plus in durata programului, sansele ca programul sa fie PhD fata de Bachelor cresc de 68 ori

# Suma probabilitatilor este egala cu 1:
# P(Bachelor)+P(Master)+P(PhD)=1

# Determinarea probabilitatilor folosind predict
predict(model_regresie_multi, date_proiect)

predict(model_regresie_multi, date_proiect, type="prob")
# Probabilitatea ca primul program sa fie de Bachelor este de 1.06*10^-5, sa fie de Master 9.99*10^-1 si sa fie PhD 4.97*10^-10 => programule este de Master

# Previzionarea valorilor pentru observatiile 10, 200 si 650
predict(model_regresie_multi, date_proiect[c(10,200,650),], type = "prob")
# Primul este program de Master, al doilea si al treilea sunt programe de Bachelor

# Compararea predictiilor modelului cu date reale pentru primele 50 de observatii
matrice_confuzie <- table(date_proiect$Level[1:50], predict(model_regresie_multi)[1:50])
matrice_confuzie

# Interpretare:
# Din 22 programe de Bachelor, 18 au fost etichetate corect, iar 4 au fost etichetate in mod eronat ca fiind PhD
# Din 17 programe de Master, toate au fost clasificate corect
# Din 11 programe de PhD, 7 au fost identificate in mod corect, iar 4 in mod eronat fiind clasificate ca Bachelor
# Observam ca a existat o confuzie intre Bachelor si PhD, iar programele de Master au fost toate identificate corect

mean(date_proiect$Level[1:50] == predict(model_regresie_multi)[1:50])
# Acuratetea modelului este de 84%, ceea ce indica o clasificare excelenta a datelor

##################
#Arbori de decizie
##################

install.packages("ISLR")
install.packages("rpart")
install.packages("pROC")
library(ISLR)
library(rpart)
library(pROC)

# Crearea variabilei tinta binare
range(date_proiect$Living_Cost_Index) # valori intre 32.5 si 95.2
hist(date_proiect$Living_Cost_Index)

# Transform variabila Living_Cost_Index in variabila binara "High_Living_Cost" cu valorile "Yes" daca > 70 si "No" in caz contrar
date_proiect$High_Living_Cost <- ifelse(date_proiect$Living_Cost_Index > 65,'Yes','No')

# Voi pastra doar variabilele dorite si variabila tinta
date_model <- date_proiect[, c("Level", "Duration_Years", "Tuition_USD", "Rent_USD", 
                               "Visa_Fee_USD", "High_Living_Cost")]

# Transform variabila in variabila de tip factor
date_model$High_Living_Cost <- as.factor(date_model$High_Living_Cost)

# Extrag doua esantioane egale, unul pt antrenare si unul pt testare
set.seed(123)
antrenare <- sample(1:nrow(date_model), nrow(date_model)/2)
set_antrenare <- date_model[antrenare,]
set_antrenare
set_testare <- date_model[-antrenare,]
set_testare

# Definirea si vizualizarea arborelui de clasificare
arbore <- rpart(set_antrenare$High_Living_Cost~., data=set_antrenare, method="class")
# Relatia de dependenta liniara este intre eticheta High Living Cost cu valorile yes(cost de trai ridicat) sau no (cost de trai scazut) si restul atributelor setului de date

# Reprezentarea grafica a arborelui
plot(arbore)
text(arbore, pretty=0)

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(arbore, extra=106)
table(set_antrenare$High_Living_Cost)
# In nodul 1, din 397 de observatii, 229 sunt etichetate ca fiind clasa "Yes" (cost de trai ridicat), 
# iar 168 sunt clasificate ca fiind clasa "No" (cost de trai scazut)
# Vectorul de probabilitate este (0.42, 0.58), indicand faptul ca cele 229 de observatii sunt in categoria "yes"
# cu o probabilitate de 58%
# Clasa dominanta in nodul 1 este "Yes" (costuri de trai ridicate), cu o probabilitate de 58%

# Predictia observatiilor din setul de testare
predictie <- predict(arbore, set_testare, type="class")
confuzie <- table(set_testare$High_Living_Cost, predictie)
confuzie
# Interpretare matrice de confuzie:
# Din 180 observatii etichetate in clasa "No" (cost scazut), 159 au fost clasificate corect, iar 21 gresit
# Din 217 observatii ectihetate in clasa "Yes" (cost ridicat), 194 au fost clasificate corect, iar 23 gresit

#Eroarea de clasificare
mean(predictie!=set_testare$High_Living_Cost)
# 11.08% din observatii au fost eronat clasificate

predictie1 <- predict(arbore, set_testare, type="prob")
predictie1

# Reprezentarea curbei ROC
curbaroc <- roc(set_testare$High_Living_Cost, predictie1[,"Yes"])
plot(curbaroc)

auc(curbaroc)
# Clasificatorul distinge in proportie de 95.51% costurile de trai ridicate de cele scazute

# Determinarea parametrului de complexitate optim (cp)
plotcp(arbore)
mincp <- arbore$cptable[which.min(arbore$cptable[,"xerror"]), "CP"]
mincp
printcp(arbore)
# Eroarea relativa de validare incrucisata este minima (xerror = 0.25) pt o valoare a parametrului de 
# complexitate de 0.01, iar arborele curatat are aproximativ 9 noduri terminale (nsplit=8)

# Construirea arborelui curatat
arbore_curat <- prune(arbore, cp=arbore$cptable[which.min(arbore$cptable[,'xerror']),"CP"])
rpart.plot(arbore_curat, extra=106) # arborele curatat are 9 noduri terminale

printcp(arbore_curat) # se confirma ca cp minim este 0.013158, cu 9 noduri terminale

# Predictia pe arborele curatat
predictie2 <- predict(arbore_curat, set_testare, type="class")
predictie2

# Matricea de confuzie
confuzie <- table(set_testare$High_Living_Cost, predictie2)
confuzie
mean(predictie2!=set_testare$High_Living_Cost)
# 11.08% observatii au fost eronat clasificate (eroare a scazut de la 18.7%)

predictie3 <- predict(arbore_curat, set_testare, type="prob")
predictie3

# Curba ROC
curbaroc1 <- roc(set_testare$High_Living_Cost, predictie3[,"Yes"])
plot(curbaroc1)

auc(curbaroc1)  # Acuratete de 95.51%

####################
# Arbori de regresie
####################

install.packages("tree")
install.packages("ISLR")
library(tree)
library(ISLR)

# Scopul este de a estima costul chiriei

# Histograma chiriilor
hist(date_proiect$Rent_USD)

# Pentru avea o distributie normala a chiriilor, vom logaritma variabila
date_proiect$Rent_USD <- log(date_proiect$Rent_USD)

# Refacem histograma
hist(date_proiect$Rent_USD)

install.packages("caret")
library(caret)

# Impartirea setului de date in antrenare si testare (50% antrenare, 50% testare)
impartire <- createDataPartition(y=date_proiect$Rent_USD, p=0.5, list=FALSE)
impartire

# Setul de antrenare
antrenare <- date_proiect[impartire,]
antrenare

# Setul de testare
testare <- date_proiect[-impartire,]
testare

# Definirea arborelui de decizie
arbore <- tree(Rent_USD~., antrenare)
plot(arbore)
text(arbore, pretty = 0)
# Fiecare nod terminal al arborelui prezinta chiria logaritmata pentru observatiile din nodul respectiv

# Curatarea arborelui

cv.trees <- cv.tree(arbore)
cv.trees$size
cv.trees$dev
# In acest caz, cea mai mica eroare este 0.55 aferenta numarului de 8 noduri

# Reprezentare grafica a deviantei in functie de nr de noduri
plot(cv.trees$size, cv.trees$dev, type = "b")
# Din grafic deducem faptul ca eroarea minima se atinge cand avem 8 noduri terminale

# Construim arborele curatat
arbore1 <- prune.tree(arbore, best = 8)
plot(arbore1)
text(arbore1, pretty = 0)

# Predictii pe setul de testare
pred <- predict(arbore1, testare)
pred # in acest rezultat sunt afisate valorile previzionate pentru chirie (forma logaritmata)

# Reprezentarea grafica a valorilor previzionate pentru chirii din setul de testare
plot(pred, testare$Rent_USD) 

# Eroarea de predictie
mean((pred-testare$Rent_USD)^2)
# 0.0013% din observatii au fost previzionate eronat => deci modelul de previziune este bun

################
# Algoritmul KNN
################

library(caret)
library(e1071)

# Scopul este de a clasifica programele ca "Program accesibil" vs "Program inaccesibil", pe baza costurilor totale estimate

# Calcularea costului total
date_proiect$Total_Cost <- date_proiect$Tuition_USD + (date_proiect$Rent_USD*12*date_proiect$Duration_Years)+date_proiect$Visa_Fee_USD 
df_totalcost <- data.frame(date_proiect$University, date_proiect$Total_Cost)
View(df_totalcost)
# Valoarea maxima pt Total Cost
date_proiect[which.max(date_proiect$Total_Cost),]
# Valoarea minima pt Total Cost
date_proiect[which.min(date_proiect$Total_Cost),]

# Variabila tinta va fi Accesible si va avea valorile: accesibil (1) daca costul total <= 15000, inaccesibil (0) altfel
date_proiect$Accessible <- ifelse(date_proiect$Total_Cost <= 15000, 1, 0)
table(date_proiect$Accessible) # 296 programe sunt inaccesibile, 498 sunt accesibile

# Transform variabila in variabila de tip factor
date_proiect$Accessible <- as.factor(date_proiect$Accessible)

set.seed(101)

# Impartirea setului de date in antrenare si testare
index <- sample(2, nrow(date_proiect), replace = TRUE, prob = c(0.7,0.3))
index

set_antrenare <- date_proiect[index==1,]
set_antrenare

set_testare <- date_proiect[index==2,]
set_testare

# In mod implicit, nivelurile variabilei dependente sunt 0 si 1
# Le voi transforma in nume de variabile valide
levels(set_antrenare$Accessible) <- make.names(levels(factor(set_antrenare$Accessible)))
levels(set_testare$Accessible) <- make.names(levels(factor(set_testare$Accessible)))

# Validare incrucisata
# Voi defini repeats=3, numbers=10
repeats=3
numbers=10
set.seed(1234)
x=trainControl(method="repeatedcv", number=numbers, repeats = repeats, classProbs = TRUE, summaryFunction = twoClassSummary)
tunel = 10

# Definirea modelului KNN
# Voi utilza doar variabilele numerice in model
model_knn <- train(Accessible~Duration_Years+Tuition_USD+Living_Cost_Index+Rent_USD+Visa_Fee_USD, data= set_antrenare, method="knn", preProcess=c("center", "scale"), trControl=x, metric='ROC', tuneLength = tunel)
model_knn
# Cea mai mare valoare a ariei de sub curba ROC este 0.9956922, aferenta modelului optim cu cei mai apropiati 23 vecini
# Valoarea curbei ROC in acest caz este apropiata de 1, fiind un model bun pentru separarea programelor accesibile de cele inaccesibile

plot(model_knn)

# Predictii pe setul de testare
valid_pred <- predict(model_knn, set_testare, type="prob")
head(valid_pred)
# Pentru primele 6:
# Primul program este previzionat a fi inaccesibil
# Al doilea accesibil
# Al treilea accesibil
# Al patrulea acc
# Al cincilea inacc
# Al saselea acc

# Reprezentare grafica a ariei de sub curba ROC pt setul de testare
library(ROCR)
pred_val <- prediction(valid_pred[,2], set_testare$Accessible)
perf_val <- performance(pred_val, "tpr", "fpr")
plot(perf_val, col="red", lwd=1.5)
# Graficul se apropie de coltul din stanga sus, ceea ce este ideal

auc <- performance(pred_val, measure="auc")
auc <- auc@y.values[[1]]
auc
# Aria de sub curba ROC este de 0.9947524, indicand 99.4% sanse de a separa clasa pozitiva de clasa negativa

##################
# Retele neuronale
##################

# 1. Retele neuronale cand variabila dependenta are > 2 clase

# Variabila calitativa utilizata va fi 'Level', care ia valorile: 'Bachelor', 'Master' sau 'PhD'
table(date_proiect$Level)
# Vom elimina celelalte variabile calitative
date_rn <- date_proiect[,-c(1,2,3,4)]
View(date_rn)
# Extrag in mod aleatoriu 500 observatii pt setul de antrenare:
set.seed(123)
train <- date_rn[sample(1:794,500),]
head(train)
# Adaug in setul de antrenare atribute ce convin valoarea de adevar a apartenentei fiecarei observatii la cele 3 categorii
train$Bachelor <- c(train$Level=="Bachelor")
train$Master <- c(train$Level=="Master")
train$PhD <- c(train$Level=="PhD")
# Elimin variabila 'Level' din setul de date
train$Level <- NULL
head(train)

# Voi standardiza variabilele numerice, deoarece au scale diferite
var_exp <- c("Duration_Years", "Tuition_USD", "Living_Cost_Index", "Rent_USD", "Visa_Fee_USD")
# Media si deviatia standard pe setul de antrenare
means <- apply(train[, var_exp], 2, mean)
sds <- apply(train[, var_exp], 2, sd)
# Standardizez datele din train
train[, var_exp] <- scale(train[, var_exp], center = means, scale = sds)
# Standardizez toate datele pentru predicție (date_rn), folosind media și sd din train
date_rn_scaled <- date_rn
for (v in var_exp) {
  date_rn_scaled[, v] <- (date_rn_scaled[, v] - means[v]) / sds[v]
}

# Antrenarea retelei neuronale ce contine 3 noduri in stratul ascuns:
library(neuralnet)
retea_neuronala <- neuralnet(Bachelor+Master+PhD~Duration_Years+Tuition_USD+Living_Cost_Index+Rent_USD+Visa_Fee_USD,train,hidden = 3,lifesign = "full",stepmax = 1e6)
plot(retea_neuronala, rep="best", intercept=FALSE)

# Predictii pentru nivelul programului de studii, dupa eliminarea coloanei 1 ('Level')
predictie <- compute(retea_neuronala,date_rn_scaled[,-c(1)])
predictie

# Gradul de apartenenta al fiecarui program la cele 3 categorii
predictie$net.result[1:10,]

# Variabila rezultat va contine clasele "Bachelor", "Master" si "PhD", pe baza predictiei
rez <- 0
for(i in 1:794){rez[i] <- which.max(predictie$net.result[i,])}
for(i in 1:794){if(rez[i]==1){rez[i]="Bachelor"}}
for(i in 1:794){if(rez[i]==2){rez[i]="Master"}}
for(i in 1:794){if(rez[i]==3){rez[i]="PhD"}}

# Comparam rezultatele cu valorile reale
comparatie <- date_rn
comparatie$Predicted <- rez
comparatie
head(comparatie)
# Variabila denumita comparatie cuprinde col 1 (val reale) si 7 (val previzonate )
comparatie[1:10, c(1,7)]

# Matricea de confuzie
tab <- table(comparatie$Level, comparatie$Predicted)
tab

install.packages("e1071")
library(e1071)

classAgreement(tab)

# 2. Retele neuronale in regresie (previzionarea valorilor unei variabile numerice)

# Variabila numerica tinta va fi Total_Cost (costul total) pe care imi doresc sa o previzionez in 
# functie de durata programului si indicele costului de trai (nu voi folosi celelalte variabile deoarece 
# sunt componente directe ale costului total)
View(date_proiect)

set.seed(123)
install.packages("caTools")
library(caTools)

# Voi elimina coloanele care nu sunt numerice:
date_proiect1 <- date_proiect[,-c(1:5)]
View(date_proiect1)

# Voi imparti setul de date in 75% antrenare si 25% testare
split <- sample.split(date_proiect1$Tuition_USD, SplitRatio = 0.75)
set_antrenare <- subset(date_proiect1,split==TRUE)
set_testare <- subset(date_proiect1,split==FALSE)
View(set_testare)

# Voi standardiza datele folosind metoda max-min:
maxim <- apply(date_proiect1,2,max)
minim <- apply(date_proiect1,2,min)
data_std <- as.data.frame(scale(date_proiect1,center = minim,scale=maxim-minim))
View(data_std)
data_std$split <- split

# Definirea seturilor de antrenare si testare ale retelei
antrenare_retea <- subset(data_std, split==TRUE)
testarere_retea <- subset(data_std, split==FALSE)

# Verificăm numele variabilelor
names(antrenare_retea)

# Construirea retelei neuronale:
retea_neuronala1 <- neuralnet(Total_Cost ~ Duration_Years+Living_Cost_Index, 
                   data = antrenare_retea, hidden = 5, linear.output = TRUE)
# Reprezentare grafica
plot(retea_neuronala1)

# Previzionarea valorilor taxei de scolarizare
predictie <- compute(retea_neuronala1,testarere_retea[,c(1,3)])
# Vizualizarea rezultatelor standardizate
predictie$net.result
# Aducem valorile la dimensiunile reale pentru a le putea compara cu valorile reale
predictie <- (predictie$net.result*(max(date_proiect$Total_Cost)-min(date_proiect$Total_Cost)))+min(date_proiect$Total_Cost)
# Vizualizam predictiile
head(predictie)

# Voi reprezenta grafic val reale si cele previzionate:
plot(set_testare$Total_Cost, predictie, col="green", pch=14,ylab="Val. Previzionate", xlab="Valori reale")
abline(0,1)
# Determin eroarea RMSE(Root Mean Squared Error) pt setul de testare conform formulei:
eroare <- (sum(set_testare$Total_Cost-predictie)^2/nrow(set_testare))^0.5
eroare #eroarea este de 57840.79
df_rn <- data.frame(set_testare$Total_Cost,predictie) #Valori reale vs previzionate
View(df_rn)
