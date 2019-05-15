dataFinal=read.csv2("/Users/Wildsoap/Documents/MAIN4/Atlas/2015Final.csv",header=T,sep=",",row.names=1)
dataFinalNum=read.csv2("/Users/Wildsoap/Documents/MAIN4/Atlas/2015Final.csv",header=F,sep=",",row.names=1)

data=subset(dataFinal,select=c(3,12,13,14,15,16,17,18))

#data$Dystopia.Residual=subset(dataAlphabetisation,select=c(1))

####################################NETTOYAGE DU DATASET####################################

head(data)


str(data)

#On transforme les variables qualitatives en quantitaves

data$Bonheur=as.character(data$Bonheur)
data$Bonheur=as.numeric(data$Bonheur)

data$Democratie=as.character(data$Democratie)
data$Democratie=as.numeric(data$Democratie)

data$Bellicisme=as.character(data$Bellicisme)
data$Bellicisme=as.numeric(data$Bellicisme)

data$Alphabetisation=as.character(data$Alphabetisation)
data$Alphabetisation=as.numeric(data$Alphabetisation)

data$PIB=as.character(data$PIB)
data$PIB=as.numeric(data$PIB)

data$EsperanceDeVie=as.character(data$EsperanceDeVie)
data$EsperanceDeVie=as.numeric(data$EsperanceDeVie)

data$Liberalisme=as.character(data$Liberalisme)
data$Liberalisme=as.numeric(data$Liberalisme)

#data$Country=as.character(data$Country)
#data$Region=as.factor(data$Region)


str(data)

attach(data)

############################################################################

require(rpart)
require(rpart.plot)
require(FactoMineR)
require(MASS)
require(ROCR)
require(gplots)
require(randomForest)
require(glmnet)
require(car)

#Nombre de pays que l'on analyse (il y en a 194 d'après l'ONU)
n=NROW(data)
n

###########################ACP#######################################

#dataOutRank=subset(data,select=c(6,7,8,9,10))

pca <- PCA(data,scale.unit=F)

#On passe au log car sinon c'est illisible

data[data==0]=0.001 
tab=log(data)
data2=t(scale(t(tab)))
pca2 <- PCA(data2)

#Bien expliquer l'opposition Alphabétisation/Espérance de Vie
#Faire un classement de l'alphabétisation (en tête il y a les anciens pays du bloc de l'est)

#On retire démocratie et bonheur car pas assez bien représenté

dataACP3=subset(data,select=c(3,4,5,6,7,8))

dataACP3[dataACP3==0]=0.001 
tab=log(dataACP3)
data2=t(scale(t(tab)))
pca2 <- PCA(data2)

#On fait un ACP qu'entre bonheur et démocratie

dataACP4=subset(data,select=c(1,2))

pca2 <- PCA(dataACP4)

#On n'observe qu'un petit lien entre bonheur et démocratie

#########################QUELQUES REGRESSIONS########################################

regression1=lm(data$Liberalisme~data$EsperanceDeVie)

plot(data$EsperanceDeVie,data$Liberalisme)
abline(regression1)

#La corrélation semble bien avéré entre Libéralisme et espérance de vie

summary(regression1)

confint(regression1)


#La p-value très largement inférieur à 0.5, le modèle est donc vérifié


regression2=lm(data$Population~data$Bellicisme)

plot(data$Bellicisme,data$Population)
abline(regression2)

#La corrélation entre Population et Bellicisme ne semble pas très net

summary(regression2)

#Ce qui est confirmé par le p-value supérieur à 0.05

##############################CAH############################

data.cr = scale(data,center=T,scale=T)
dist.data.cr <- dist(data.cr)
cah.ward.cr <- hclust(dist.data.cr,method="ward.D2")

plot(cah.ward.cr,hang =-1)
barplot(cah.ward.cr$height)

#A la louche, on va garder K=4 classes

K=4

plot(cah.ward.cr,hang =-1,main="ward.D2")      
rect.hclust(cah.ward.cr,k=4)

#liste des groupes
groupes.4 <- cutree(cah.ward.cr,k=4)

sort(groupes.4)
I=which(groupes.4==4)
I


#Inde et la Chine relativement à part

#Essayons avec K=6 classes

K=6

plot(cah.ward.cr,hang =-1,main="ward.D2")      
rect.hclust(cah.ward.cr,k=6)

#liste des groupes
groupes.6 <- cutree(cah.ward.cr,k=6)

sort(groupes.6)
I3=which(groupes.6==3)
I3

#Groupe 3 : Petit pays avec un fort PIB

I1=which(groupes.6==1)
I1

#Pays développés, majoritairement européen


I4=which(groupes.6==4)
I4



#Pays Autoritaire

I5=which(groupes.6==5)
I5



#Pays peu développés

I6=which(groupes.6==6)
I6

#Inde et Chine, vraiment à part





#################################KMEANS#######################################

kmeans.result <- kmeans(data,centers=K,nstart=100)

pairs(data, col=kmeans.result$cluster )

#Afficher les 6 groupes trouvé par le Kmeans sur une carte

#############################ARBRE CART######################################

#arbreData=subset(data,select=c(5,6,7,8,9,10))

#Étudions le bonheur, selon vous qu'est ce qui est important pour être heureux (selon nos variables) ? 

arbreBonheur=rpart(Bonheur~.,data))#,control=rpart.control(cp=0.016))

#print(arbre)
plotcp(arbreBonheur) #On ne va pas l'élaguer car on veut voir les paramètres qui influencent le bonheur
#summary(arbre)

rpart.plot(arbreBonheur,type=4)

#L'argent ne fait pas le bonheur, mais il y contribue
#L'espérance de vie est plus déterminante pout le bonheur


arbreDemocratie=rpart(Democratie~.,data,control=rpart.control(cp=0.034))

plotcp(arbreDemocratie)

rpart.plot(arbreDemocratie,type=4)

#Une démocratie fait moins de morts, car moins de guerre, et est plus compatible avec la démocratie


arbrePIB=rpart(PIB~.,data,control=rpart.control(cp=0.013))

plotcp(arbrePIB)

rpart.plot(arbrePIB,type=4)

#Le libéralisme amène du PIB, mais aussi qu'au bout se sont toujours les petits pays qui auront plus de PIB




############################VARIABLE QUALITATIVE : DEMOCRATIE########################################

#On va transformer nos indices de démocratie en une variable qualitative

dataDemocratie=subset(data)

head(dataDemocratie)
str(dataDemocratie)


dataDemocratie$Democratie[dataDemocratie$Democratie>=8] <- "Démocratie Pleine"
dataDemocratie$Democratie[dataDemocratie$Democratie<4] <-"Régime Autoritaire"
dataDemocratie$Democratie[dataDemocratie$Democratie<6] <-"Régime Hybride"
dataDemocratie$Democratie[dataDemocratie$Democratie<8] <-"Démocratie Imparfaite"

#On choisit ses classes selon des stats officiels

#On vérifie que les changements ont bien eu lieu
table(dataDemocratie$Democratie)

str(dataDemocratie)

dataDemocratie$Democratie=as.factor(dataDemocratie$Democratie)

str(dataDemocratie)

##############################QUELQUES BOXPLOT###############################

boxplot(dataDemocratie$PIB~dataDemocratie$Democratie)

#Dans l'ensemble, les démocraties sont celles avec une meilleure économie
#Néanmoins, on voit que les résultats pour les régimes Autoritaires sont assez dispercés
#En revanche, un régime hybride ne permet pas une économie florissante

boxplot(dataDemocratie$Bonheur~dataDemocratie$Democratie)

#Prévisible, mais bon à rapeller

#############################################

dataBonheurPolitique=subset(dataDemocratie)

dataBonheurPolitique$Bonheur[dataBonheurPolitique$Bonheur>=6.5] <- "Très Heureux"
dataBonheurPolitique$Bonheur[dataBonheurPolitique$Bonheur<4.2] <-"Très Malheureux"
dataBonheurPolitique$Bonheur[dataBonheurPolitique$Bonheur<5] <-"Malheureux"
dataBonheurPolitique$Bonheur[dataBonheurPolitique$Bonheur<6.5] <-"Heureux"


table(dataBonheurPolitique$Bonheur)

regression3=lm(dataDemocratie$PIB~dataDemocratie$ )

#####################################LDA/QDA######################################

set.seed(42)
n <- nrow(dataDemocratie)
p <- ncol(dataDemocratie)-1
test.ratio <- .2 # ratio of test/train samples
n.test <- round(n*test.ratio)
tr <- sample(1:n,n.test)
data.test <- dataDemocratie[tr,]
data.train <- dataDemocratie[-tr,]

res_lda <- lda(Democratie~.,data=data.train)
res_qda <- qda(Democratie~.,data=data.train)

class_lda <- predict(res_lda,newdata=data.test)$class
class_qda <- predict(res_qda,newdata=data.test)$class

table(class_lda, data.test$Democratie)

table(class_qda, data.test$Democratie)

accuracy_lda = mean(class_lda == data.test$Democratie)
accuracy_qda = mean(class_qda == data.test$Democratie)
accuracy_lda
accuracy_qda

#Mauvais résultats, on ne peut pas conclure


################################RANDOM FOREST################################

library(randomForest)

fit_RF <- randomForest(Democratie~.,data.train)
fit_RF
plot(fit_RF)

class_RF= predict(fit_RF, newdata=data.test,  type="response")

table(class_RF, data.test$Democratie)

accuracy_RF = mean(class_RF == data.test$Democratie)
accuracy_RF

#On a aussi de mauvais résultats avec randomForest
#Cela montre que les régimes politiques ne dépend pas des variables que l'on a fixé
#Il y a très probablement aussi un aspect historique derrière


##########################COURBE ROC LDA/QDA####################################

#MARCHE PAS

library(ROCR)

pred_lda <- predict(res_lda,newdata=data.test)$posterior[,2]
predictions_lda <- prediction(pred_lda,  data.test$Region)
perf_lda <- performance(predictions_lda , "tpr", "fpr" )
plot(perf_lda)
AUC_lda <- performance(predictions_lda,"auc")@y.values[[1]]
AUC_lda

pred_qda <- predict(res_qda,newdata=data.test)$posterior[,2]
predictions_qda <- prediction(pred_qda,  data.test$DIFF)
perf_qda <- performance(predictions_qda , "tpr", "fpr" )
plot(perf_qda, add=TRUE, col="red")



############################REGRESSION LOGISTIQUE######################
library(glmnet)

#MARCHE PAS
logit.train <- glm(Region ~ ., family = binomial , data=data.train)

res_Lasso <- glmnet(as.matrix(data.train[,-1]),data.train$Region,family='binomial')  
cvLasso <- cv.glmnet(as.matrix(data.train[,-1]),data.train$Region,family="binomial", type.measure = "class") 
cvLasso$lambda.min

#####################################################

boxplot(newData$Freedom ~ newData$Family)
