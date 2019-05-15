require(rpart)
require(rpart.plot)
require(FactoMineR)
require(MASS)
require(ROCR)
require(gplots)
require(randomForest)
require(glmnet)
require(car)
require(factoextra)
require(ape)
require(corrplot)

###################################CHARGEMENT DES DONNEES##############################
dataFinal=read.csv2("/Users/Wildsoap/Documents/MAIN4/Atlas/2015Final.csv",header=T,sep=",",row.names=1)

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

eig.val <- get_eigenvalue(pca2)
eig.val

fviz_eig(pca2, addlabels = TRUE, ylim = c(0, 50))



#On peut donc voir quelles axes représente le mieux quelle variable
var <- get_pca_var(pca2)

corrplot(var$cos2, is.corr=FALSE)

#On pourrait garder entre 2, et 3 axes

#Bien expliquer l'opposition Alphabétisation/Espérance de Vie
#Faire un classement de l'alphabétisation (en tête il y a les anciens pays du bloc de l'est)

#On retire démocratie et bonheur car pas assez bien représenté

dataACP3=subset(data,select=c(3,4,5,6,7,8))

dataACP3[dataACP3==0]=0.001 
tab=log(dataACP3)
data2=t(scale(t(tab)))
pca3 <- PCA(data2)

#On fait un ACP qu'entre bonheur et démocratie

dataACP4=subset(data,select=c(1,2))

pca4 <- PCA(dataACP4)

#On n'observe qu'un petit lien entre bonheur et démocratie

#########################QUELQUES REGRESSIONS########################################

regression1=lm(data$Liberalisme~data$EsperanceDeVie)

plot(data$EsperanceDeVie,data$Liberalisme,xlab="Espérance de Vie",ylab="Libéralisme")
abline(regression1)

#La corrélation semble bien avéré entre Libéralisme et espérance de vie

summary(regression1)

confint(regression1)


#La p-value très largement inférieur à 0.5, le modèle est donc vérifié


regression2=lm(data$Population~data$Bellicisme)

plot(data$Bellicisme,data$Population,xlab="Bellicisme",ylab="Population")
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

#Meilleure visualisation : 
colors = c("red", "blue", "green", "pink","purple","orange")
clus4 = cutree(cah.ward.cr, 6)
plot(as.phylo(cah.ward.cr), type = "fan", tip.color = colors[clus4],label.offset = 1, cex = 0.8)

plot(as.phylo(cah.ward.cr), type = "radial", tip.color = colors[clus4],label.offset = 0.1,cex=0.7)


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


meansGroupes <- matrix(NA, nrow=K, ncol=dim(data)[2])
colnames(meansGroupes)=colnames(data)
for (i in 1:K) meansGroupes[i,]<- colMeans(data[groupes.6==i,])
meansGroupes

par(mfrow=c(2,2))
barplot(meansGroupes[,"Bonheur"],main="Moyenne de bonheur",xlab="Groupes",ylab="Bonheur",col=c("red", "blue", "green", "pink","purple","orange"))

barplot(meansGroupes[,"Democratie"],main="Moyenne de democratie",xlab="Groupes",ylab="Democratie",col=c("red", "blue", "green", "pink","purple","orange"))

barplot(meansGroupes[,"Population"],main="Moyenne de population",xlab="Groupes",ylab="Population",col=c("red", "blue", "green", "pink","purple","orange"))

barplot(meansGroupes[,"PIB"],main="Moyenne de PIB",xlab="Groupes",ylab="PIB",col=c("red", "blue", "green", "pink","purple","orange"))
par(mfrow=c(1,1))


#################################KMEANS#######################################

#On utilise le kmeans du package factoextra
#Car il permet de tracer la courbe des silhouettes facilement

inertie.intra <- rep(0,times=20)
for (k in 1:10){
  #On ne s'occupe pas de l'inertie intra pour k<3
  inertie.intra[k] =0
  if k>3 :
    kmeans.result <- eclust(data, "kmeans", k = k,nstart = 100, graph = FALSE)
    inertie.intra[k] <- kmeans.result$tot.withinss/kmeans.result$totss
}
# graphique de l'inertie, afin de choisir le nombre de classes optimales
plot(1:20,inertie.intra,type="b",xlab="Nb. de groupes",ylab="% inertie intra")

kmeans.result <- eclust(data, "kmeans", k = 7,nstart = 100, graph = FALSE)

pairs(data, col=kmeans.result$cluster)

plot(pca2, choix="ind", col.ind=kmeans.result$cluster, cex=pca2$ind$cos2)


fviz_silhouette(kmeans.result)


#############################ARBRE CART######################################

#arbreData=subset(data,select=c(5,6,7,8,9,10))

#Étudions le bonheur, selon vous qu'est ce qui est important pour être heureux (selon nos variables) ? 

arbreBonheur=rpart(Bonheur~.,data)#,control=rpart.control(cp=0.016))

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

#Proportion des régimes
pie(table(dataDemocratie$Democratie))
#Peu de démocratie pleine, et beaucoup de régimes autoritaires


##############################QUELQUES BOXPLOT###############################

boxplot(dataDemocratie$PIB~dataDemocratie$Democratie,xlab="Régime Politique",ylab="PIB")

#Dans l'ensemble, les démocraties sont celles avec une meilleure économie
#Néanmoins, on voit que les résultats pour les régimes Autoritaires sont assez dispercés
#En revanche, un régime hybride ne permet pas une économie florissante

boxplot(dataDemocratie$Bonheur~dataDemocratie$Democratie,xlab="Régime Politique",ylab="Bonheur")

#Prévisible, mais bon à rapeller

#############################################

dataBonheurPolitique=subset(dataDemocratie)

#On définit arbitrairement ses classes

dataBonheurPolitique$Bonheur[dataBonheurPolitique$Bonheur>=6.6] <- "Très Heureux"
dataBonheurPolitique$Bonheur[dataBonheurPolitique$Bonheur<4.4] <-"Très Malheureux"
dataBonheurPolitique$Bonheur[dataBonheurPolitique$Bonheur<5] <-"Malheureux"
dataBonheurPolitique$Bonheur[dataBonheurPolitique$Bonheur<5.8] <-"Assez Heureux"
dataBonheurPolitique$Bonheur[dataBonheurPolitique$Bonheur<6.6] <-"Bien Heureux"


table(dataBonheurPolitique$Bonheur)

str(dataBonheurPolitique)
dataBonheurPolitique$Bonheur=as.factor(dataBonheurPolitique$Bonheur)
str(dataBonheurPolitique)

regression3=lm(dataBonheurPolitique$PIB~dataBonheurPolitique$Bonheur*dataBonheurPolitique$Democratie)

anova(regression3)

par(mfrow=c(2,2))
plot(regression3)
par(mfrow=c(1,1))

#Regarder si la p-value 


#On fait une transformation logarithmique pour réduire l'effet trompette
regression4=lm(log(dataBonheurPolitique$PIB)~dataBonheurPolitique$Bonheur*dataBonheurPolitique$Democratie)

anova(regression4)

par(mfrow=c(2,2))
plot(regression4)
par(mfrow=c(1,1))

#Residuals vs Fitted : Les résidus ne semblent pas suivre de pattern
#Normal Q-Q : Le modèle suit bien une loi normale
#Scale Location : Les résidus sont répartis avec une variance égale (homoscédasciticé)
#Residuals vs Leverage : Les résidus semblent indépendant

#p-value <0.05 : Donc c'est bon

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


