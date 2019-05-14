data=read.csv2("/Users/Wildsoap/Documents/MAIN4/Bonheur/2015.csv",header=T,sep=",",row.names=1)
datacopy=read.csv2("/Users/Wildsoap/Documents/MAIN4/Bonheur/2015.csv",header=F,sep=",",row.names=1)

testdata=load("/Users/Wildsoap/Desktop/NewData.RData")

####################################NETTOYAGE DU DATASET####################################

head(data)


str(data)

#On transforme les variables qualitatives en quantitaves

data$Happiness.Score=as.character(data$Happiness.Score)
data$Happiness.Score=as.numeric(data$Happiness.Score)

data$Standard.Error=as.character(data$Standard.Error)
data$Standard.Error=as.numeric(data$Standard.Error)

data$Economy..GDP.per.Capita. =as.character(data$Economy..GDP.per.Capita. )
data$Economy..GDP.per.Capita. =as.numeric(data$Economy..GDP.per.Capita. )

data$Family=as.character(data$Family)
data$Family=as.numeric(data$Family)

data$Health..Life.Expectancy.=as.character(data$Health..Life.Expectancy.)
data$Health..Life.Expectancy.=as.numeric(data$Health..Life.Expectancy.)

data$Freedom=as.character(data$Freedom)
data$Freedom=as.numeric(data$Freedom)

data$Trust..Government.Corruption.=as.character(data$Trust..Government.Corruption.)
data$Trust..Government.Corruption.=as.numeric(data$Trust..Government.Corruption.)

data$Generosity=as.character(data$Generosity)
data$Generosity=as.numeric(data$Generosity)

data$Dystopia.Residual=as.character(data$Dystopia.Residual)
data$Dystopia.Residual=as.numeric(data$Dystopia.Residual)

#data$Country=as.character(data$Country)
data$Region=as.factor(data$Region)


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

#Nombre de pays que l'on analyse (il y en a 194 d'après l'ONU)
n=NROW(data)

#Voilà les indices surlesquelles nous allons nous concentrer
#Afin d'évaluer leur importances, nous avons réalisé un boxplot

#boxData=subset(data,select=c(6,7,8,9,10,11))

#boxplot(boxData)


#############################ARBRE CART######################################

arbreData=subset(data,select=c(5,6,7,8,9,10))

arbre=rpart(Freedom~.,arbreData)

#print(arbre)
#plotcp(arbre)
#summary(arbre)

rpart.plot(arbreLiberté,type=4)

arbre=rpart(Freedom~.,arbreData)


###########################ACP#######################################

dataOutRank=subset(data,select=c(6,7,8,9,10))

pca <- PCA(dataOutRank,scale.unit=F)

#On passe au log car sinon c'est illisible

dataOutRank[dataOutRank==0]=0.001 
tab=log(dataOutRank)
data2=t(scale(t(tab)))
pca2 <- PCA(data2)


##############################CAH############################

dataOutRank.cr = scale(dataOutRank,center=T,scale=T)
dist.dataOutRank.cr <- dist(dataOutRank.cr)
cah.ward.cr <- hclust(dist.dataOutRank.cr,method="ward.D2")

plot(cah.ward.cr,hang =-1)
barplot(cah.ward.cr$height)

#A la louche, on va garder K=3 classes

K=3

plot(cah.ward.cr,hang =-1,main="ward.D2")      
rect.hclust(cah.ward.cr,k=3)

#liste des groupes
groupes.3 <- cutree(cah.ward.cr,k=3)
groupes.3
table(groupes.3)


#################################KMEANS#######################################

kmeans.result <- kmeans(dataOutRank.cr,centers=K,nstart=100)

pairs(dataOutRank, col=kmeans.result$cluster )

#Afficher les 3 groupes trouvé par le Kmeans sur une carte

#####################################################################

library(MASS)

newData=subset(data,select=c(1,5,6,7,8,9,10))
str(newData)
table(newData$Region)

newData$Region=as.character(newData$Region)
str(newData)

#Les groupes de régions sont trop petits, on va en fusionner certains

newData$Region[newData$Region == "Eastern Asia"] <- "Asia"
newData$Region[newData$Region == "Southern Asia"] <- "Asia"
newData$Region[newData$Region == "Southeastern Asia"] <- "Asia"

newData$Region[newData$Region == "North America"] <- "Western Europe and North America and Oceania"
newData$Region[newData$Region == "Australia and New Zealand"] <- "Western Europe and North America and Oceania"
newData$Region[newData$Region == "Western Europe"] <- "Western Europe and North America and Oceania"

newData$Region=as.factor(newData$Region)
table(newData$Region)

#newData$Region=as.character(newData$Region)
#dataSupervisee = newData[newData$Region !="North America" && newData$Region!="Australia and New Zealand"]
#dataSupervisee = newData[newData$Region!="Australia and New Zealand"]
#dataSupervisee$Region=as.factor(dataSupervisee$Region)
#table(dataSupervisee$Region)

set.seed(42)
#n <- nrow(dataSupervisee)
#p <- ncol(dataSupervisee)-1
n <- nrow(newData)
p <- ncol(newData)-1
test.ratio <- .2 # ratio of test/train samples
n.test <- round(n*test.ratio)
tr <- sample(1:n,n.test)
data.test <- newData[tr,]
data.train <- newData[-tr,]

res_lda <- lda(Region~.,data=data.train)
res_qda <- qda(Region~.,data=data.train)

class_lda <- predict(res_lda,newdata=data.test)$class
class_qda <- predict(res_qda,newdata=data.test)$class

table(class_lda, data.test$Region)

table(class_qda, data.test$Region)

accuracy_lda = mean(class_lda == data.test$Region)
accuracy_qda = mean(class_qda == data.test$Region)
accuracy_lda
accuracy_qda

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

################################RANDOM FOREST################################

library(randomForest)

fit_RF <- randomForest(Region~.,data.train)
fit_RF
plot(fit_RF)

class_RF= predict(fit_RF, newdata=data.test,  type="response")

table(class_RF, data.test$Region)

accuracy_RF = mean(class_RF == data.test$Region)
accuracy_RF


############################REGRESSION LOGISTIQUE######################
library(glmnet)

#MARCHE PAS
logit.train <- glm(Region ~ ., family = binomial , data=data.train)

res_Lasso <- glmnet(as.matrix(data.train[,-1]),data.train$Region,family='binomial')  
cvLasso <- cv.glmnet(as.matrix(data.train[,-1]),data.train$Region,family="binomial", type.measure = "class") 
cvLasso$lambda.min

#####################################################

boxplot(newData$Freedom ~ newData$Family)
