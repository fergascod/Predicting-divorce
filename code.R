## ESTUDI PREVI
## -----------------------------------------------------------
# Lectura de dades

data <- read.csv2("divorce.csv")
colors =c("orange", "blue")

n <- dim(data)[1]
p <- dim(data)[2]-1

dd <- data
for (i in 1:(p+1)){
  dd[,i] <- as.factor(dd[,i])
}


## -----------------------------------------------------------
knitr::kable(head(dd[,c(1:7,(p-6):p+1)]))


## -----------------------------------------------------------
knitr::kable(data.frame("Divorciats"=86, "Casats"=84))


## ---- warning=FALSE-----------------------------------------
# Test Chi-quadrat de les variables categòriques amb la target
chi<-c()
for (i in 1:p){
  chi<-c(chi,(chisq.test(dd[,i], dd$Class))$p.value)
}
table(chi>0.05)


## -----------------------------------------------------------
par(mfrow=c(2,2))
d.div<-dd[dd$Class=="Divorciats",]
d.cas<-dd[dd$Class=="Casats",]

boxplot(as.numeric(dd$Atr1)~dd$Class,xlab="Classe", ylab="Pregunta 1")
boxplot(as.numeric(dd$Atr2)~dd$Class,xlab="Classe", ylab="Pregunta 2")
boxplot(as.numeric(dd$Atr3)~dd$Class,xlab="Classe", ylab="Pregunta 3")
boxplot(as.numeric(dd$Atr4)~dd$Class,xlab="Classe", ylab="Pregunta 4")

## TRACTANT LES DADES COM A CATEGÒRIQUES
## Naive-Bayes
## -----------------------------------------------------------
# LOOCV
library(e1071) #Funció naiveBayes
sum=0
for (i in 1:n){
  mod <- naiveBayes(Class~., data=dd[-i,])
  correct <- (data[i,"Class"]==predict(mod, newdata=dd[i,], type="class"))
  sum=sum+correct
}
(1-sum/n)*100


## -----------------------------------------------------------
# Independència entre les variables predictores
chi<-c()
for (i in 1:p){
  for (j in 1:p){
    chi<-c(chi,(chisq.test(dd[,i], dd[,j]))$p.value)
  }
}
table(chi>0.05)

## Random Forest
## -----------------------------------------------------------
library(randomForest)

num_trees <- 2^(0:10)
oob.err <- c()
set.seed(2048)
for (i in num_trees){
  forest <- randomForest(Class~., data=dd, ntree=i, proximity=T, importance=T)
  oob.err <- c(oob.err, forest$err.rate[i,1])
}
plot(log(num_trees,2),oob.err, type='l')
oob.err[5+1]


## -----------------------------------------------------------
tuneRF(dd[,-55], dd$Class, ntreeTry=32)


## -----------------------------------------------------------
(forest <- randomForest(Class~., data=dd, ntree=32, proximity=T, importance=T))
plot(forest, main="Error de l'ensemble")
legend("topright", legend=c("OOB Error", "Error divorci", "Error no divorci"), fill=c("black", "red","green"))


## -----------------------------------------------------------
varImpPlot(forest, cex=0.5, cex.lab=1.75, main="Importància de les variables")


## -----------------------------------------------------------
MDSplot(forest, dd$Class, k=2, main="MDS de la matriu de proximitats")


## -----------------------------------------------------------
# LOOCV
sum=0
for (i in 1:n){
  forest <- randomForest(Class~., data=dd[-i,], ntree=32, mtry=7, proximity=T, importance=T)
  sum=sum+(predict(forest, newdata=dd[i,])==dd[i,"Class"])
}
100*(1-sum/n)

## TRACTANT LES DADES COM A NUMÈRIQUES
## -----------------------------------------------------------
dd.factor <- dd
dd <- data # Recuperem les dades originals (sense factors)

## GLM (Regressió logística) 
## ----warning=FALSE------------------------------------------
sum=0
for (i in 1:n){
  mod.glm<-glm(Class~., dd[-i,], family="binomial")
  sum(diag(table(round(predict(mod.glm, type="response")), dd[-i, "Class"])))/(n-1)
  correct=round(predict(mod.glm,newdata=dd[i,],type="response"))==dd[i, "Class"]
  sum=sum+correct
}
(1-sum/n)*100


## ----warning=FALSE------------------------------------------
mod.glm<-glm(Class~., dd, family="binomial")
step(mod.glm, trace=FALSE)


## ----warning=FALSE------------------------------------------
sum=0
for (i in 1:n){
  mod.glm<-glm(formula = Class ~ Atr14 + Atr17 + Atr24 + Atr40,
               family = "binomial", data = dd[-i,])
  correct=round(predict(mod.glm, newdata=dd[i,],type="response"))==dd[i, "Class"]
  sum=sum+correct
}
(1-sum/n)*100


## -----------------------------------------------------------
# PCA
par(mfrow=c(1,2))
pca <- princomp(dd[,-55])
plot(pca, main="% variança dels components")
plot(pca$scores[,1:2],col=colors[as.factor(dd$Class)], main="PCA")


## ----warning=FALSE------------------------------------------
sum=0
for (i in 1:n){
  i=1
  pca <- princomp(dd[-i,-55])
  mod.glm<-glm(dd$Class[-i]~., family = "binomial", data = data.frame("Comp.1"=pca$scores[,1]))
  new_obs <- data.frame("Comp.1"=as.matrix(dd[i,-55])%*%pca$loadings[,1])
  correct=round(predict(mod.glm,newdata=new_obs,type="response"))==dd[i, "Class"]
  sum=sum+correct
}
(1-sum/n)*100

## kNN
## -----------------------------------------------------------

library(class)
library(MASS)

knn.LOOCV<-function(dd, k){
  wrong<-c()
  sum=0
  n<-dim(dd)[1]
  p<-dim(dd)[2]-1
  for (i in 1:n){
    test.class<-knn(dd[-i,1:p],dd[i,1:p], cl=dd[-i, "Class"], k=k)
    if(test.class==dd[i, "Class"]) sum=sum+1
    else{
      wrong<-c(wrong,i)
    }
  }
  cat("Classification errors in observations: ",wrong,"\n")
  return (sum/n)
}
for (i in 1:round(sqrt(n)/2)){
  loocv.err<-1-knn.LOOCV(dd, 2*i)
  cat ("k =",2*i-1, "LOOCV error:",loocv.err, "\n\n")
}


## -----------------------------------------------------------
color=colors[as.factor(dd$Class)]
color[c(1,5,6,10)]<-"red"

pca <- princomp(dd[,-55])
plot(pca$scores[,1:2],col=color, main="PCA")

## MCA
## -----------------------------------------------------------
dd <- dd.factor #Recuperem els factors d'abans

## -----------------------------------------------------------
library(ca)
MCA.res<-mjca(dd[,1:p], lambda="indicator")
plot(MCA.res$rowcoord[,1:2], col=colors[dd$Class], pch=20)


## -----------------------------------------------------------
# Percentatge de la inèrcia explicada per les dimensions del MCA
eigenvals<-MCA.res$sv * MCA.res$sv
i=1
percent=0
while(percent<0.8){
  percent=sum(eigenvals[1:i])/sum(eigenvals)
  i=i+1
}
i-1


## -----------------------------------------------------------
dd.mca <- as.data.frame(MCA.res$rowcoord[,1:33])
dd.mca$Class<-dd$Class

## GLM (regressió logística)
## ----warning=FALSE------------------------------------------
mod.glm<-glm(Class~., dd.mca, family="binomial")
step(mod.glm, trace=FALSE)


## ----warning=FALSE------------------------------------------
## GLM amb step
sum=0
for (i in 1:n){
  mod.glm<-glm(formula = Class ~ V1 + V6, family = "binomial", data = dd.mca[-i,])
  sum(diag(table(round(predict(mod.glm, type="response")), dd.mca[-i, "Class"])))/(n-1)
  correct=round(predict(mod.glm,newdata=dd.mca[i,],type="response"))==dd.mca[i, "Class"]
  sum=sum+correct
}
(1-sum/n)*100

## kNN
## Fent servir les 33 dimensions escollides
## -----------------------------------------------------------
knn.LOOCV<-function(dd, k){
  sum=0
  n<-dim(dd)[1]
  p<-dim(dd)[2]-1
  for (i in 1:n){
    test.class<-knn(dd[-i,1:p],dd[i,1:p], cl=dd[-i, "Class"], k=k)
    if(test.class==dd[i, "Class"]) sum=sum+1
  }
  return (sum/n)
}

## Escollint només les tres primeres dimensions (les del plot)
## -----------------------------------------------------------
for (i in 1:round(sqrt(n)/2)){
  loocv.err <- 1-knn.LOOCV(dd.mca, i)
  cat ("k =",2*i-1, "LOOCV error:",loocv.err, "\n")
}


## -----------------------------------------------------------
for (i in 1:round(sqrt(n)/2)){
  loocv.err<-1-knn.LOOCV(dd.mca[,c(1, 2,3, dim(dd.mca)[2])], i)
  cat ("k =",2*i-1, "LOOCV error:",loocv.err, "\n")
}

## CONCLUSIONS
## -----------------------------------------------------------
# Taula de resultats finals

results <- data.frame(Dades=c(rep("Categòriques", 2), rep("Numèriques", 4), rep("MCA", 3)),
                      Mètodes=c("Naive Bayes", "Random Forest", "GLM", "GLM + Step", "GLM + PCA", "kNN", "GLM + Step", "kNN (33 dims)", "kNN (3 dims)"),
                      Error = c("2.35%","2.35%","3.53%","1.17%","0%", "2.35%", "0.58%", "7.65%", "0.58%"))
require(knitr)
require(kableExtra)

kable(results, booktabs=T) %>%
  kable_styling(position = "center") %>%
  column_spec(1, bold=T) %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle")

