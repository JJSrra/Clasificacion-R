## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
set.seed(17)

## ------------------------------------------------------------------------
wbcd = read.csv("Datos/wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
head(wbcd)

## ------------------------------------------------------------------------
wbcd = wbcd[,-1]
head(wbcd)

## ------------------------------------------------------------------------
table(wbcd$diagnosis)
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

## ------------------------------------------------------------------------
wbcd$diagnosis = factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant")) 
table(wbcd$diagnosis) 

## ------------------------------------------------------------------------
summary(wbcd[,c("radius_mean", "area_mean", "smoothness_mean")])

## ------------------------------------------------------------------------
wbcd_n = as.data.frame(lapply(wbcd[,2:31], scale, center = TRUE, scale = TRUE)) 

summary(wbcd_n[,c("radius_mean", "area_mean", "smoothness_mean")]) 
boxplot(wbcd_n[,c("radius_mean", "area_mean", "smoothness_mean")]) 

## ------------------------------------------------------------------------
plot(wbcd[,2:5]) 
plot(wbcd_n[,1:4], col=wbcd[,1]) 
cor(wbcd[,2:5]) 
cor(wbcd_n[,1:4]) 

## ------------------------------------------------------------------------
shuffle_ds = sample(dim(wbcd_n)[1]) 
eightypct = (dim(wbcd_n)[1] * 80) %/% 100 

wbcd_train = wbcd_n[shuffle_ds[1:eightypct], ] 
wbcd_test = wbcd_n[shuffle_ds[(eightypct+1):dim(wbcd_n)[1]], ] 

wbcd_train_labels = wbcd[shuffle_ds[1:eightypct], 1] 
wbcd_test_labels = wbcd[shuffle_ds[(eightypct+1):dim(wbcd_n)[1]], 1]

## ------------------------------------------------------------------------
library(class)
wbcd_test_pred = knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
table(wbcd_test_pred,wbcd_test_labels)

## ------------------------------------------------------------------------
require(caret)
knnModel = train(wbcd_train, wbcd_train_labels, method="knn",
           metric="Accuracy", tuneGrid = data.frame(.k=1:10)) 
knnModel

## ------------------------------------------------------------------------
mejoresK = order(knnModel$results$Accuracy, decreasing = TRUE)[1:3]
mejoresK

## ------------------------------------------------------------------------
knnModel.1 = train(wbcd_train, wbcd_train_labels, method="knn",
               metric="Accuracy", tuneGrid = data.frame(.k=mejoresK[1]))

knnModel.2 = train(wbcd_train, wbcd_train_labels, method="knn",
               metric="Accuracy", tuneGrid = data.frame(.k=mejoresK[2]))

knnModel.3 = train(wbcd_train, wbcd_train_labels, method="knn",
               metric="Accuracy", tuneGrid = data.frame(.k=mejoresK[3]))

## ------------------------------------------------------------------------
knnPred.1 = predict(knnModel.1, wbcd_test)
knnPred.2 = predict(knnModel.2, wbcd_test)
knnPred.3 = predict(knnModel.3, wbcd_test)

## ------------------------------------------------------------------------
postResample(knnPred.1, wbcd_test_labels)
postResample(knnPred.2, wbcd_test_labels)
postResample(knnPred.3, wbcd_test_labels)

## ------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(wbcd_test$texture_mean~wbcd_test$area_mean,col=knnPred.1, main="k-NN Modelo 1")
plot(wbcd_test$texture_mean~wbcd_test$area_mean,col=knnPred.2, main="k-NN Modelo 2")
plot(wbcd_test$texture_mean~wbcd_test$area_mean,col=knnPred.3, main="k-NN Modelo 3")
plot(wbcd_test$texture_mean~wbcd_test$area_mean,col=wbcd_test_labels, main="Originales Test")
par(mfrow=c(1,1))

## ------------------------------------------------------------------------
library(ISLR)
head(Smarket)

## ------------------------------------------------------------------------
glmFit = train(Smarket[,-8:-9], y=Smarket[,9], method="glm",
         preProcess=c("center", "scale"), tuneLength=10,
         control=glm.control(maxit=500), trControl=trainControl(method = "cv")) 
glmFit

## ------------------------------------------------------------------------
shapiro.test(Smarket$Lag1)
qqnorm(y=Smarket$Lag1)
qqline(y=Smarket$Lag1)

shapiro.test(Smarket$Lag2)
qqnorm(y=Smarket$Lag2)
qqline(y=Smarket$Lag2)

shapiro.test(Smarket$Lag3)
qqnorm(y=Smarket$Lag3)
qqline(y=Smarket$Lag3)

shapiro.test(Smarket$Lag4)
qqnorm(y=Smarket$Lag4)
qqline(y=Smarket$Lag4)

shapiro.test(Smarket$Lag5)
qqnorm(y=Smarket$Lag5)
qqline(y=Smarket$Lag5)

## ------------------------------------------------------------------------
var(Smarket$Lag1)
var(Smarket$Lag2)
var(Smarket$Lag3)
var(Smarket$Lag4)
var(Smarket$Lag5)

## ------------------------------------------------------------------------
boxplot(Smarket[2:6])

## ------------------------------------------------------------------------
require(MASS)
lda.fit <- lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket, subset=Year<2005)
lda.fit

## ------------------------------------------------------------------------

plot(lda.fit, type="both")

## ------------------------------------------------------------------------
Smarket.2005 = subset(Smarket,Year==2005) 
lda.pred = predict(lda.fit, Smarket.2005)

## ------------------------------------------------------------------------
table(lda.pred$class,Smarket.2005$Direction)
ldaPred = mean(lda.pred$class==Smarket.2005$Direction)
ldaPred

## ------------------------------------------------------------------------
library(klaR)
partimat(Direction~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket, method="lda")

## ------------------------------------------------------------------------
glmFit$results$Accuracy
ldaPred

## ------------------------------------------------------------------------
Smarket.up = Smarket[Smarket$Direction == "Up",]
Smarket.down = Smarket[Smarket$Direction == "Down",]

var(Smarket.up$Lag1)
var(Smarket.up$Lag2)
var(Smarket.up$Lag3)
var(Smarket.up$Lag4)
var(Smarket.up$Lag5)

var(Smarket.down$Lag1)
var(Smarket.down$Lag2)
var(Smarket.down$Lag3)
var(Smarket.down$Lag4)
var(Smarket.down$Lag5)

## ------------------------------------------------------------------------
qda.fit = qda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket, subset=Year<2005)
qda.fit

## ------------------------------------------------------------------------
qda.pred = predict(qda.fit, Smarket.2005)

table(qda.pred$class, Smarket.2005$Direction)
qdaPred = mean(qda.pred$class==Smarket.2005$Direction)
qdaPred

## ------------------------------------------------------------------------
partimat(Direction~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket, method="qda")

## ------------------------------------------------------------------------
alumnos = read.csv("Datos/clasif_train_alumnos.csv")
tabla.train = cbind(alumnos[,2:dim(alumnos)[2]])
colnames(tabla.train) = names(alumnos)[2:dim(alumnos)[2]]
rownames(tabla.train) = alumnos[,1]

## ------------------------------------------------------------------------
LDAvsQDA = wilcox.test(tabla.train$out_train_lda, tabla.train$out_train_qda,
           alternative="two.sided", paired=TRUE)
Rmas = LDAvsQDA$statistic
pvalue = LDAvsQDA$p.value
LDAvsQDA = wilcox.test(tabla.train$out_train_qda, tabla.train$out_train_lda,
              alternative="two.sided", paired=TRUE)
Rmenos = LDAvsQDA$statistic
Rmas
Rmenos
pvalue

## ------------------------------------------------------------------------
friedman.test(as.matrix(tabla.train))

## ------------------------------------------------------------------------
tam = dim(tabla.train)
groups = rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tabla.train), groups, p.adjust = "holm", paired = TRUE)

