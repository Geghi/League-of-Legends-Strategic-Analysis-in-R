source("mr_cmroc.r")
data = read.csv("tabella.csv")
#data = read.csv("games.csv")
data[,12:26] <- NULL
data[,17:36] <- NULL
data[,22:26] <- NULL
data$gameId <- NULL
data$seasonId <- NULL
data$creationTime <- NULL

library(MASS)
data$winner <- data$winner - 1
lda=lda(winner~.,data=data)
lda.p=predict(lda)
lda.post=lda.p$posterior[,2]
sum((lda.post>0.5)==(data$winner>0.5))/length(data$winner)
mconfmat(data$winner,lda.post)
lda.roc=mroc(data$winner,lda.post)
mroc.plot(lda.roc)
mauc(lda.roc)

glm = glm(formula = winner~. , family = binomial,data = data)
glm.p=predict(glm,type="response")
sum((glm.p>0.5)==(data$winner>0.5))/length(data$winner)
mconfmat(data$winner,glm.p)
glm.roc=mroc(data$winner,glm.p)
mroc.plot(lda.roc,col="blue")
mroc.lines(glm.roc,col="green")
mauc(glm.roc)
legend(0.7, 0.2, legend=c("Regressione Logistica", "Analisi Discriminante Lineare"),col=c("green", "blue"), lty=1, cex=1)


data.dragon <- data[data$firstDragon!= 0,] 
data.dragon$firstDragon <- data.dragon$firstDragon - 1
glm = glm(formula = winner~firstDragon , family = binomial,data = data.dragon)
glm.p=predict(glm,type="response")
sum((glm.p>0.5)==(data.dragon$winner>0.5))/length(data.dragon$winner)
mconfmat(data.dragon$winner,glm.p)
glm.roc=mroc(data.dragon$winner,glm.p)
mroc.plot(glm.roc,col="blue")
mauc(glm.roc)

data.tower = data[data$firstTower!= 0,] 
data.tower$firstTower <- data.tower$firstTower - 1
glm = glm(formula = winner~firstTower , family = binomial,data = data.tower)
glm.p=predict(glm,type="response")
sum((glm.p>0.5)==(data.tower$winner>0.5))/length(data.tower$winner)
mconfmat(data.tower$winner,glm.p)
glm.roc=mroc(data.tower$winner,glm.p)
mroc.plot(glm.roc,col="blue")
mauc(glm.roc)

data.blood = data[data$firstBlood != 0,] 
data.blood$firstBlood <- data.blood$firstBlood - 1
glm = glm(formula = winner~firstBlood , family = binomial,data = data.blood)
glm.p=predict(glm,type="response")
sum((glm.p>0.5)==(data.blood$winner>0.5))/length(data.blood$winner)
mconfmat(data.blood$winner,glm.p)
glm.roc=mroc(data.blood$winner,glm.p)
mroc.plot(glm.roc,col="blue")
mauc(glm.roc)

data.inhib <- data[data$firstInhibitor!= 0,] 
data.inhib$firstInhibitor <- data.inhib$firstInhibitor - 1
glm = glm(formula = winner~firstInhibitor , family = binomial,data = data.inhib)
glm.p=predict(glm,type="response")
sum((glm.p>0.5)==(data.inhib$winner>0.5))/length(data.inhib$winner)
mconfmat(data.inhib$winner,glm.p)
glm.roc=mroc(data.inhib$winner,glm.p)
mroc.plot(glm.roc,col="blue")
mauc(glm.roc)


