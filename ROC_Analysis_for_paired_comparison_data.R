library(pROC)

par(mfrow=c(4,2))

#####################################
#MLB
MLB2017 <- read.csv("Downloads/MLB2017.csv")
team <- levels(factor(MLB2017$Home))
m <- length(team)
N <- nrow(MLB2017)
X <- data.frame(matrix(0,nrow=N,ncol=m))
colnames(X) <- c(team)
Y <- rep(0,N)
for (i in 1:N) {
  home <- MLB2017$Home[i]
  visitor <- MLB2017$Visitor[i]
  win <- as.numeric(MLB2017$Home.score[i]>MLB2017$Vis.score[i])
  X[i,] <- as.numeric(colnames(X)[1:m]==home)-as.numeric(colnames(X)[1:m]==visitor)
  Y[i] <- win
}
dat <- data.frame(X,Y)
fit.bt <- glm(Y~.-1,data=dat[,-m],family=binomial)
mu.hat.bt <- c(fit.bt$coef,0)
fit.tm <- glm(Y~.-1,data=dat[,-m],family=binomial(link="probit"))
mu.hat.tm <- c(fit.tm$coef,0)
P.bt <- as.vector(1/(1+exp(-as.matrix(X)%*%mu.hat.bt)))
#WL
X.wl.bt <- rbind(X*(2*Y-1), -X*(2*Y-1))
Y.wl.bt <- c(rep(1,nrow(X)),rep(0,nrow(X)))
P.wl.bt <- as.vector(1/(1+exp(-as.matrix(X.wl.bt)%*%mu.hat.bt)))
par(pty = "s")
plot(roc(Y.wl.bt, P.wl.bt), print.auc=TRUE, print.auc.pattern = "AUWLC:\n%.3f", print.auc.cex=1.2, legacy.axes=TRUE, main="MLB-17 WL-ROC Curve")

#SW
Y.bi.bt <- as.numeric(P.bt>0.5)
Y.sw.bt <- ((2*Y-1)*(2*Y.bi.bt-1)+1)/2
X.sw.bt <- X*(2*Y.bi.bt-1)
P.sw.bt <- as.vector(1/(1+exp(-as.matrix(X.sw.bt)%*%mu.hat.bt)))
plot(roc(Y.sw.bt,P.sw.bt), print.auc=TRUE, print.auc.pattern = "AUSWC:\n%.3f", print.auc.cex=1.2, legacy.axes=TRUE, main="MLB-17 SW-ROC Curve", asp=F)

#c-statistic
c_MLB <- c(auc(roc(Y.wl.bt, P.wl.bt)), auc(roc(Y.sw.bt,P.sw.bt)))


#####################################
#NHL
NHL <- read.csv("Downloads/NHL_Scores.csv")
NHL2017 <- NHL[NHL$Season%in%c(2017),]
team <- levels(factor(NHL2017$Home))
m <- length(team)
N <- nrow(NHL2017)
X <- data.frame(matrix(0,nrow=N,ncol=m))
colnames(X) <- c(team)
Y <- rep(0,N)
for (i in 1:N) {
  home <- NHL2017$Home[i]
  visitor <- NHL2017$Visitor[i]
  win <- as.numeric(NHL2017$Home_Goals[i]>NHL2017$Visitor_Goals[i])
  X[i,] <- as.numeric(colnames(X)[1:m]==home)-as.numeric(colnames(X)[1:m]==visitor)
  Y[i] <- win
}
dat <- data.frame(X,Y)
fit.bt <- glm(Y~.-1,data=dat[,-m],family=binomial)
mu.hat.bt <- c(fit.bt$coef,0)
P.bt <- as.vector(1/(1+exp(-as.matrix(X)%*%mu.hat.bt)))
#WL
X.wl.bt <- rbind(X*(2*Y-1), -X*(2*Y-1))
Y.wl.bt <- c(rep(1,nrow(X)),rep(0,nrow(X)))
P.wl.bt <- as.vector(1/(1+exp(-as.matrix(X.wl.bt)%*%mu.hat.bt)))
plot(roc(Y.wl.bt, P.wl.bt),print.auc=TRUE, print.auc.pattern = "AUWLC:\n%.3f",print.auc.cex=1.2, legacy.axes=TRUE, main="NHL-17-18 WL-ROC Curve", asp=F)

#SW
Y.bi.bt <- as.numeric(P.bt>0.5)
Y.sw.bt <- ((2*Y-1)*(2*Y.bi.bt-1)+1)/2
X.sw.bt <- X*(2*Y.bi.bt-1)
P.sw.bt <- as.vector(1/(1+exp(-as.matrix(X.sw.bt)%*%mu.hat.bt)))
plot(roc(Y.sw.bt,P.sw.bt),print.auc=TRUE, print.auc.pattern = "AUSWC:\n%.3f",print.auc.cex=1.2, legacy.axes=TRUE, main="NHL-17-18 SW-ROC Curve", asp=F)

#c-statistic
c_NHL <- c(auc(roc(Y.wl.bt, P.wl.bt)), auc(roc(Y.sw.bt,P.sw.bt)))



#####################################
#NBA
NBA <- read.csv("Downloads/NBA-PBP-2017-2018.csv")
NBA2017 <- NBA[NBA$GameType=="regular",c("URL", "GameType", "Date","Time","WinningTeam","AwayTeam","HomeTeam")]
NBA2017 <- unique(NBA2017)
team <- levels(factor(NBA2017$HomeTeam))[-1]
m <- length(team)
N <- nrow(NBA2017)
X <- data.frame(matrix(0,nrow=N,ncol=m))
colnames(X) <- c(team)
Y <- rep(0,N)
for (i in 1:N) {
  home <- NBA2017$HomeTeam[i]
  away <- NBA2017$AwayTeam[i]
  winner <- NBA2017$WinningTeam[i]
  win <- as.numeric(winner==home)
  X[i,] <- as.numeric(colnames(X)[1:m]==home)-as.numeric(colnames(X)[1:m]==away)
  Y[i] <- win
}
dat <- data.frame(X,Y)
fit.bt <- glm(Y~.-1,data=dat[,-m],family=binomial)
mu.hat.bt <- c(fit.bt$coef,0)
fit.tm <- glm(Y~.-1,data=dat[,-m],family=binomial(link="probit"))
mu.hat.tm <- c(fit.tm$coef,0)
P.bt <- as.vector(1/(1+exp(-as.matrix(X)%*%mu.hat.bt)))
#WL
X.wl.bt <- rbind(X*(2*Y-1), -X*(2*Y-1))
Y.wl.bt <- c(rep(1,nrow(X)),rep(0,nrow(X)))
P.wl.bt <- as.vector(1/(1+exp(-as.matrix(X.wl.bt)%*%mu.hat.bt)))
plot(roc(Y.wl.bt, P.wl.bt),print.auc=TRUE, print.auc.pattern = "AUWLC:\n%.3f",print.auc.cex=1.2, legacy.axes=TRUE, main="NBA-17-18 WL-ROC Curve", asp=F)

#SW
Y.bi.bt <- as.numeric(P.bt>0.5)
Y.sw.bt <- ((2*Y-1)*(2*Y.bi.bt-1)+1)/2
X.sw.bt <- X*(2*Y.bi.bt-1)
P.sw.bt <- as.vector(1/(1+exp(-as.matrix(X.sw.bt)%*%mu.hat.bt)))
plot(roc(Y.sw.bt,P.sw.bt),print.auc=TRUE, print.auc.pattern = "AUSWC:\n%.3f",print.auc.cex=1.2, legacy.axes=TRUE, main="NBA-17-18 SW-ROC Curve", asp=F)

#c-statistic
c_NBA <- c(auc(roc(Y.wl.bt, P.wl.bt)), auc(roc(Y.sw.bt,P.sw.bt)))

#####################################
#NFL
NFL <- read.csv("Downloads/NFL_Scores.csv")
NFL2017 <- NFL[NFL$schedule_season%in%c(2017)&NFL$schedule_week%in%seq(1,17),c("team_home","team_away","score_home","score_away")]
team <- levels(factor(NFL2017$team_home))
m <- length(team)
N <- nrow(NFL2017)
X <- data.frame(matrix(0,nrow=N,ncol=m))
colnames(X) <- c(team)
Y <- rep(0,N)
for (i in 1:N) {
  home <- NFL2017$team_home[i]
  visitor <- NFL2017$team_away[i]
  win <- as.numeric(NFL2017$score_home[i]>NFL2017$score_away[i])
  X[i,] <- as.numeric(colnames(X)[1:m]==home)-as.numeric(colnames(X)[1:m]==visitor)
  Y[i] <- win
}
dat <- data.frame(X,Y)
fit.bt <- glm(Y~.-1,data=dat[,-m],family=binomial)
mu.hat.bt <- c(fit.bt$coef,0)
P.bt <- as.vector(1/(1+exp(-as.matrix(X)%*%mu.hat.bt)))
#WL
X.wl.bt <- rbind(X*(2*Y-1), -X*(2*Y-1))
Y.wl.bt <- c(rep(1,nrow(X)),rep(0,nrow(X)))
P.wl.bt <- as.vector(1/(1+exp(-as.matrix(X.wl.bt)%*%mu.hat.bt)))
plot(roc(Y.wl.bt, P.wl.bt),print.auc=TRUE, print.auc.pattern = "AUWLC:\n%.3f", print.auc.cex=1.2, legacy.axes=TRUE, main="NFL-17 WL-ROC Curve", asp=F)

#SW
Y.bi.bt <- as.numeric(P.bt>0.5)
Y.sw.bt <- ((2*Y-1)*(2*Y.bi.bt-1)+1)/2
X.sw.bt <- X*(2*Y.bi.bt-1)
P.sw.bt <- as.vector(1/(1+exp(-as.matrix(X.sw.bt)%*%mu.hat.bt)))
plot(roc(Y.sw.bt,P.sw.bt),print.auc=TRUE, print.auc.pattern = "AUSWC:\n%.3f", print.auc.cex=1.2, legacy.axes=TRUE, main="NFL-17 SW-ROC Curve", asp=F)

#c-statistic
c_NFL <- c(auc(roc(Y.wl.bt, P.wl.bt)), auc(roc(Y.sw.bt,P.sw.bt)))

