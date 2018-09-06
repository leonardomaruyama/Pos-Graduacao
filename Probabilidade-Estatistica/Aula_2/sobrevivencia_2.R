require(stats4)
require(survival)
library(maxLik)
########### DADOS CHUVA PRESIDENTE PRUDENTE ########


dwg<-function(t,c,theta)
theta^(c+1)/((theta+c)*gamma(c))*t^(c-1)*(1+t)*exp(-theta*t)

###### FUNÇÂO ACUMULADA #########

pwg<-function(t,c,theta){
p<-(theta/(c+theta))
den<-p*pgamma(t,shape=c,rate=theta)+(1-p)*pgamma(t,shape=c+1,rate=theta)
return(den)
}

igamma<-function(a,b){
(pgamma(a,b, lower=FALSE)*gamma(b))
}

tttplot<-function(t)
{
t <- sort(t)
n <- length(t)
aux <- matrix(0, n, 2)
S <- sum(t)
for(i in 1:n)
{
aux[i, 1] <- i/n
aux[i, 2] <- (sum(t[1:i]) + (n - i) * t[i])/S
}
return(aux)
}

### Log like gamma #

ll <- function(theta) { 
 fi<-theta[1]
 mi<-theta[2]
 alfa<-1
 l<- d*log(alfa)+(d*alfa*fi)*log(mi)-n*lgamma(fi)+(alfa*fi-1)*sum(delta*log(t))-(mi^alfa)*sum(delta*(t^alfa))+sum((1-delta)*log(pgamma((mi*t)^alfa,fi, lower=FALSE)*gamma(fi)))
 return(l)
 }

###################### Log da Verossimilhança##################
loglike<-function(theta) {
c<-theta[1]
lam<-theta[2]
aux<-d*(c+1)*log(lam)-n*log(lam+c)-n*lgamma(c)+sum((1-delta)*log((lam+c)*igamma(lam*t,c)+(((lam*t)^c)*exp(-lam*t))))+sum(delta*(c-1)*log(t))+sum(delta*log(1+t))-lam*sum(delta*t)
return(aux)
}


t<-c(20,8,19,23,1,12,22,3,20,31,39,27,15,8,3,4,26,3,38,41,25,20,32,104,84,2,8,24,91,68,20,8,9,53,72,36,145,104,33,34,42,5,79,14,38,38,24,64,89,1,89,80,12,5,16,57,33,1,2,26,10,6,3,10,55,154,75,34,91,3,41,55,73,3,31,7,75,59,106,4,2,1,30,77,147,49,9,37,8,36,12,1,20,58,12,106,91,62,30,147,14,47,6,145,17,21,47,10,183)

delta<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0)

length(t)
length(delta)

n<-length(t)
d<-sum(delta)

#Ajustando K-M
ekm<-survfit(Surv(t,delta)~1)
time<-ekm$time
st<-ekm$surv

#Ajustando WL
res  <- try(maxLik(loglike, start=c(0.7,0.5),iterlim = 100))
cau<-coef(res)[1]
tau<-coef(res)[2]

#Ajustando Weibull
ajust1 <- survreg(Surv(t,delta)~1,dist='weibull')
alpha <- exp(ajust1$coefficients[1])
gamma <- 1/ajust1$scale


#Ajustando Lognormal
ajust2 <- survreg(Surv(t,delta)~1,dist='lognormal')
mlog<-ajust2$coefficients[1]
slog<-ajust2$scale

#Ajustando Logistic
ajust3 <- survreg(Surv(t,delta)~1,dist='logistic')
mlogistic<-ajust3$coefficients[1]
slogistic<-ajust3$scale


#Ajustando Gamma
res2  <- try(maxLik(ll, start=c(0.5,2),iterlim = 20))
cfi<-coef(res2)[1]
cmi<-coef(res2)[2]

#time[length(time)+1]<-max(time)

std  <- exp(-(time/alpha)^gamma)
stw<- 1-pwg(time, c=cau, theta=tau)
stl<-1-plnorm(time, meanlog = mlog, sdlog =  slog)
stg<-1-pgamma(time,shape=cfi,rate=cmi)
stlogi<-1-plogis(time, location = mlogistic, scale = slogistic)

X<-seq(0.01,max(time),0.5)

#H1<- dlnorm(X, meanlog = mlog, sdlog =  slog)/(1-plnorm(X, meanlog = mlog, sdlog =  slog))
H1<- (gamma/alpha)*((X/alpha)^(gamma-1))        ###Weibull
ttt <- tttplot(t)
tline<-seq(0,1,0.01)
ttt[1,1]<-0;ttt[1,2]<-0;
par(mfrow=c(1,3))
par(mai=c(0.85,0.8,0.2, 0.1))
plot(ttt[,1],ttt[,2],xlim=c(0,1),ylim=c(0,1),xlab="r/n",ylab="G(r/n)",type="l",col="1",lwd=1,lty=1,main=NULL)
points(tline,tline,type="l",col="1",lwd=1,lty=2)
plot(ekm, conf.int=F, xlim=c(min(t),max(t)),ylim=c(0,1.1), xlab="Tempo", ylab="S(t)", col="1", lty=1,lwd=1)
lines(c(0,time),c(1,stl), col="2", lty=1,lwd=1)
lines(c(0,time),c(1,std), col="3", lty=1,lwd=1)
lines(c(0,time),c(1,stg), col="4", lty=1,lwd=1)
lines(c(0,time),c(1,stlogi), col="5", lty=1,lwd=1)
legend("topright",lty=c(1,1,1,1,1),c("Kaplan-Meier","Lognormal", "Weibull", "Gamma", "Logistica"), bty="n",col = c(1,2,3,4,5),cex=1.4)
plot(X,H1,xlim=c(0,max(t)),ylim=c(min(H1),max(H1)),xlab="Tempos",ylab="h(t)",type="l",col="1",lwd=1,lty=1,main=NULL)

pe<-c(cau, tau)
est2<-c(cfi,cmi)

############################ DISCRIMINATION TEST ############################
k<-2
round(-2*ajust1$loglik[1]+4,3)                             #AIC Weibull
round(-2*ll(est2)+4,3)                                     #AIC GAMMA
round(-2*ajust2$loglik[1]+4,3)                             #AIC Lognormal
round(-2*ajust3$loglik[1]+4,3)                             #AIC Loglogistic

   
estimativas<-c(alpha,gamma)
round(estimativas,5)

qweibull(0.25, shape=gamma, scale = alpha, lower.tail = TRUE, log.p = FALSE)