serie=ts(read.table("airmad.csv",header=F)/1000,
         start=1990,
         end=c(2019,12),
         freq=12)
plot(serie)


lnserie=log(serie)
plot(lnserie)

d12lnserie=diff(lnserie,12)
plot(d12lnserie)

d1d12lnserie=diff(diff(lnserie,12))
plot(d1d12lnserie)

par(mfrow=c(1,2))
acf(d1d12lnserie,ylim=c(-1,1),lag.max=72,col=c(2,rep(1,11)),lwd=2)
pacf(d1d12lnserie,ylim=c(-1,1),lag.max=72,col=c(rep(1,11),2),lwd=2)
par(mfrow=c(1,1))

(mod=arima(d1d12lnserie,order=c(0,0,1),
           seasonal=list(order=c(2,0,0),period=12)))

(mod=arima(lnserie,order=c(0,1,1),
           seasonal=list(order=c(2,1,0),period=12)))

resi=resid(mod)

plot(resi)
abline(h=0)
abline(h=c(-3*sd(resi),3*sd(resi)),lty=3,col=4)

scatter.smooth(sqrt(abs(resi)), lpars=list(col=2))

qqnorm(resi)
qqline(resi,col=2,lwd=2)

hist(resi,breaks=20, freq=FALSE)
curve(dnorm(x, mean=mean(resi), sd=sd(resi)), col=2, lwd=2, add=T)

par(mfrow=c(1,2))
acf(resi,ylim=c(-1,1),lag.max=72,col=c(2,rep(1,11)),lwd=2)
pacf(resi,ylim=c(-1,1),lag.max=72,col=c(rep(1,11),2),lwd=2)
par(mfrow=c(1,1))

tsdiag(mod,gof.lag=72)

Mod(polyroot(c(1,-mod$model$phi)))
Mod(polyroot(c(1,mod$model$theta)))

##Pesos Pi (AR infinit)
(pis=-ARMAtoMA(ar=-mod$model$theta,ma=-mod$model$phi,lag.max=36))

##Pesos Psi (MA infinit)
(psis=ARMAtoMA(ar=mod$model$phi,ma=mod$model$theta,lag.max=36))

(AIC1=AIC(mod))
(BIC1=BIC(mod))

ultim=c(2018,12)
serie2=window(serie, end=ultim)
lnserie2=log(serie2)

mod
(mod2=arima(lnserie2,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=12)))

pre=predict(mod2,n.ahead = 12)

ll=exp(pre$pred-1.96*pre$se)
pr=exp(pre$pred)
ul=exp(pre$pred+1.96*pre$se)

ts.plot(serie,ll,ul,pr,
        lty=c(1,2,2,1),
        col=c(1,4,4,2),
        xlim=c(2016,2020), type="o")
abline(v=2016:2020,lty=3,col=4)

obs=window(serie,start=2019)
(RMSE1=sqrt(mean((obs-pr)^2)))
(MAE1=mean(abs(obs-pr)))
(RMSPE1=sqrt(mean(((obs-pr)/obs)^2)))
(MAPE1=mean(abs(obs-pr)/obs))

(CI1=mean(ul-ll))

##### Calendar Effects

source("CalendarEffects.r")


inici=c(1990,1,length(serie))
vEa=Weaster(inici)
vTD=Wtrad(inici)

(mod1=arima(lnserie,order=c(0,1,1),
            seasonal=list(order=c(2,1,0),period=12)))
(mod1Ea=arima(lnserie,order=c(0,1,1),
            seasonal=list(order=c(2,1,0),period=12),
            xreg=data.frame(vEa)))
(mod1TD=arima(lnserie,order=c(0,1,1),
              seasonal=list(order=c(2,1,0),period=12),
              xreg=data.frame(vTD)))
(mod1EC=arima(lnserie,order=c(0,1,1),
              seasonal=list(order=c(2,1,0),period=12),
              xreg=data.frame(vEa,vTD)))

## Intervention Analysis (Terminal T4 and Subway to city)

vT4=ts(rep(0,length(serie)),start=1990,freq=12)
window(vT4,start=c(2006,2))<-1

vMetro=ts(rep(0,length(serie)),start=1990,freq=12)
window(vMetro,start=c(2002,1))<-1

(mod1ECIA=arima(lnserie,order=c(0,1,1),
              seasonal=list(order=c(2,1,0),period=12),
              xreg=data.frame(vEa,vTD,vT4,vMetro)))


exp(coef(mod1EC)["vEa"])
exp(coef(mod1EC)["vTD"])


lnserieEC=lnserie-
          coef(mod1EC)["vEa"]*vEa-
          coef(mod1EC)["vTD"]*vTD

exp(cbind(lnserie,lnserieEC))

d1d12lnserieEC=diff(diff(lnserieEC,12))

par(mfrow=c(1,2))
acf(d1d12lnserieEC,ylim=c(-1,1),lag.max=72,col=c(2,rep(1,11)),lwd=2)
pacf(d1d12lnserieEC,ylim=c(-1,1),lag.max=72,col=c(rep(1,11),2),lwd=2)
par(mfrow=c(1,1))

(modEC=arima(lnserie,order=c(0,1,2),
              seasonal=list(order=c(0,1,1),period=12),
              xreg=data.frame(vEa,vTD)))

resi=resid(modEC)

plot(resi)
abline(h=0)
abline(h=c(-3*sd(resi),3*sd(resi)),lty=3,col=4)

scatter.smooth(sqrt(abs(resi)), lpars=list(col=2))

qqnorm(resi)
qqline(resi,col=2,lwd=2)

hist(resi,breaks=20, freq=FALSE)
curve(dnorm(x, mean=mean(resi), sd=sd(resi)), col=2, lwd=2, add=T)

par(mfrow=c(1,2))
acf(resi,ylim=c(-1,1),lag.max=72,col=c(2,rep(1,11)),lwd=2)
pacf(resi,ylim=c(-1,1),lag.max=72,col=c(rep(1,11),2),lwd=2)
par(mfrow=c(1,1))

tsdiag(modEC,gof.lag=72)

Mod(polyroot(c(1,-modEC$model$phi)))
Mod(polyroot(c(1,modEC$model$theta)))


(AIC2=AIC(modEC))
(BIC2=BIC(modEC))

ultim=c(2018,12)
serie2=window(serie, end=ultim)
lnserie2=log(serie2)
vEa2=window(vEa,end=ultim)
vTD2=window(vTD,end=ultim)

modEC
(modEC2=arima(lnserie2,order=c(0,1,2),
            seasonal=list(order=c(0,1,1),period=12),
            xreg=data.frame(vEa2,vTD2)))

vEa2=window(vEa,start=ultim+c(0,1))
vTD2=window(vTD,start=ultim+c(0,1))

pre=predict(modEC2,n.ahead = 12,newxreg=data.frame(vEa2,vTD2))

ll=exp(pre$pred-1.96*pre$se)
pr=exp(pre$pred)
ul=exp(pre$pred+1.96*pre$se)

ts.plot(serie,ll,ul,pr,
        lty=c(1,2,2,1),
        col=c(1,4,4,2),
        xlim=c(2016,2020), type="o")
abline(v=2016:2020,lty=3,col=4)

obs=window(serie,start=2019)
(RMSE2=sqrt(mean((obs-pr)^2)))
(MAE2=mean(abs(obs-pr)))
(RMSPE2=sqrt(mean(((obs-pr)/obs)^2)))
(MAPE2=mean(abs(obs-pr)/obs))

(CI2=mean(ul-ll))


### Outlier Treatment

source("atipics2.r")

mod.atip=outdetec(modEC,dif=c(1,12),crit=2.8,LS=T)

atipics=mod.atip$atip[order(mod.atip$atip[,1]),]
meses=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
data.frame(atipics,
           Fecha=paste(meses[(atipics[,1]-1)%%12+1],start(lnserie)[1]+((atipics[,1]-1)%/%12)),
           PercVar=exp(atipics[,3])*100)

lnserie.lin=lineal(lnserie,mod.atip$atip)
serie.lin=exp(lnserie.lin)

plot(serie)
lines(exp(lnserie.lin),col=2)

plot(lnserie-lnserie.lin)

lnserieEC.lin=lnserie.lin-
  coef(modEC)["vEa"]*vEa-
  coef(modEC)["vTD"]*vTD

d1d12lnserieEC.lin=diff(diff(lnserieEC.lin,12))
plot(d1d12lnserieEC.lin)
abline(h=0)

par(mfrow=c(1,2))
acf(d1d12lnserieEC.lin,ylim=c(-1,1),lag.max=72,col=c(2,rep(1,11)),lwd=2)
pacf(d1d12lnserieEC.lin,ylim=c(-1,1),lag.max=72,col=c(rep(1,11),2),lwd=2)
par(mfrow=c(1,1))

(modEClin=arima(lnserie.lin,order=c(0,1,3),
              seasonal=list(order=c(0,1,1),period=12),
              xreg=data.frame(vEa,vTD)))


resi=resid(modEClin)

plot(resi)
abline(h=0)
abline(h=c(-3*sd(resi),3*sd(resi)),lty=3,col=4)

scatter.smooth(sqrt(abs(resi)), lpars=list(col=2))

qqnorm(resi)
qqline(resi,col=2,lwd=2)

hist(resi,breaks=15, freq=FALSE)
curve(dnorm(x, mean=mean(resi), sd=sd(resi)), col=2, lwd=2, add=T)

par(mfrow=c(1,2))
acf(resi,ylim=c(-1,1),lag.max=72,col=c(2,rep(1,11)),lwd=2)
pacf(resi,ylim=c(-1,1),lag.max=72,col=c(rep(1,11),2),lwd=2)
par(mfrow=c(1,1))

tsdiag(modEClin,gof.lag=72)

(AIC3=AIC(modEClin)+2*nrow(mod.atip$atip))
(BIC3=BIC(modEClin)+log(length(serie)**nrow(mod.atip$atip)))

Mod(polyroot(c(1,-modEClin$model$phi)))
Mod(polyroot(c(1,modEClin$model$theta)))

ultim=c(2018,12)
serie.lin2=window(serie.lin, end=ultim)
lnserie.lin2=log(serie.lin2)
vEa2=window(vEa,end=ultim)
vTD2=window(vTD,end=ultim)

modEClin
(modEClin2=arima(lnserie.lin2,order=c(0,1,3),
              seasonal=list(order=c(0,1,1),period=12),
              xreg=data.frame(vEa2,vTD2)))

vEa2=window(vEa,start=ultim+c(0,1))
vTD2=window(vTD,start=ultim+c(0,1))

pre=predict(modEClin2,n.ahead = 12,newxreg=data.frame(vEa2,vTD2))

wLS=sum(mod.atip$atip[mod.atip$atip[,2]=="LS",3])

ll=exp(pre$pred+wLS-1.96*pre$se)
pr=exp(pre$pred+wLS)
ul=exp(pre$pred+wLS+1.96*pre$se)

ts.plot(serie,ll,ul,pr,
        lty=c(1,2,2,1),
        col=c(1,4,4,2),
        xlim=c(2016,2020), type="o")
abline(v=2016:2020,lty=3,col=4)

obs=window(serie,start=2019)
(RMSE3=sqrt(mean((obs-pr)^2)))
(MAE3=mean(abs(obs-pr)))
(RMSPE3=sqrt(mean(((obs-pr)/obs)^2)))
(MAPE3=mean(abs(obs-pr)/obs))

(CI3=mean(ul-ll))


res=data.frame(
        par=c(length(coef(mod)),length(coef(modEC)),length(coef(modEClin))+nrow(mod.atip$atip)),
        sigma2=c(mod$sigma2,modEC$sigma2,modEClin$sigma2),
        AIC=c(AIC1,AIC2,AIC3),
        BIC=c(BIC1,BIC2,BIC3),
        RMSE=c(RMSE1,RMSE2,RMSE3),
        MAE=c(MAE1,MAE2,MAE3),
        RMSPE=c(RMSPE1,RMSPE2,RMSPE3),
        MAPE=c(MAPE1,MAPE2,MAPE3),
        CIml=c(CI1,CI2,CI3)
        )
row.names(res)=c("ARIMA","ARIMA+EC","ARIMA+EC+OutTreat")
res
