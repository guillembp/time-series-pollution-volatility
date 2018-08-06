
rm( list=ls() )

library(moments)
library(tseries)
library(xts)


wd='/home/juan/Desktop/Financial_Econometrics/Project_DS/'
setwd(wd)

#read the data ----

pollution<- read.table('NY_pollution.csv',header = TRUE,
                       sep=',')
pollution$Date_Local<-as.Date(pollution$Date_Local,format="%Y-%m-%d")

#subset the data ----

pollution<-pollution[pollution$Date_Local>="2014-01-01",]

# convert to dialy data ----
ts.data <- xts(pollution$CO_Mean,
               order.by = pollution$Date_Local)

# hourly_ <- to.daily(ts.data)
hourly_<-period.apply(ts.data,endpoints(ts.data,"days"),max)
ind=index(hourly_)
values=as.vector(hourly_)
dialy_poll<-data.frame(y=values,date=ind)
dates_poll <- dialy_poll$date
CO_units_ <- dialy_poll$y
T=length(dates_poll)

CO_units_returns <- diff( log(CO_units_) )*100

CO_units_returns[is.na(CO_units_returns) ] <- 0
CO_units_returns[is.infinite(CO_units_returns)] <- 0
CO_units_returns[is.nan(CO_units_returns)] <- 0


CO_units_grw <- 4 * diff( log(CO_units_) ) * 100
CO_units_grw[is.na(CO_units_grw) ] <- 0
CO_units_grw[is.infinite(CO_units_grw)] <- 0
CO_units_grw[is.nan(CO_units_grw)] <- 0


#in / out sample

cut="2016-04-15"
in.sample <- CO_units_[dates_poll<cut]
out.sample <- CO_units_[dates_poll>=cut]
length(in.sample)+length(out.sample)==dim(dialy_poll)[1]


#normality ----

box_test <- Box.test(in.sample,lag = 22, type="Ljung-Box")

#auto correlation ----

#CO_units
acf_ <- acf(in.sample,lag.max = 120,col='darkorange2',tck=0.02,lwd=3)
pacf <- pacf(in.sample,lag.max = 120,col='darkorange2',tck=0.02,lwd=3)

#estimation ----

model_1_1 <- arima(in.sample,order = c(1,0,1))
model_2_1 <- arima(in.sample,order = c(2,0,1))
model_3_1 <- arima(in.sample,order = c(3,0,1))
model_ar_1 <- arima(in.sample,order = c(1,0,0))
model_ar_2 <- arima(in.sample,order = c(2,0,0))


#model selection

model_selection <- function(models,T){
  results = c(NA,NA,NA)
  for (element in models){
    loglik=element$loglik
    aic=(-2*element$loglik+2*3)/T
    bic=(-2*element$loglik+log(T)*3)/T
    results=rbind(results,c(loglik,aic,bic))
  }
  return(results)
}

list_models=list(ARMA11=model_1_1,
                 ARMA21=model_2_1,
                 ARMA31=model_3_1,
                 AR1=model_ar_1,
                 AR2=model_ar_2)

model_selection(models = list_models,T=length(in.sample))


#residuals
i=1
for (element in list_models){
  
  y=in.sample
  
  sigma <- y-element$residuals
  sigma_r <- as.numeric(element$residuals)
  jb_test <- jarque.test(sigma_r)$p.value
  box_test<-Box.test(sigma_r,lag=22,type="Ljung-Box" )$p.value
  
  #plots 
  par(mar=c(3,2,3,2) , mfrow=c(3,2))
  
  #fitted values plot
  plot( y , t='p' , pch=16, col='red' , tck = 0.02, xlim=c(1,length(in.sample)))
  lines( sigma , t='l' , lwd=1.5 , col='blue4',ylab='',xlab='')
  grid( lwd=1 , col="darkgrey" )
  
  #residuals plot
  plot( sigma_r , col='grey55' ,ylab='',xlab='')
  abline( h=0 , lwd=2 )
  grid( lwd=1 , col="darkgrey" )
  
  #autocorrelations plot
  acf( sigma_r , ylim=c(-0.2,1) , lwd=3, col='blue2' , tck=0.02)
  legend('topright',c('ACF'),col=c('blue2'),lwd=1)
  pacf( sigma_r , ylim=c(-0.2,1) , lwd=3, col='blue2' , tck=0.02)
  legend('topright',c('PACF'),col=c('blue2'),lwd=1) 
  
  #normality and QQ plot
  kernel <- density(sigma_r/sqrt(element$sigma2))
  plot(kernel,main="density")
  polygon( kernel , col="darkslategray1" , border='darkslategrey')
  abline(h=0,lwd=2)
  lines( seq(-10,20,0.1) , dnorm( seq(-10,20,0.1) ) , col='darkblue' ,lwd=2 )
  qqnorm(sigma_r,col='firebrick3')
  qqline(sigma_r,lwd=2,lty=3)
  
  title(main=paste0(names(list_models)[i],paste0("- Jarque Bera Test= ",round(jb_test,2),
                                                 "-","Ljung-Box= ",round(box_test,2))),outer = TRUE)
  
  i=i+1
}

#Forecasting

for (element in list_models){
  y=in.sample
  N=length(in.sample)
  H=length(out.sample)
  
  sigma <- y-element$residuals
  sigma_r <- as.numeric(element$residuals)
  
  prediction <- predict(element,n.ahead = H)
  mse_ <- mean((out.sample - as.numeric(prediction$pred))**2)
  plot( c((N-10):(N+H)) , c(y[(N-10):N], out.sample), 
        main=sprintf('AR(2) MSE %3.3f',mse_) , 
        ylim=c(min(y),max(y)) , ylab='',xlab='', 
        tck = 0.02 , pch=16 , col='darkorange') 
  abline( v=N , lwd=2 )
  abline( h=element$coef['intercept'] , lwd=2 )
  grid( lwd=1 , col="darkgrey" )
  lines( c((N-10):N) , sigma[(N-10):N] , t='l' , lwd=2 , col='blue3' )
  lines( c((N+1):(N+H)) , as.numeric(prediction$pred)  , t='b' , lwd=2 , col='blue3' )
}





