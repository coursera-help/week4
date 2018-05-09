library(fpp)
library(forecast)
library(devtools)
library(tseries)
library(TTR)

plot.ts(dowjones)
class(dowjones)
head(dowjones)
View(dowjones)

fit<-rwf(dowjones, drif=TRUE, h=20)
plot(fit)
res<-residuals(fit)
plot(res)
hist(res)
Acf(res)
Box.test(res, lag=10, fitdf=0, type="Lj")

meanFit<-meanf(dowjones, h=20)
plot(fit)

naiveFit<-naive(dowjones, h=20)
plot(naiveFit)

seasFit<-snaive(dowjones, h=20)
plot(seasFit)



########### Dataset usdeaths ###########
plot.ts(usdeaths)
death<-decompose(usdeaths)
plot(death)

deaths<-aggregate(usdeaths)
plot.ts(deaths)


fit<-rwf(deaths, drift=TRUE, h=10)
plot(fit)
accuracy(fit)
plot.ts(fit$residuals)
hist((residuals(fit)))
Box.test(fit, lag=8, type="Lj")


fit1<-naive(deaths, h=10)
plot(fit1)
accuracy(fit1)

fit2<-meanf(deaths, h=10)
plot(fit2)
accuracy(fit2)





plot.ts(bricksq) ##upward trend, seasonal
plot.ts(ibmclose) ##downwardtrend seasonal
plot.ts(hsales)   ##cyclic

fit_1<-snaive(bricksq)
plot.ts(fit_1$residuals)
hist(residuals(fit_1))
res<-residuals(fit_1)
Acf(res)
Box.test(res, lag=8, type="Lj") ###Not a white noise

fit_2<-snaive(ibmclose)
plot.ts(fit_2$residuals)
hist(residuals(fit_2))
res<-residuals(fit_2)
Acf(res)
Box.test(res, lag=8, type="Lj") ###white noise


fit_3<-snaive(hsales)
plot.ts(fit_3$residuals)
hist(residuals(fit_3))
res<-residuals(fit_3)
Acf(res)
Box.test(res, lag=8, type="Lj")


######## dataset bricksq ######
plot.ts(bricksq)


bricktrain<-window(bricksq, end=1987.99)
brickTest<-window(bricksq, start=1988)

plot(bricksq)
lines(bricktrain, col="red")
lines(brickTest, col="blue")

brick_fit1<-meanf(bricktrain)
accuracy(brick_fit1, brickTest)

brick_fit2<-naive(bricktrain)
accuracy(brick_fit2, brickTest)

brick_fit3<-snaive(bricktrain)
accuracy(brick_fit3, brickTest)

brick_fit4<-rwf(bricktrain, drift=TRUE) ## Best method
accuracy(brick_fit4, brickTest)
res_brick<-residuals(brick_fit4)
Acf(res_brick)
hist(res_brick, breaks = "FD")
Box.test(res_brick, lag=8, fitdf=0, type="Lj") ##Not white noise


####### IBM Close dataset ########
plot.ts(ibmclose)
ibmclose
ibmclose.train=window(ibmclose, start=1, end=300)
ibmclose.test=window(ibmclose, start=301)

plot.ts(ibmclose)
lines(ibmclose.train, col="red")
lines(ibmclose.test, col="blue")

ibmclose_fit1<-meanf(ibmclose.train)
accuracy(ibmclose_fit1, ibmclose.test)

ibmclose_fit2<-naive(ibmclose.train) ## Best method
accuracy(ibmclose_fit2, ibmclose.test)
res_ibm<-residuals(ibmclose_fit2)
Acf(res_ibm)
hist(res_ibm, breaks="FD")
Box.test(res_ibm, lag=8, fitdf=0, type="Lj") ##Not white noise

ibmclose_fit3<-snaive(ibmclose.train)
accuracy(ibmclose_fit3, ibmclose.test)

ibmclose_fit4<-rwf(ibmclose.train, drift=TRUE)
accuracy(ibmclose_fit4, ibmclose.test)



############# Simple Exponential Smoothening ############
plot.ts(oil)
fit_oil<-ses(oil, initial="simple", h=3)
fit_oil
summary(fit_oil)
fit_oil$model
accuracy(fit_oil) ## Best Method
hist(residuals(fit_oil))
checkresiduals(fit_oil)
Box.test(residuals(fit_oil), lag=10, fitdf = 0, type="Lj") ###White noise

fit_oil1<-ses(oil, alpha=0.8, initial="simple", h=3)
fit_oil1
fit_oil1$model
accuracy(fit_oil1)


### dataset: a10
a10
monthplot(a10)
plot.ts(a10)
agg10<-aggregate(a10)
agg10.train<-window(agg10, end=1999.99)
agg10.test<-window(agg10, start=2000)

fit_a10<-ses(agg10.train, initial="simple", h=10)
fit_a10$model
accuracy(fit_a10)
checkresiduals(fit_a10)
Box.test(residuals(fit_a10), lag=8, fitdf = 0, type="Lj") ##Not white noise

fit1_a10<-ses(agg10.train, alpha=0.2, initial="simple", h=10)
accuracy(fit1_a10)



##### Holts Method ##########
##dataset:strikes
eggs
plot.ts(eggs)
length(eggs)

##Additive Model
cat("\014")
holt_fitA<-holt(eggs, alpha=0.8, beta=0.2, initial="simple", h=8)
accuracy(holt_fitA)

holt_fitA1<-holt(eggs, initial="simple", h=8)
accuracy(holt_fitA1)
holt_fitA1$model
checkresiduals(holt_fitA1)
Box.test(residuals(holt_fitA1), lag=10, fitdf = 0, type="Lj") ###white noise

##Exponential/Multiplicative Model
cat("\014")
holt_fitM<-holt(eggs, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=8)
accuracy(holt_fitM)

holt_fitM1<-holt(eggs, initial="simple", exponential=TRUE, h=8)
holt_fitM1$model
accuracy(holt_fitM1)

checkresiduals(holt_fitM1)
Box.test(residuals(holt_fitM1), lag=10, fitdf = 0, type="Lj") ###white noise

####Additive, damped
cat("\014")
holt_fitDA<-holt(eggs, alpha=0.8, beta=0.2, initial="simple", dampled=TRUE, h=8)
accuracy(holt_fitDA)

holt_fitDA1<-holt(eggs, initial="simple", dampled=TRUE, h=8)
accuracy(holt_fitDA1)
holt_fitDA1$model

checkresiduals(holt_fitDA1)
Box.test(residuals(holt_fitDA1), lag=10, fitdf = 0, type="Lj") ###white noise

####Exponential/multiplicative, damped
cat("\014")
holt_fitDM<-holt(eggs, alpha=0.8, beta=0.2, initial="simple", dampled=TRUE, exponential=TRUE, h=8)
accuracy(holt_fitDM)

holt_fitDM1<-holt(eggs, initial="simple", dampled=TRUE, exponential=TRUE, h=8)
accuracy(holt_fitDM)
holt_fitDM$model

checkresiduals(holt_fitDM1)
Box.test(residuals(holt_fitDM1), lag=10, fitdf = 0, type="Lj") ###white noise

ets(eggs)


############## Holt's Winter Method ################
###Dataset: visitors
plot.ts(visitors)
visitors
#agg.visitots<-aggregate(visitors)
decompose(visitors)

visitors.train=window(visitors, end=1999.99)
visitors.test=window(visitors, start=2000)

############Seasonal Multiplicative
hw_fitM<-hw(visitors.train, seasonal = "multiplicative", h=8)
accuracy(hw_fitM, visitors.test)
hw_fitM$model

############Seasonal Additive
hw_fitA<-hw(visitors.train, seasonal = "additive", h=8)
accuracy(hw_fitA, visitors.test)
hw_fitA$model

############Seasonal Multiplicative, Multiplicative Model
hw_fitMM<-hw(visitors.train, seasonal = "multiplicative", exponential=TRUE, h=8)
accuracy(hw_fitMM, visitors.test)
hw_fitMM$model

############Seasonal Multiplicative, Damped, Multiplicative Model
hw_fitMDE<-hw(visitors.train, seasonal = "multiplicative", damped=TRUE, exponential=TRUE, h=8)
accuracy(hw_fitMDE, visitors.test)
hw_fitMDE$model ##Best Model
checkresiduals(hw_fitMDE)
Box.test(residuals(hw_fitMDE), lag=10, fitdf=0, type="Lj") ##White noise

tr<-ets(visitors.train)
test<-ets(visitors.test, model=tr)
accuracy(test)

##################### Non Seasonal ARIMA #######################
##dataset: wmurders
wmurders
adf.test(wmurders) ##Null Hypothesis accepted, so unit root, so nonstationary
plot.ts(wmurders)

wmurders_diff<-diff(wmurders, differences = 1)
adf.test(wmurders_diff) ##Null hypothesis rejected so stationary
plot.ts(wmurders_diff)


acf(diff(wmurders_diff)) ####MA, q=1
pacf(diff(wmurders_diff)) ###AR, p=1

tsdisplay(diff(wmurders_diff))

cat("\014")
arima.fit_murders<-Arima(y=wmurders, order=c(1,1,1))
arima.fit_murders
Acf(residuals(arima.fit_murders))
Box.test(residuals(arima.fit_murders), lag=20, fitdf=0, type="Lj")
## error is white noise and doesnot exhibit autocorrelation

arima.fitA<-auto.arima(wmurders, , stepwise=FALSE, approximation=FALSE)
arima.fitA

wmurders_forecast<-forecast(arima.fit_murders, h=8)
plot(wmurders_forecast, shaded = TRUE, shadecols = "oldstyle")

###dataset: usgdp
cat("\014")
usgdp
plot.ts(usgdp)

adf.test(usgdp)

usgdp_diff<-diff(usgdp, differences = 1)
adf.test(usgdp_diff)
plot.ts(usgdp_diff)

acf(diff(usgdp_diff))### MA, q=1
pacf(diff(usgdp_diff))### AR, p=2

arim.fit_gdp<-Arima(y=usgdp, order=c(2,2,1)) ##Best Model
arim.fit_gdp
Acf(residuals(arim.fit_gdp))
Box.test(arim.fit_gdp$residuals, lag=20, fitdf=0, type="Ljung-Box")
### Data doesnot exhibit Autocorrelation, white noise

arim.fit_gdp2<-Arima(y=usgdp, order=c(2,2,2))
arim.fit_gdp2

arima.fitA1<-auto.arima(usgdp)
arima.fitA1

usgdp_forecast<-forecast(arim.fit_gdp, h=8)
plot(usgdp_forecast, shaded = TRUE, shadecols = "oldstyle")


###dataset: intusage
cat("\014")
WWWusage
intusage<-WWWusage
plot.ts(intusage)
adf.test(intusage)

intusage_diff<-diff(intusage, differences = 2)
plot(intusage_diff)
adf.test(intusage_diff)

acf(diff(intusage_diff))##MA, q=0
pacf(diff(intusage_diff))##AR, p=3

tsdisplay(diff(intusage))

arima.fit_usage<-Arima(y=intusage, order=c(3,2,0)) ##Best Model
arima.fit_usage
Acf(residuals(arima.fit_usage))
Box.test(arima.fit_usage$residuals, lag=20, fitdf=0, type="Ljung-Box")
### Error doesnot exhibit autocorrelation and white noise

arima.fitA2<-auto.arima(intusage)
arima.fitA2

intusage_forecast<-forecast(arima.fit_usage, h=8)
plot(intusage_forecast, shaded = TRUE, shadecols = "oldstyle")


####dataset: mcopper
cat("\014")
mcopper
plot.ts(mcopper)
adf.test(mcopper)

mcopper_diff<-diff(mcopper, differences = 1)
adf.test(mcopper_diff)
plot.ts(mcopper_diff)

acf(diff(mcopper_diff))##MA, q=2
pacf(diff(mcopper_diff))##AR, p=0

arima.fit_mcopper1<-Arima(y=mcopper, order=c(0,1,2))
arima.fit_mcopper1

arima.fit_mcopper2<-Arima(y=mcopper, order=c(0,1,1)) ##Best Model
arima.fit_mcopper2
Acf(arima.fit_mcopper2$residuals)
Box.test(residuals(arima.fit_mcopper2), fitdf = 0, type = "Ljung-Box")
## error is white noise and doesnot exhibit autocorrelation

arima.fitA3<-auto.arima(mcopper, stepwise=FALSE, approximation=FALSE)
arima.fitA3

mcopper_forecast<-forecast(arima.fit_mcopper2, h=8)
plot(mcopper_forecast, shaded = TRUE, shadecols = "oldstyle")





############################ Seasonal Arima ##########################
###dataset: euretail
euretail
plot.ts(euretail)
adf.test(euretail)

euretail_diff<-diff(euretail, differences = 1)
adf.test(euretail_diff)
plot(euretail_diff)

acf(diff(euretail_diff)) ##q=1
pacf(diff(euretail_diff)) ###p=0

arima.fit_euretail<-arima(euretail, order=c(0,1,1), seasonal=c(0,1,1))
arima.fit_euretail

arima.fit_euretail2<-arima(euretail, order=c(0,1,2), seasonal=c(0,1,1)) 
arima.fit_euretail2

arima.fit_euretail3<-arima(euretail, order=c(0,1,3), seasonal=c(0,1,1)) ###Best Model
arima.fit_euretail3
Acf(residuals(arima.fit_euretail3))
Box.test(residuals(arima.fit_euretail3), lag=20, fitdf=0, type="Lj")
###Error terms are not correlated and is white noise

euretail_forecast<-forecast(arima.fit_euretail3, h=8)
plot(euretail_forecast, shaded = TRUE, shadecols = "oldstyle")

arima.fitS1<-auto.arima(euretail, stepwise=FALSE, approximation=FALSE)
arima.fitS1

##dataset: condmilk
condmilk
plot.ts(condmilk,)
adf.test(condmilk)

acf(diff(condmilk)) ##q=1
pacf(diff(condmilk)) ##p=2

arima.fit_condomilk<-arima(euretail, order=c(2,0,1), seasonal=c(2,0,0)) ###Best Model
arima.fit_condomilk
Acf(residuals(arima.fit_condomilk))
Box.test(residuals(arima.fit_condomilk), lag=20, fitdf=0, type="Lj")
## Error terms doesnot exhibit autocorrelation and white noise

condomilk_forecast<-forecast(arima.fit_condomilk, h=8)
plot(condomilk_forecast, shaded = TRUE, shadecols = "oldstyle")

arima.fitS2<-auto.arima(condmilk)
arima.fitS2



############# Detrend a time series ##############
usgdp
plot.ts(usgdp)
class(usgdp)
timeseries_usgdp=usgdp
trend_usgdp=ma(timeseries_usgdp, order=4, centre=TRUE)
detrend_usgdp=timeseries_usgdp-trend_usgdp
detrend_usgdp<-na.omit(detrend_usgdp)
plot.ts(detrend_usgdp)

adf.test(detrend_usgdp)

acf(diff(detrend_usgdp)) ##q=0
pacf(diff(detrend_usgdp)) ##p=1

arima.fit_detrendUsgdp<-arima(detrend_usgdp, order=c(1,0,0))
arima.fit_detrendUsgdp

arima.fit_detrendUsgdp2<-arima(detrend_usgdp, order=c(1,0,1))
arima.fit_detrendUsgdp2

arima.fit_detrendUsgdp3<-arima(detrend_usgdp, order=c(2,0,1)) ##Best Model
arima.fit_detrendUsgdp3
Acf(residuals(arima.fit_detrendUsgdp3))
Box.test(residuals(arima.fit_detrendUsgdp3), lag=20, fitdf = 0, type="Lj")
## Error terms doesnot exhibit autocorrelation and white noise

forecast_detrendusgdp<-forecast(arima.fit_detrendUsgdp3, h=10)
plot(forecast_detrendusgdp, shaded = TRUE, shadecols = "oldstyle")

auto.arima(detrend_usgdp)
