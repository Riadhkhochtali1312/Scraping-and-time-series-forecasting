#packages
library(ggplot2)
library(forecast)
library(highcharter)
library(tseries)
library(Metrics)
library(fpp2)
library(TTR)
library(dplyr)
library(urca)
library(smooth)





#ARIMAX using the exchange rate
tun3$`1 Mois`=as.numeric(tun3$`1 Mois`)
tun3$xrate=as.numeric(tun3$xrate)

tun3$`1 Mois`
tr1  <-tun3[120:240, ]
plot(ts(tr1$`1 Mois`),lwd=2.0,col="blue")
tst1   <- tun3[240:270, ]
mx1=tun3[4]

sbx1=mx1[120:270,]
tssbx1=ts(sbx1,frequency=365)
plot(tssbx1)
tseg1=ts(tr1[12],frequency=365)
tstr1=ts(tr1[4],frequency=365)
tstx1=ts(tst1[4],frequency=365)
ts.plot(tstr1,lwd=2.0,col="blue")
ts.plot(tseg1,lwd=2.0,col="blue")
tstr1 %>% ur.kpss() %>%summary()
tseg1 %>% ur.kpss() %>%summary()
arx1=auto.arima(tstr1,xreg=tseg1,allowdrift=TRUE, trace=TRUE)
arx1
checkresiduals(arx1)
fexe1 <-arx1 %>% forecast(xreg=tseg1,h=10)
fexe1
plot(fexe1,lwd=2.0,ylim=c(7.4,8.1))   
lines(tssbx1,col="red",lwd=2.0)  
summary(arx1)
Box.test(arx1$residuals,type="Ljung-Box")

cor.test(tr1$`1 Mois`,tr1$xrate)

#two months
mx2=tun3$`2 Mois`
sbx2=mx2[120:272]
tssbx2=ts(sbx2,frequency=365)
tseg2=ts(tr1$xrate,frequency=365)
tstr2=ts(tr1$`2 Mois`,frequency=365)
ts.plot(tstr2)
ts.plot(tseg2)
tstr2 %>% ur.kpss() %>%summary()
tseg2 %>% ur.kpss() %>%summary()
arx2=auto.arima(tstr2,xreg=tseg2,allowdrift=TRUE, trace=TRUE)
arx2
checkresiduals(arx2)
fexe2 <-arx2 %>% forecast(xreg=tseg2,h=41)
fexe2
plot(fexe2,lwd=2.0,ylim=c(7.6,8.5))
lines(tssbx2,lwd=2.0,col="red")
Box.test(arx2$residuals,type="Ljung-Box")


#three months
mx3=tun3[6]
sbx3=mx3[190:272,]
tssbx3=ts(sbx3,frequency=365)
tseg3=ts(tr1[12],frequency=365)
tstr3=ts(tr1[6],frequency=365)
ts.plot(tstr3)
ts.plot(tseg3)
tstr3 %>% ur.kpss() %>%summary()
tseg3 %>% ur.kpss() %>%summary()
arx3=auto.arima(tstr3,xreg=tseg3,allowdrift=TRUE, trace=TRUE)
arx3
checkresiduals(arx3)
fexe3 <-arx3 %>% forecast(xreg=tseg3,h=41)
fexe3
plot(fexe3)
lines(tssbx3,col="red",lwd=2.0)
Box.test(arx3$residuals,type="Ljung-Box")


#six months
mx4=tun3[7]
sbx4=mx4[190:272,]
tssbx4=ts(sbx4,frequency=365)
tseg4=ts(tr1[12],frequency=365)
tstr4=ts(tr1[7],frequency=365)
ts.plot(tstr4)
ts.plot(tseg4)
tstr4 %>% ur.kpss() %>%summary()
tseg4 %>% ur.kpss() %>%summary()
arx4=auto.arima(tstr4,xreg=tseg4,allowdrift=TRUE, trace=TRUE)
arx4
checkresiduals(arx4)
fexe4 <-arx4 %>% forecast(xreg=tseg4,h=100)
fexe4
plot(fexe4,lwd=2.0)
lines(tssbx4,lwd=2.0,col="red")
summary(arx)
Box.test(arx4$residuals,type="Ljung-Box")



#plot the whole thing
dev.new(width=30, height=5, unit="cm")
plot(ts(tun3$`12 Mois`),main = "TUNIBOR for each maturity")
lines(ts(tun3$`2 Mois`),col="red")
lines(ts(tun3$`3 Mois`),col="green")
lines(ts(tun3$`6 Mois`),col="blue")
lines(ts(tun3$`9 Mois`),col="cornflowerblue") 
lines(ts(tun3$`1 Mois`),col="cadetblue1")

legend("topleft",
       c("9 mois","1 mois","2 mois","3 mois","6 mois","12 mois"),
       fill=c("black","red","green","blue","cornflowerblue","cadetblue1")
)


#ploting maturities in one day
month_test=tst1$`1 Mois`
two_month_test=tst1$`2 Mois`
three_months_test=tst1$`3 Mois`
six_months_test=tst1$`6 Mois`

ma=fexe1$mean[1]
na=fexe2$mean[1]
oa=fexe3$mean[1]
pa=fexe4$mean[1]
ma

v2=c(ma,na,oa,pa)
plot(v2,type="o",col="red")

laa=list(ts(month_test))
l2aa=list(ts(two_month_test))
l3aa=list(ts(three_months_test))
l6aa=list(ts(six_months_test))
eaa=laa[[1]][1]
faa=l2aa[[1]][1]
gaa=l3aa[[1]][1]
haa=l6aa[[1]][1]

plot(c(eaa,faa,gaa,haa),col="red",type="o",lwd=2.0,ylim=c(8.0,8.8))
lines(c(ma,na,oa,pa),col="blue",type="o",lwd=2.0)
legend(x="topleft",legend=c("real", "forecasted"), fill = c("red","blue"))


v3=c(fexe1$mean[2],fexe2$mean[2],fexe3$mean[2],fexe4$mean[2])
c3=c(laa[[1]][2],l2aa[[1]][2],l3aa[[1]][2],l6aa[[1]][2])
plot(v3,type="o",col="blue",lwd=2.0)
lines(c3,type="o",col="red",lwd=2.0)
legend(x="topleft",legend=c("real", "forecasted"), fill = c("red","blue"))

v4=c(fexe1$mean[3],fexe2$mean[3],fexe3$mean[3],fexe4$mean[3])
c4=c(laa[[1]][3],l2aa[[1]][3],l3aa[[1]][3],l6aa[[1]][3])
plot(v4,type="o",col="blue",lwd=2.0)
lines(c4,type="o",col="red",lwd=2.0)
legend(x="topleft",legend=c("real", "forecasted"), fill = c("red","blue"))

ggplot()+
  geom_line(data=tun3, mapping = aes(x=date, y=`1 Mois`),  color="blue")+
  geom_line(data=tun3, mapping = aes(x=date, y=`2 Mois`),  color="red")+
  geom_line(data=tun3, mapping = aes(x=date, y=`3 Mois`),  color="green")+
  geom_line(data=tun3, mapping = aes(x=date, y=`6 Mois`),  color="yellow")+
  geom_line(data=tun3, mapping = aes(x=date, y=`9 Mois`),  color="black")+
  geom_line(data=tun3, mapping = aes(x=date, y=`12 Mois`),  color="cornflowerblue")+
  scale_colour_manual("", 
                      breaks = c("1 month", "2 months", "3 months","6 months","9 months","12 months"),
                      values = c("1 month"="blue", "2 months"="red", 
                                 "3 months"="green","6 months"="yellow","9 months"="black","12 months"="cornflowerblue")) +
  xlab("date ") +
  scale_y_continuous("tunibor value", limits = c(7,10)) + 
  labs(title="evolution for different maturities")
 
  
adf.test(tun3$`2 Mois`)
plot(ts(tun3$`2 Mois`))

     