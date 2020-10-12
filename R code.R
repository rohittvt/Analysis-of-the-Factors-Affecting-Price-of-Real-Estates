#Data Cleaning

library(knitr)
#read excel file
data<-read.csv("C:/Users/DANISH/Desktop/project_sem2/SMBA_project1/project_data.csv")
#checking for any missing values
sum(is.na(data))
Data summary
summary(data)
str(data)
#correlation check
correlation Matrix
round(cor(data),2)

#Data Visulaization
library(corrplot)
M <- cor(data)
corrplot(M, method = "number")
plot(data)
library("ggplot2", lib.loc="~/R/win-library/3.5")
ggplot(data=data,mapping=aes(x=latitude, y=longitude, alpha= house.price))+ geom_point(size=4)
#theres is a dense region where prices are high? why?
ggplot(data=data,mapping=aes(x=stores, y=house.price))+ geom_boxplot(fill="pink")
ggplot(data=data,mapping=aes(x=dmrt, y=house.price))+ geom_smooth(color="orange") + geom_point(aes(color=
stores))
#no of stores effect
ggplot(data=data,mapping=aes(x=latitude, y=longitude, color= dmrt))+ geom_point(size=2)
# we can see the high price points near mrt station in graph.
ggplot(data=data,mapping=aes(x=House.age, y=house.price))+ geom_smooth(color="orange") + geom_point(aes(color=
dmrt))
# interesting thing as we might expect intuitively house price increases with decreasing age, that is not the case here
that explains role of other variables in analysis.
summary(data$T.date)
ggplot(data=data,mapping=aes(x=as.factor(T.date), y=house.price))+ geom_boxplot(fill="orange")
ggplot(data=data,mapping=aes(x=as.numeric(T.date), y=house.price))+ geom_smooth(color="orange")
#effect of time of deal
cor(as.numeric(data$T.date),data$house.price)
Multicollinearity check
plot(data$dmrt,data$longitude, xlab="Distance from station", ylab="Longitude", col="dark green")
#There is no perfect multicollinearity
checking for outlier
#checking for outlier
boxplot(data$T.date , main="Transaction date",
 col = "brown")
boxplot(data$House.age ,main="House age",
 col = "blue")
boxplot(data$dmrt ,main="Distance from stores",
 col = "red") #large potential outliers exist
boxplot(data$latitude , main="Latitude",
 col = "green")
boxplot(data$stores , main="Stores",
 col = "grey")
boxplot(data$longitude , main="Longtitude",
 col = "dark green")
boxplot(data$T.date , main="Transaction date",
 col = "brown", outline=FALSE)
boxplot(data$House.age ,main="House age",
 col = "blue",outline=FALSE)
boxplot(data$dmrt ,main="Distance from stores",
 col = "red",outline=FALSE) #large potential outliers exist
boxplot(data$latitude , main="Latitude",
 col = "green",outline=FALSE)
boxplot(data$stores , main="Stores",
 col = "grey",outline=FALSE)
boxplot(data$longitude , main="Longtitude",
 col = "dark green",outline=FALSE)
 
#Model Fitting
#single Regressor
# Model 1
model1=lm(data=data,house.price~poly(as.numeric(T.date),3))
summary(model1)
ggplot(data=data,mapping=aes(x=as.numeric(T.date), y=house.price))+ geom_smooth(color="orange",
method="lm",formula=y~poly(x,3))

# Model 2
model2=lm(data=data, house.price~ poly(House.age,3))
summary(model2)
#plot(House.age,house.price, col="red")
#lines(smooth.spline(House.age,predict(model2)),col="blue",
 #lwd=3)
plot(model2)

#Model 3
model3=lm(data=data, house.price~ poly(dmrt,2))
summary(model3)
#plot(dmrt,house.price, col="red" ,xlab="Distance from station")
#lines(smooth.spline(dmrt,predict(model3)),col="blue",
 #lwd=3)
plot(model3)

#Model 4
Model 4 (house.price=27.1811+2.6377*stores)
attach(data)
plot(as.factor(stores),house.price,
 col="blue",
xlab="stores", ylab="HousePrice")
plot(stores,house.price, type="p", col="red")
model4<-lm(house.price~stores)
summary(model4)
abline(model4, col="Blue" )
plot(model4)

#Model 5
plot(latitude,house.price, type="p", col="red")
model5<-lm(house.price~latitude)
abline(model5, col="Blue" )
summary(model5)
plot(model5)

#Model 5
plot(latitude,house.price,
xlab="Latitude", ylab="HousePrice")
model5<-lm(house.price~poly(latitude,2, raw=FALSE))
summary(model5)
lines(smooth.spline(latitude,predict(model5)),col="blue",
lwd=3)

#Model 6
plot(longitude,house.price,
xlab="longitude", ylab="HousePrice")
model6<-lm(house.price~poly(longitude,2, raw=FALSE))
summary(model6)
lines(smooth.spline(longitude,predict(model6)),col="blue",
lwd=3)

#Multiple Regressor
#Model 7
model7<-lm(house.price~T.date+House.age+dmrt+ stores+latitude+longitude)
summary(model7)

#Model 8
model8<-lm(house.price~T.date+House.age+dmrt+ stores+latitude)
summary(model8)

#Model 9 Log-linear model
model9<-lm(log(house.price)~T.date+House.age+dmrt+ stores+latitude+longitude)
summary(model9)

#Model 10 Linear-log model
model10<-lm(house.price~log(T.date)+House.age+stores+log(dmrt)+log(longitude)+log(latitude))
summary(model10)

#Model 11 Log-Log Model
model11<-lm(log(house.price)~log(T.date)+House.age+stores+log(dmrt)+log(latitude))
summary(model11)
plot(model11)

#Model 12 ( Picking Best Individual model)
model12<-lm(log(house.price)~poly(T.date,2)+poly(House.age,2)+stores+poly(dmrt,2)+log(latitude))
summary(model12)
plot(model12)
