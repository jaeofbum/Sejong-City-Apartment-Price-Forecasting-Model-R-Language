data = read.csv("./final.csv", header=F, fileEncoding = "euc-kr")
#names(data) <- c("단지명", "주소","평수", "년","월", "일", "매매가격", "층수", "준공연도","중위가격", "매매 수급동향", "금리", "날짜")
names(data) <- c("apt_name", "adress","area", "year","month", "day", "price", "floor", "make_year","middle_price", "sale_rate", "IR")
#data$'date'<-as.Date(data$'date', "%Y-%m-%d")
str(data)
dim(data)
data = na.omit(data)
data_cor <- data[,c("area","price","floor","year","month","day","make_year","middle_price","sale_rate", "IR")]
cor(data_cor)
#그래프 그리기
pairs(data_cor[,1:10], pch = 19)

#트레인 데이터셋
set.seed(1)
train=data(True,False)

#valdation 데이터셋

summary(data)

names(data)
lm.fit=lm(price~.-apt_name-adress, data=data)
lm.fit
summary(lm.fit)
confint(lm.fit)
library(car)
vif(lm.fit)
lm.fit1=lm(price~.-apt_name-adress,data=data)
summary(lm.fit1)
lmfit1=update(lm.fit, ~.-apt_name-adress)
library(ISLR)
set.seed(1)
train=sample(32248, 6200)
attach(data)
mean((price-predict(lm.fit,data))[-train]^2)
lm.fit=lm(price~.-apt_name-adress,data=data,subset=train)
lm.fit

summary(lm.fit)
glm.fit=glm(price~.-apt_name-adress-year-month-day,data=data)
coef(glm.fit)
library(boot)
glm.fit=glm(price~.,data=data)
cv.err=cv.glm(data,glm.fit)
cf.err$delta
cv.error=rep(0,5)
names(data)
#subset
library(leaps)
regfit.full=regsubsets(price~.-apt_name-adress-year-month-day, data=data,nvmax=10)
reg.summary=summary(regfit.full)
summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables",ylab="RSS")
plot(reg.summary$adjr2, xlab="Number of Variables", 
     ylab="Adfusted RSq")
coef(regfit.full,7)
#Foward and Backward
regfit.fwd=regsubsets(price~.-apt_name-adress,data=data,nvmax=10,
                      method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(price~.-apt_name-adress,data=data,nvmax=10,
                      method="backward")
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)
#choosing
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(data),rep=TRUE)
test=(!train)

regfit.best=regsubsets(price~., data=data[train,],
                       nvmax=7)

test.mat=model.matrix(price~., data=data[test,])
val.errors=rep(NA,7)
for(i in 1:7){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((data$price[test]-pred)^2)
}
val.errors
#ridge regression
#fix(data)
x=model.matrix(price~.-apt_name-adress,data)[,-1]
y=data$price
library(glmnet)
grid=10^seq(10,-2,length=32469)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda [50]
coef(ridge.mod)[ ,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2) )
ridge.mod$lambda [60]
sqrt(sum(coef(ridge.mod)[-1,60]^2) )
predict (ridge.mod ,s=50,type=" coefficients") [1:20,]
set.seed(1)
train=sample (1: nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[ train],alpha=0, lambda =grid ,
                 thresh =1e-12)
ridge.pred=predict (ridge.mod ,s=4, newx=x[test ,])
mean((ridge.pred -y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred=predict (ridge.mod ,s=1e10 ,newx=x[test ,])
mean((ridge.pred -y.test)^2)
#lasso
lasso.mod=glmnet(x[train,],y[train],alpha=1, lambda =grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[ train],alpha=1)
plot(cv.out)
bestlam =cv.out$lambda.min
lasso.pred=predict(lasso.mod ,s=bestlam ,newx=x[test,])
mean((lasso.pred -y.test)^2)
out=glmnet (x,y,alpha=1, lambda=grid)
lasso.coef=predict (out ,type=" coefficients",s= bestlam) [1:20,]
lasso.coef
#Tree
install.packages("tree")
library(tree)
library(MASS)
set.seed(1)
train = sample (1:nrow(data), nrow(data)/2)
tree.data=tree(price~.-apt_name-adress,data , subset=train)
summary(tree.data)
plot(tree.data)
text(tree.data, pretty =0)
cv.data=cv.tree(tree.data)
plot(cv.data$size ,cv.data$dev ,type='b')
prune.data=prune.tree(tree.data ,best=11)
plot(prune.data)
text(prune.data , pretty =0)
yhat=predict (tree.data ,newdata=data[-train ,])
data.test=data[-train ,"price"]
plot(yhat ,data.test)
abline (0,1)
mean((yhat -data.test)^2)
#random frest
#install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.data= randomForest(price~.,data=data , subset=train,
                       mtry=11,importance =TRUE)
bag.data
yhat.bag = predict(bag.data , newdata=data[-train ,])
plot(yhat.bag , data.test)
abline (0,1)
mean((yhat.bag -data.test)^2)
bag.boston= randomForest(price~.,data=data , subset=train ,
                          mtry=13,ntree=25)
yhat.bag = predict(bag.data , newdata=data[-train ,])
mean((yhat.bag -data.test)^2)
set.seed(1)
rf.data= randomForest(price~.,data=data , subset=train ,
                          mtry=6, importance =TRUE)
yhat.rf = predict(rf.data ,newdata=data[-train ,])
mean((yhat.rf-data.test)^2)
importance(rf.data)
varImpPlot(rf.data)



#----------------------------
lm.fit1=lm(price~area, data=data)
lm.fit2=lm(price~floor, data=data)
lm.fit3=lm(price~make_year, data=data)
lm.fit4=lm(price~middle_price, data=data)
lm.fit5=lm(price~sale_rate, data=data)
lm.fit6=lm(price~IR, data=data)
lm.fit7=lm(price~date, data=data)
summary(lm.fit1)
summary(lm.fit2)
summary(lm.fit3)
summary(lm.fit4)
summary(lm.fit5)
summary(lm.fit6)
summary(lm.fit7)

head(lm.fit)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data=data.2021_22,
        interval='confidence')
library(car)
vif(lm.fit)


attach(data)
contrasts(area)

lm.fit=lm(price~floor, data=data,subset=train)
attach(data)
mean((price-predict(lm.fit,data))[-train]^2)
