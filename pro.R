df = heart[c("age","chol","thalach","oldpeak","fbs","trestbps")]
cor(df)
df$fbs <- as.factor(df$fbs)
#variable correlation
pairs(df)
#full model
reg <- lm(trestbps ~ fbs*(age + chol + thalach + oldpeak),data = df)
summary(reg)
reg <- lm(trestbps~.+fbs*oldpeak,data=df)
summary(reg)
#qqplot
qqnorm(rstandard(reg)) 
qqline(rstandard(reg))
#light heavy tail, so we need to take log of trestbps

df$logtrestbps <- log(df$trestbps)
reg <- lm(logtrestbps~age + chol + thalach + oldpeak+fbs*oldpeak,data=df)
qqnorm(rstandard(reg)) 
qqline(rstandard(reg))
#residual plot against fitted value
plot(rstandard(reg)~reg$fitted.values)
abline(0,0)
#check outlier
abline(3,0) #one outlier

#residual plots against every variable
plot(rstandard(reg)~df$age)
abline(0,0)
abline(3,0)
plot(rstandard(reg)~df$chol)
abline(0,0)
abline(3,0)
plot(rstandard(reg)~df$thalach)
abline(0,0)
abline(3,0)
plot(rstandard(reg)~df$oldpeak) 
abline(0,0)
abline(3,0)

# model: log(trestbps)~age+chol+thalach+oldpeak+fbs
summary(reg) #adjr2 = 0.1246

#varibale selection
library(leaps)
sreg <- regsubsets(logtrestbps~age+chol+thalach+oldpeak+fbs*oldpeak,data = df)
summary(sreg)$which
cp <- summary(sreg)$cp
plot(2:7,cp,xlab="p",ylab="Cp",ylim=c(0,20),xlim=c(0,10))
abline(0,1)
#Cp value of model4 and model5 are closest to p, and smallest
which.max(summary(sreg)$adjr2)
#model4 has largest adjr2 value

#C-V using model4 and model5
library(caret)
# Define train control for k fold cross validation
set.seed(123)
train_control <- trainControl(method="cv", number=5)
# Fit Model
model <- train(logtrestbps~age+oldpeak+thalach+oldpeak*fbs-fbs, data=df, trControl=train_control, method="lm")
# Summarise Results
print(model)

set.seed(123)
train_control <- trainControl(method="cv", number=5)
model <- train(trestbps~age+oldpeak+thalach+oldpeak*fbs+chol-fbs, data=df, trControl=train_control, method="lm")
print(model)
#model4 has smaller RMSE, model4 is the best model

model <- lm(trestbps~age+oldpeak+thalach+oldpeak*fbs-fbs,data = df)
summary(model)
#every coefficient is significant, and adjusted R-squared is larger than the full 
#model (0.1272 > 0.1246)
