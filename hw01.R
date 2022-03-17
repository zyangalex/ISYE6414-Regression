#1, a):
rm(list = ls())


y <- c(2.563, 2.857, 3.549, 2.952, 3.691, 3.160, 2.879, 3.792, 3.419, 3.914);
x <- c( 24, 22, 27, 26, 30, 21, 20, 28, 22, 28);
plot(x, y);
fm1 <- lm( y ~ x)
fm1
summary(fm1)
xmean <- mean(x)
ymean <-mean(y)
x2sum <- sum(x**2)
y2sum <- sum(y**2)
xysum <- sum(x*y)
n <- length(x)

#1, b):
Sxx <- x2sum -n*(xmean)**2
Sxy <- xysum-n*xmean*ymean
beta1 <- Sxy/Sxx
beta0 <- ymean- beta1*xmean

#1, c):
Syy<- y2sum -n*(ymean)**2
RSS <- Syy-beta1*Sxy
sig2 <- RSS/(n-2)
sig <- sqrt(sig2)

#1, d):
x1 = 30
y30 = beta0+beta1*x1

#2, a):
alpha = 0.01
degrees.freedom = n - 2
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)
high = beta1 + t.score* sig/(sqrt(Sxx))
low = beta1 - t.score* sig/(sqrt(Sxx))
beta1CI  = c(high, low)

#2, b)
high2 = beta0 +t.score*sig*sqrt((1/n)+(xmean**2)/Sxx)
low2 = beta0 -t.score*sig*sqrt((1/n)+(xmean**2)/Sxx)
beta0CI  = c(high2, low2)

#2, c):
high3 = y30+ t.score*sig*sqrt((1/n)+(x00-xmean)**2/Sxx)
low3 =y30- t.score*sig*sqrt((1/n)+(x00-xmean)**2/Sxx)
yCI  = c(high3, low3)

#2, d):
high4 = y30+ t.score*sig*sqrt(1+(1/n)+(x1-xmean)**2/Sxx)
low4 =y30- t.score*sig*sqrt(1+(1/n)+(x1-xmean)**2/Sxx)
ypCI  = c(high4, low4)

#3, a):
alpha1 = 0.05
degrees.freedom = n - 2
t.score1 = qt(p=alpha1/2, df=degrees.freedom,lower.tail=F)
print(t.score1)
beta1t = 0
tobs = (beta1-beta1t)/(sig/sqrt(Sxx))
if (abs(tobs) > t.score1){
  print("Reject H0")
  }else{
  print("Accept H0")}

#3, b):
beta0t = 1
tobs1 = (beta0-beta0t)/(sig*sqrt((1/n)+(xmean**2/Sxx)))
if (abs(tobs1) > t.score1){
  print("Reject H0")
}else{
  print("Accept H0")}

#4, A)
x1 = sqrt(x)
x1mean <- mean(x1)
x12sum <- sum(x1**2)
x1ysum <- sum(x1*y)

Sx1x1 <- x12sum -n*(x1mean)**2
Sx1y <- x1ysum-n*x1mean*ymean
beta1new <- Sx1y/Sx1x1
beta0new <- ymean- beta1new*x1mean


Syy<- y2sum -n*(ymean)**2
RSSnew <- Syy-beta1new*Sx1y
signew2 <- RSSnew/(n-2)
signew <- sqrt(signew2)

#4 b i): ## CI on beta1(with sqrt(x))
high5 = beta1new+t.score*(signew/sqrt(Sx1x1))
low5 = beta1new-t.score*(signew/sqrt(Sx1x1))
beta1CInew = c(low5, high5)

#4 b ii);## CI on beta0(with sqrt(x))
high6 = beta0new +t.score*signew*sqrt((1/n)+(x1mean**2)/Sx1x1)
low6 = beta0new -t.score*signew*sqrt((1/n)+(x1mean**2)/Sx1x1)
beta0CInew  = c(low6, high6)

#4 b iii):
x0 = sqrt(30)
Ynew = beta0new +beta1new*x0
#CI on Ynew:
high7 = Ynew+ t.score*sig*sqrt((1/n)+(x0-x1mean)**2/Sx1x1)
low7 =Ynew- t.score*sig*sqrt((1/n)+(x0-x1mean)**2/Sx1x1)
yCInew  = c(high7, low7)


#4 b iv):
high8 = Ynew+ t.score*sig*sqrt(1+(1/n)+(x0-x1mean)**2/Sx1x1)
low8 =Ynew- t.score*sig*sqrt(1+(1/n)+(x0-x1mean)**2/Sx1x1)
ypCInew  = c(high4, low4)
