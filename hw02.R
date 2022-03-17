


GPAdata  <- read.table(file.choose(), sep = ",");
colnames(GPAdata) <- c("Y", "X")

fitLm1 = lm(Y ~ X, data=GPAdata);
summary(fitLm1)
##Report the fitted linear regression model, and from the summary, 
##what conclusions can you draw onpredicting the freshman GPA from 
##the ACT test score?
Y <- c(GPAdata[,1])
X <- c(GPAdata[, 2])
r = cor(X, Y)

n = length(X) #n = 120
alpha = 0.05
degrees.freedom = n - 2
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F) #t.score = 1.9803
Tobs = r*sqrt((n-2)/(1-r**2)) #3.0398
Tobs > t.score

ro0 = 0.1
zobs = ((0.5*log((1+r)/(1-r), base = exp(1))-0.5*log((1+ro0)/(1-ro0), base = exp(1))))/sqrt(1/(n-3))
##zobs = 1.9034024 > 1.645 = Z(0.05)

Zr = (0.5*log((1+r)/(1-r), base = exp(1)))
zalpha2 = 1.645
Zu = Zr+zalpha2*(1/sqrt(n-3))
Zl = Zr-zalpha2*(1/sqrt(n-3))
CIforE = c((exp(2*Zl)-1)/(exp(2*Zl)+1),(exp(2*Zu)-1)/(exp(2*Zu)+1) )
CIforE

#f)
rs = cor(X, Y, method = "spearman")

#g)
tobss = rs* sqrt((n-2)/(1-rs**2))
#tobss  = 3.5772013
alpha = 0.05
degrees.freedom = n - 2
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F) #t.score = 1.9803

plot(X, Y, data = GPAdata)
abline(fitLm1)
