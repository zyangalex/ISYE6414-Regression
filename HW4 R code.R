
#####p1 a):
p1X1 <- c(4 , 4 , 4 , 4 , 6 , 6 , 6 , 6 , 8 , 8 ,8 , 8 , 10 , 10 , 10 , 10)
p1X2 <- c(2 , 4 , 2 , 4 , 2 , 4 , 2 , 4 , 2 , 4 , 2 , 4 , 2 , 4 , 2 , 4)
p1Y <- c(64 ,73, 61 ,76 , 72 , 80 , 71, 83 , 83 , 89 , 86, 93 , 88 , 95 , 94 , 100)

lm1  = lm(p1Y~ p1X1 + p1X2)
summary(lm1)
lm1

X1 = as.matrix(cbind(1, p1X1, p1X2));
Y = as.vector(p1Y);
c1 = as.matrix(rbind(1, 5, 6));

xnewdf =  data.frame(p1X1=5, p1X2=6);
predict(lm1, xnewdf, interval = "prediction", level = 0.95)

p2X3 = p1X1 * p1X2

lm2  = lm(p1Y~ p1X1 + p1X2 + p2X3)
lm2
xnew2 = data.frame(p1X1 = 5, p1X2 = 6, p2X3 = 30)
predict(lm2, xnew2, interval = "prediction", level = 0.95)

p3x = p1X1 ** 2;

lm3  = lm(p1Y~ p1X1 + p1X2 + p3x + p2X3);
lm3;
xnew3 = data.frame(p1X1 = 5, p1X2 = 6);
predict(lm3, xnew3, interval = "prediction", level = 0.95);

##get SSR:

c(deviance(lm1), deviance(lm2), deviance(lm3))

c(summary(lm1)$r.squared, summary(lm2)$r.squared, summary(lm3)$r.squared)
> c(summary(lma)$adj.r.squared, summary(lmb)$adj.r.squared, summary(lmc)$adj.r.squared)
[1] 0.9446834 0.9527834 0.9500508
> c(summary(lma)$sigma, summary(lmb)$sigma, summary(lmc)$sigma)
[1] 2.693297 2.488306 2.559297
> c(AIC(lma), AIC(lmb), AIC(lmc))
[1] 81.78831 79.97439 81.48238
> c(BIC(lma), BIC(lmb), BIC(lmc))
[1] 84.87867 83.83733 86.11791


salary0 <-read.table(file.choose(), sep=",");

salary <- salary0; 
salary$V2 <- I(salary0$V2 == 2);
salary$V3 <- I(salary0$V2 == 3); 
salary[,4:5] <- salary0[,3:4];
colnames(salary) <- c("Y", "X1", "X2","X3", "X4");

lm4  = lm(salary$Y~ salary$X1 + salary$X2 + salary$X3 + salary$X4);
library(ggplot2)
lm4
summary(lm4)
plot(lm4)
resid(lm4)

plot(abs(resid(lm4))~salary$X3)

lm5  = lm(abs(resid(lm4))~ salary$X3 + salary$X4);
summary(lm5)

wt <- 1 / ((2.4204+0.3996*salary$X3+0.2695*salary$X4)^2)
wls_model <- lm(salary$Y~ salary$X1 + salary$X2 + salary$X3 + salary$X4, weights=wt)
summary(wls_model)

wls_model_E  = lm(abs(resid(wls_model))~ salary$X3 + salary$X4);
summary(wls_model_E)

wt2 = 1 / ((1.7033+0.4724* salary$X3+0.2988*salary$X4)^2)
wls_model_2 <- lm(salary$Y~ salary$X1 + salary$X2 + salary$X3 + salary$X4, weights=wt2)
summary(wls_model_2)

