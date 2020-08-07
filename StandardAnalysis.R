### This file performs reading the data and offers a variety of standard statistical analysis tools 
### to start the analysis.

### reading the data, code for a particular real economic dataset
a=read.csv("data.csv", header=T, dec=",", sep=";"); 
x=cbind(a[,3], a[,4], a[,5], a[,6], a[,7], a[,8], a[,9], a[,10], a[,11], a[,12], a[,13], a[,15]); 
y=a[,14];

### exploratory data analysis should e.g. reveal whether the linear regression model is meaningful
### plot of the response
for (i in 1:12summ)
  {plot(x[,i], y);
   print(c(i, "press sth"));
   readline()
  }

###variable selection, naive approaches
index=c(5,6);
  my1=rq(y~x[,index], tau=0.1);
  summary(my1);
index=c(5,6);
  my3=rq(y~x[,index], tau=0.3);
  summary(my3);
index=c(1,4,5,6,10);
  my5=rq(y~x[,index], tau=0.5);
  summary(my5);
index=c(5,6,10);
  my7=rq(y~x[,index], tau=0.7);
  summary(my7);
index=c(6,10);
  my9=rq(y~x[,index], tau=0.9);
  summary(my9);

### technical necessity
 mydata=data.frame(y,x;
### least squares
  index=c(1,4,5,6,10); 
  index=c(3,5,6,10);
   # index=1:12;
  my=lm(y~x[,index]); #my=lm(y~x[,index]);
  summary(my);
### var(b)
  varb=summary(my)$cov.unscaled
### heteroscedasticity test; this is multiple testing
### should be applied after variable selection; but variable selection suffers from heter.
### under multicollinearity, the test have a low power! practitioners are usually not aware of this
  library(het.test);
  bptest(y~x);
  bptest(my, data = mydata); 
  bptest(my, varformula = y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12, data = mydata); 
  bptest(my, varformula = y ~ X1+X5+X6+X9+X10, data = mydata); 
  bptest(my, varformula = y ~ X1*X5 + X1*X6 + X1*X9 + X1*X10 + X5*X6 + X5*X9 + X5*X10+ X6*X9 + X6*X10 + X9*X10
   + I(X1^2) + I(X5^2) + I(X6^2) + I(X9^2) + I(X10^2), data=mydata); #White's test 
    ## too many regressors; using all of them loses power
### heteroscedastic regression
  uu=my$resid^2;
  xx=x[,index]/sqrt(uu); #indiv. rows
  yy=y/sqrt(uu); 
  mytrafo=lm(yy~xx); #extremely bad
  summary(mytrafo); #very heteroscedastic
  bptest(my, varformula = yy ~ xx[,1] + xx[,2] + xx[,3] + xx[,4] +xx[,5]);
  bptest(my, varformula = yy ~ xx[,1] + xx[,2] + xx[,3] + xx[,4] +xx[,5] + xx[,6] + xx[,7] + xx[,8] + xx[,9] +xx[,10] + xx[,11] + xx[,12]);
### White's estimation
  library("sandwich");
  w=vcovHC(my); #var(b), we need sqrt
  l=my$coef-2*sqrt(diag(w)); u=my$coef+2*sqrt(diag(w));
  cbind(l,u); #test of significance
### quantile regression (possibly for the most relevant regressors)
  library(quantreg);
  index=c(3,5,6,8,10); xx=x[,index];
  #taus=rep(1:10)/10-0.05; #10 values
  taus=rep(1:5)/5-0.1; #5 values
  my=rq(y~xx, tau=taus); #here, lower and upper bounds are given
  summary(rq(y~xx, tau=taus)); #conf.int. for more values of tau
  # my$rho=sum of residuals, objective function
  # my$dual = regression rank scores
##############################################
 

x11()
jpeg("U:/plot5.jpg", height=5, width=5, units="in", res=600);
plot(x[,5], y, xlab=" ", ylab=" ");
dev.off()
x11()
jpeg("U:/plot6.jpg", height=5, width=5, units="in", res=600);
plot(x[,6], y, xlab=" ", ylab=" ");
dev.off()
x11()
jpeg("U:/plot10.jpg", height=5, width=5, units="in", res=600);
plot(x[,10], y, xlab=" ", ylab=" ");
dev.off()
############################### test for regression quantiles
  library(quantreg);
  my1 = rq(y~x, tau=0.1); my3 = rq(y~x, tau=0.3); my5 = rq(y~x, tau=0.5); my7 = rq(y~x, tau=0.7); my9 = rq(y~x, tau=0.9); 
  anova(my1,my3,my5,my7,my9);
  mya = rq(y~x[,5]+x[,6], tau=0.1);
  myb = rq(y~x[,5]+x[,6], tau=0.3);
  myc = rq(y~x[,1]+x[,4]+x[,5]+x[,6]+x[,10], tau=0.5);
  myd = rq(y~x[,5]+x[,6]+x[,10], tau=0.7);
  mye = rq(y~x[,6]+x[,10], tau=0.9);
  anova(mya,myb,myc,myd,mye); #not possible, not the same regressors!
### and for a submodel
  #xm=cbind(x[,1], x[,4], x[,5], x[,6], x[,10]);
  xm=cbind(x[,1], x[,3], x[,5], x[,6], x[,9], x[,10]);
  my1 = rq(y~xm, tau=0.1); my3 = rq(y~xm, tau=0.3); my5 = rq(y~xm, tau=0.5); my7 = rq(y~xm, tau=0.7); my9 = rq(y~xm, tau=0.9); 
  anova(my1,my3,my5,my7,my9);
##############
x11()
setEPS()
postscript("U:/plot5.eps", height=5, width=5, units="in", res=600);
plot(x[,5], y, xlab=" ", ylab=" ");
dev.off()
x11()
setEPS()
postscript("U:/plot6.eps");
plot(x[,6], y, xlab=" ", ylab=" ");
dev.off()
x11()
setEPS()
postscript("U:/plot10.eps");
plot(x[,10], y, xlab=" ", ylab=" ");
dev.off()
