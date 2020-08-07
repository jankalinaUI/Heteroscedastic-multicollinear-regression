### this file contains code for more advanced statistical tools, including regression quantiles
### we assume the data to be the same as in the file StandardAnalysis.R 

### lasso
library(glmnet);
my=glmnet(x,y); #lambda=0.015; #Gaussian
print(my); #results for all possible lambda
coef(my);
### cross validation
  my=cv.glmnet(x,y);
  plot(my);
  summary(my);
### LARS
  library(lars);
  my=lars(x,y,type="lasso", normalize=FALSE);
  plot(my); #again for all possible lambda

### multicollinearity
xx= t(x) %*% x;
e=eigen(xx);
kapa=e$values[1]/e$values[12];
mysvd=svd(xx)$d;
myratio=max(mysvd)/min(mysvd);

### PCA
mypca=prcomp(x);
b=mypca$sdev;
pom=b[1]/sum(b); #or squared?
c=mypca$sdev^2;
po=c[1]/sum(c);

### Khmaladze
library(quantreg); library(Qtools);
mydata=data.frame(x,y);
fit = rqProcess(y~x, data=mydata, taus=seq(0.1, 0.9, by=0.05));
  #warnings harmless
mytest=KhmaladzeTest(y~x, data=mydata, taus=seq(0.05, 0.95, by=0.01));
  #only test statistics are reported
class(mytest);
KhmaladzeFormat(mytest, epsilon=0.05);

### Khmaladze for subset
library(quantreg); library(Qtools);
#xmale=cbind(x[,3], x[,5], x[,6], x[,7], x[,12]);
  xmale=cbind(x[,5]);
mydata=data.frame(xmale,y);
#fit = rqProcess(y~xmale, data=mydata, taus=seq(0.1, 0.9, by=0.05));
  #warnings harmless
mytest=KhmaladzeTest(y~xmale, data=mydata, taus=seq(0.05, 0.95, by=0.01));
  #only test statistics are reported
class(mytest);
KhmaladzeFormat(mytest, epsilon=0.05);

### lasso estimates for regression quantiles
my=rq(y~x, method="lasso", lambda=10);

### graphical output
mytau=0.9;
eps=0.05;
ind=seq(from=0, to=10, by=0.333);
out=matrix(-1, nrow=length(ind), ncol=13);
pocty=rep(-1,length(ind));
for (k in 1:length(ind))
  {pom=rq(y~x, tau=mytau, method="lasso", lambda=ind[k]);
  out[k,]=pom$coef*(abs(pom$coef)>eps);
  pocty[k]=sum(abs(pom$coef)>eps); #plot(pocty)
}
n=length(ind);
sm=matrix(-1, nrow=n, ncol=2);
for (i in 1:n) {sm[i,1]=ind[i]; sm[i,2]=out[i,2];}
  x11() #prip. potlacit
  jpeg("Z:/moje9.jpg", height=5, width=5, units="in", res=600)
plot(sm, ylim=c(-0.7, 0.7), xlab=" ", ylab=" "); lines(sm[,1], out[,2]);
for (k in 3:13) {points(sm[,1], out[,k]); lines(sm[,1], out[,k]);}
  dev.off()

### graphical presentation of the importance of individual variables
### how the variables contribute to the fit by regression quantiles
fce=function(k)
{eps=0.05;
ind=seq(from=30, to=40, by=0.1);
n=length(ind);
out=matrix(-1, nrow=n, ncol=13);
pocty=rep(-1,n);
for (i in 1:n)
  {pom=rq(y~x, method="lasso", lambda=ind[i], tau=0.9);
  out[i,]=pom$coef*(abs(pom$coef)>eps);
 # pocty[i]=sum(abs(pom$coef)>eps); #plot(pocty)
}
sm=matrix(-1, nrow=n, ncol=2);
for (i in 1:n) {sm[i,1]=ind[i]; sm[i,2]=out[i,k];}
plot(sm, ylim=c(-0.4, 0.6), xlab=" ", ylab=" "); lines(sm[,1], out[,k]);
}
