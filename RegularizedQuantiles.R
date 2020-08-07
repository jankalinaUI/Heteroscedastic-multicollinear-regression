### a deeper comparison of regression quantiles (RQ) and RQ-lasso (i.e. regularized version of RQ, 
### which seems perspective, but does not contain any tools for the variable selection in R)
### again for the same dataset

### quantile regression
  taus=0.1;
    #taus=rep(1:5)/5-0.1; #5 values
  my=rq(y~x, tau=taus); #here, lower and upper bounds are given
  summary(rq(y~x, tau=taus)); #conf.int. for more values of tau
  ### redundant regressors are sequentially discarded, forward search

### perform the same only for subsets
  taus=0.9; 
  index=c(6,10);
  xx=x[,index];
  my=rq(y~xx, tau=taus); #here, lower and upper bounds are given
  summary(rq(y~xx, tau=taus)); #conf.int. for more values of tau

### lasso estimates for regression quantiles
  my=rq(y~x, method="lasso", lambda=10);
  ### this is for a fixed lambda

### var. sel. for RQ-lasso for a particular tau and k
fce=function(tau=0.1, k=1) #k = regressor
{eps=0.01;
ind=seq(from=0, to=100, by=1);
n=length(ind);
out=matrix(-1, nrow=n, ncol=13);
pocty=rep(-1,n);
for (i in 1:n)
  {pom=rq(y~x, method="lasso", lambda=ind[i], tau=tau);
  out[i,]=pom$coef; #*(abs(pom$coef)>eps);
  # pocty[i]=sum(abs(pom$coef)>eps); #plot(pocty)
  }#for
print(out[,k]);
### now graph
  sm=matrix(-1, nrow=n, ncol=2);
  for (i in 1:n) {sm[i,1]=ind[i]; sm[i,2]=out[i,k];}
  plot(sm, ylim=c(-0.8, 10), xlab=" ", ylab=" "); lines(sm[,1], out[,k]);
}#fce

### bigger picture
mytau=0.5;
#eps=0.05;
ind=seq(from=0, to=50, by=1);
out=matrix(-1, nrow=length(ind), ncol=13);
pocty=rep(-1,length(ind));
for (k in 1:length(ind))
  {pom=rq(y~x, tau=mytau, method="lasso", lambda=ind[k]);
  out[k,]=pom$coef; #(abs(pom$coef)>eps);
 }
n=length(ind);
sm=matrix(-1, nrow=n, ncol=2);
for (i in 1:n) {sm[i,1]=ind[i]; sm[i,2]=out[i,2];} #regressor 1
  plot(sm, ylim=c(-0.7, 0.7), xlab=" ", ylab=" "); lines(sm[,1], out[,2]);
for (m in 3:13) {points(sm[,1], out[,m]); lines(sm[,1], out[,m]);} #other 11 regressors
  
### without manual steps
mytau=0.1;
eps=0.001;
ind=seq(from=0, to=50, by=0.05);
n=length(ind);
out=matrix(-1, nrow=length(ind), ncol=12);
pocty=rep(-1,length(ind));
for (k in 1:length(ind))
  {pom=rq(y~x, tau=mytau, method="lasso", lambda=ind[k]);
  out[k,]=pom$coef[2:13]; #(abs(pom$coef)>eps);
  print(k);
 }
mymax=rep(0,12);
for (j in 1:12)
  {plot(out[,j]); print(j); 
   for (i in 1:length(ind))
     {if (abs(out[i,j])>eps) mymax[j]=i;}
       print(c(j, mymax[j]));
  # readline();
}
order(mymax, decreasing=T); #a mame to

### graphical presentation 
#newgraf=function(mytau)
mytau=0.9;
eps=0.001;
ind=seq(from=0, to=200, by=0.1);
n=length(ind);
out=matrix(-1, nrow=length(ind), ncol=12);
pocty=rep(-1,length(ind));
for (k in 1:length(ind))
  {pom=rq(y~x, tau=mytau, method="lasso", lambda=ind[k]);
  out[k,]=pom$coef[2:13]; #(abs(pom$coef)>eps);
  print(k);
 }
m=sum(abs(out[1,]));
sm=rep(0,n); ymat=matrix(0, nrow=n, ncol=12);
for (i in 1:n)
  {sm[i]=sum(abs(out[i,]))/sum(abs(out[1,]));
   for (j in 1:12)
     ymat[i,j]=out[i,j];
  }#for
x11()
jpeg("U:/plot9.jpg", height=5, width=5, units="in", res=600);
plot(sm,ymat[,1], xlim=c(0,1), ylim=c(-0.6,0.7), xlab=" ", ylab=" ");   
for (j in 2:12) points(sm,ymat[,j]); 
for (j in 1:12) lines(sm, ymat[,j]);
dev.off()
#}#newgraf     





  