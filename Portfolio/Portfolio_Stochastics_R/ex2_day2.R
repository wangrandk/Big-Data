#Exercise 2 day 2
#5) sampling the waitining time for an offer>12 for my car
# we use Poisson(10) to simulate an offer -> time is a discrete variable
n=1000 # experiments
time=rep(1,n) #init of times
for (i in 1:n){
  while(rpois(1,10)<12) {
    time[i]=time[i]+1
  }
}
# Now we take the average time 
time_ave=sum(time)/n

#part2

n=1000 # experiments
time2=rep(1,n) #init of times
for (i in 1:n){
  while(rpois(1,10)<rpois(1,10)) {
    time2[i]=time2[i]+1
  }
}
# Now we take the average time 
time2_ave=sum(time2)/n

#Figure
png("d2e2_fig1.png",height=300)
par(mfrow=c(1,2))
hist(time,seq(0,max(time),1),prob=T)
lines(rep(time_ave,21),seq(0,0.25,0.25/20),col=2,lwd=3)

hist(time2,seq(0,max(time2),1),prob=T)
lines(rep(time2_ave,21),seq(0,0.25,0.25/20),col=2,lwd=3)

dev.off()


###################################################
###################################################
#6)Variance in the estimation of the median of a Poisson distribution
#a) simulate dataset of n=200 observ of Poisson(10)
n=200
dataset=rpois(n,10)
#estimation of the mean taking this sample
mu=mean(dataset)

#b)now we analyse
B=10000
X.theo <- X.boot <- matrix(nrow=n,ncol=B) # mat init
#we construct B independent samples using the theoret distrib and using sample comand
for(b in 1:B) {
  X.theo[,b] <- rpois(n,10)
  X.boot[,b] <- sample(x=dataset,size=n,replace=T)
}
#we compute the median for every column of these matrices, and we have a theo
# and a boot vector of means
mean.theo <- apply(X.theo,MARGIN=2,FUN=mean)
mean.boot <- apply(X.boot,MARGIN=2,FUN=mean)

#Lets compare the
png("d2e2_fig2.png")
par(mfrow=c(1,2)) # this is a subplot
txt1=''
plot(density(mean.boot),col=3,lwd=2,main=txt1,
     xlab='x',ylab='Density')
hist(mean.theo,prob=T,add=T)
lines(density(mean.theo),col=2,lwd=2)

hist(mean.boot,prob=T)
lines(density(mean.theo),col=2,lwd=2)
lines(density(mean.boot),col=3,lwd=2)
dev.off()

var(mean.theo)
var(mean.boot)

# they are different. Estimating V(mean) with the so-called bootstrap estimator
Var_boot = (1/B)*sum((mean.boot - (1/B)* sum(mean.boot))^2)
#this number is very close to var(mean.boot)

#######################################################################
######################################################################
#7)Simulate a sample of size 50000 from a bivariate centred, standarized 
# Gaussian vector with corr_coeff rho=0.7.

#we first create two independent normal distributed variables
n=50000
x1=rnorm(n,0,1)
x2=rnorm(n,0,1)
x=matrix(nrow=n,ncol=2) # x is nx2
x[,1]=x1;x[,2]=x2
xt=t(x) #xt is 2xn
# x must be uncorrelated and Var(x)=I_2
# we want y a bivariate sample with covariance matrix SIGMA
SIGMA=matrix(nrow=2,ncol=2)
SIGMA[1,1] <- SIGMA[2,2] <- 1
SIGMA[1,2] <- SIGMA[2,1] <- 0.7
# Cholesky factorization of SIGMA and y construction
U = chol(SIGMA) ; L = t(U)
yt=L %*% xt #y is 2xn
y=t(yt) # y is nx2

#now we plot the points for x and y
png("d2e1_fig3.png")
par(mfrow=c(1,2))
plot(x[,1],x[,2],main='original x',xlab='x',ylab='y')
plot(y[,1],y[,2],main='y=Lx',xlab='x',ylab='y')  #this looks good
dev.off()
var(y) # and the numbers are close to the target SIGMA

#checking the empirical bivariate density, Contour plot
#require(MASS)
#image(kde2d(y[,1],y[,2]),asp=T);points(y,cex=1,col=3)
#contour(kde2d(y[,1],y[,2]),add=T)

# Now we extract a sample of a random variable approximately 
# distributed as y2|y1=0.5.
#so we take y[i,j] matrix and extract y[,2] when y[,1]=0.5
i1=y[,1]>0.49
i2=y[,1]<0.51
i=i1*i2 # logical with position of the entries we want
tot=sum(i)
# we extract y[2,i] into z
z=vector(mode="numeric",length=tot) # we initialize
cont=1
for (j in 1:n){
  if(i[j]==1) {z[cont]=y[j,2];cont=cont+1}   
}
# Lets plot this sample
hist(z)
#and compare with the pdf of a bivariate with SIGMA and y1=0.5
SIGMA.INV=solve(SIGMA)
A=SIGMA.INV[1,1];B <- C <- SIGMA.INV[1,2];D=SIGMA.INV[2,2]

cte1=1/(2*pi)*(1/sqrt(det(SIGMA)))
cte2=exp(-0.5*(0.5^2)*A)
h=seq(-4,4,0.01)
Biv.y1.0p5=cte1*cte2*exp(-0.5*(0.5*h*(C+B) +h*h*D))
#In order to make the comparison we need to normailze this curve
Area=sum(0.01*Biv.y1.0p5) #
Theo=Biv.y1.0p5/Area
png("d2e2_fig4.png")
plot(h,Theo,type='l',col=2,lwd=2,
     main='Conditional distrib f(x|y=0.5)',xlab='x',ylab='Density')
hist(z,add=T,prob=T) # This looks pretty good :)
dev.off()

