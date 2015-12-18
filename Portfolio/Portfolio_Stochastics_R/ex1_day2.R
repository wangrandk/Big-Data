#Exercise 1 Day 2

#1) Simulate a sample from a Cauchy(0,1) distrib with inversion method
n=100000 # n big enough
u=runif(n,0,1)
h=seq(-5,5,.1)
f=1/(pi*(1+h^2)) 

#Plot the theoretical pdf of Cauchy(0,1)
txt="A sample from Cauchy(0,1) distribution"
png("d2e1_fig1.png")
plot(h, f, type="l",col=2, main=txt) 

#now we use inverse of cauchy cdf to create a sample of a rv 
# wich follows a Cauchy(0,1)pdf F(x)=y -> y=F^-1(x)
y=tan(pi*(u-0.5))

#Plot the histogram
lb = min(y)-0.2
ub = max(y)+0.2
hist(y, breaks = seq(lb,ub,0.2),add = T,prob=T)
dev.off()
##################################################
#################################################
#2) Simulate a sample from Beta(2,5) ditrib using rejection method with 
# a uniform distribution as auxiliar distribution.

n = 10000 
h = seq(0, 1, 0.01) #beta(2,5) pdf decays fast
K = max(dbeta(h,2,5)+0.1) #maximum of Beta pdf
#now we generate n random points in the box [0,1]x[0,K]
u1 = runif(n) 
u2 = runif(n)*K
#and select those ones below the target pdf
i = u2<dbeta(u1,2,5) 
x = u1[i]
#We plot the points in the left
png("d2e1_fig2.png",height=300)
par(mfrow=c(1,2))
plot(u1,u2,xlab='u1',ylab='u2',main='x:=u2<dbeta(u1,2,5)')
lines(u1[i],u2[i],type='p',col=2)

# x should be distributed following dbeta(2,5)
txt = "A sample from Beta(2,5)"
plot(h, dbeta(h,2,5), col=2,type="l", main=txt,xlab='x',ylab='f(x)')
hist(x,seq(-0.1,1.1,0.05),prob=T,add=T) #prob=T normalizes the hist (area=1)
                                        #add=T is a hold on 
dev.off()
#############################################################
#############################################################
#3) Simulate a sample from an N(0,1), distribution with the rejection method
# using the doble exponential distribution as auxiliar variable

h = seq(-5, 5, 0.01)
#how is this doble exp pdf?

png("d2e1_fig3.png")
plot(h, seq(0,1.0,length=length(h)),type='n',xlab='x',ylab='y',
     main="f, g and K*g")

lines(h,0.5*exp(-abs(h)),type='l',col=2) # auxiliar g case alpha=1
                         # symmetric, normalised
lines(h,dnorm(h,0,1),type='l',col=3,lwd=2) # f, target pdf
#we define K s.t. Kg(x)>f(x) forall x

K=max(abs(dnorm(h,0,1)/(0.5*exp(-abs(h)))))+0.5

#we rescale with this one
lines(h,K*0.5*exp(-abs(h)),col=2,lwd=2)
dev.off()

#Now we use rejection method
#We generate uniform numbers in the area under Kg(x)

n=10000
#1)First the X component following g(x)
u1 = c(-rexp(n/2,rate=1), rexp(n/2,rate=1))
#hist(u1,breaks=seq(min(u1)-0.1,max(u1)+0.1,0.2),add=T,prob=T)
#2) Y component is generated uniform in 0, Kg(x)
u2 = runif(n,0,K*0.5*exp(-abs(u1)))

#the pairs (u1,u2) are in the box [min(u1),max(u1)]x[0,K*g(u1)]
# we neglect those pairs above N(0,1)
i = u2 < dnorm(u1,0,1)
x = u1[i]
#Plot all sampled points
png("d2e1_fig4.png",height=300)
par(mfrow=c(1,2))

plot(h,K*0.5*exp(-abs(h)),col=2,lwd=2,type='l',xlab='x',ylab='y',
     main='Points, Rejection Method')
lines(h,dnorm(h,0,1),type='l',col=3,lwd=2)
points(u1,u2,pch=19,cex=0.2,col=2) 
#and the points that are inside the target distribution
points(x,u2[i],pch=19,cex=0.21,col=3)

# x must be normal distributed
txt = "Sampling N(0,1)"
plot(h, dnorm(h,0,1), type="l",xlab='x',ylab='Density', lwd=2,
     col=3, main=txt)
hist(x,seq(-5.1,5.1,0.5),prob=T,add=T)
dev.off()



#############################################
############################################
#4) basic experiment. Everytime we throw a needle it falls in 
# a planck at a given position (x,y), we can assume the plancks are
# infinity long and we just care about the intersection of vertical lines.

# we take the x randomly in [0,l]
n=10000
l=1
x=runif(n,0,l) #this is the sharp of the needel
#analogously the angle formed with with x-axis can be sampled 
#uniformly in [0,2pi]
theta=runif(n,0,2*pi)
#each pair (x,theta) characterize the needle. if the tail
#   x_tail = x + l*cos(theta)
#is out of the bouds [0,l] means that we have an intersection
# we count the needles that intersect the right line
right=sum(x+l*cos(theta)>l)
# and left line
left=sum(x+l*cos(theta)<0)
#the probability of intersection will be the favoral cases over the total
p_inter = (right+left)/n





