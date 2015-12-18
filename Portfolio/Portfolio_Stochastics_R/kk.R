png("beta1.5_rowFig_.png",height= 250, width=700)
par(mfrow=c(1,3),pty = "s",mex=.8,pin=c(1.9,1.9),cex=0.7) 

image(seq(0,1,l=nx),seq(0,1,l=ny),x0,
      main="Reconst (Beta=2)",
      xlab="",ylab="",
      col=c("white","black"))

plot(seq(1,niter,1),c(0,rep(0.25,niter-1)),type='n',
     main='Pixdiff with prev iter', xlab='iter',ylab='#/ntot')
lines(seq(2,niter,1),PixDiff[2:niter]/ntot,type='l',col=1,lwd=2)

image(seq(0,1,l=nx),seq(0,1,l=ny),ChangingPix/max_ChPix,
      main=paste("ChangingPix after burnin ",burn.in),
      xlab="",ylab="",
      col=terrain.colors(8))
dev.off()
