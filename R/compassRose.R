compassRose<-function(x,y,rot=0,cex=1) {
 oldcex<-par(cex=cex)
 mheight<-strheight("M")
 xylim<-par("usr")
 plotdim<-par("pin")
 xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1]
 point.angles<-seq(0,2*pi,by=pi/4)+pi*rot/180
 crspans<-rep(c(mheight*3,mheight/2),length.out=9)
 xpoints<-cos(point.angles)*crspans*xmult+x
 ypoints<-sin(point.angles)*crspans+y
 for(point in 1:8) {
  pcol<-ifelse(point%%2,"black","white")
  polygon(c(xpoints[c(point,point+1)],x),c(ypoints[c(point,point+1)],y),col=pcol)
 }
 txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.2*crspans[1]*xmult+x
 txtypoints<-sin(point.angles[c(1,3,5,7)])*1.2*crspans[1]+y
 text(txtxpoints,txtypoints,c("E","N","W","S"))
 par(oldcex)
}
