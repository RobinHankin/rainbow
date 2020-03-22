## This function plots a near-field raytracing diagram of a (2D)
## droplet, inspired by J. E. McDonald 1962. "Caustics of the primary
## rainbow", American Journal of Physics 31, p282

source("usefulrainbowfuncs.R")
options("refractive_index" = 4/3)

pdf(file="descartes1.pdf",height=9, width=9)
descartes(xlim=c(-10,1),ylim=c(-5,1),lwd=0.1)
dev.off()

pdf(file="descartes2.pdf",height=9, width=9)
descartes(xlim=c(-1,1),ylim=c(-1,1),lwd=0.1)
dev.off()

pdf(file="descartes3.pdf",height=9, width=9)
small <- 1e-9  # nominal small value for numerical stability
descartes(xlim=c(0.1,0.4),ylim=c(-1.1,-0.9),lwd=0.1,rays=seq(from=0.82,to=1-small,by=0.001))
dev.off()

pdf(file="plot4.pdf",height=9, width=9)
descartes(xlim=c(0.3,0.35),ylim=c(-1.1,-0.9),lwd=0.1,rays=seq(from=0.82,to=1-small,by=0.001),doreflect=FALSE)
dev.off()

pdf(file="plot4a.pdf",height=9, width=9)
descartes(xlim=c(0.5,1),ylim=c(0,0.5),lwd=0.1,dolegend=FALSE)
legend("topright",pch=NA,lty=1,
       col=c("red","green","blue"),
       legend=c("Cartesian ray","extremal ray","tangential ray")
       )
dev.off()

pdf(file="plot5.pdf",height=9, width=9)
fraunhofer()
dev.off()

pdf(file="plot6.pdf",height=9, width=9)
fraunhofer(xlim=c(-0.4,0.4),ylim=c(-1,-0.7), bvals = seq(from=5,to=15,len=400), dvals =  seq(from = 0.4, to = 1, len = 100) )
dev.off()

pdf(file="plot7.pdf",height=9, width=9)
fraunhofer(xlim=c(0.2,0.4),ylim=c(-1,-0.9), bvals = seq(from=6.3,to=6.8,len=100),dvals = seq(from = 0.4, to = 1, len = 9000))
dev.off()

pdf(file="plot8.pdf",height=9, width=9)
fraunhofer(xlim=c(0.2,0.3),ylim=c(-1,-0.94), bvals = seq(from=6.4,to=6.6,len=100),dvals = seq(from = 0.4, to = 1, len = 9000))
dev.off()
