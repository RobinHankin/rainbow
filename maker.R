## This function plots a near-field raytracing diagram of a (2D)
## droplet, inspired by J. E. McDonald 1962. "Caustics of the primary
## rainbow", American Journal of Physics 31, p282

source("usefulrainbowfuncs.R")
options("refractive_index" = 4/3)


pdf(file="descartes1.pdf", title="Descartes figure 1",height=9, width=9)
descartes(xlim=c(-10,1),ylim=c(-5,1),lwd=0.1)
dev.off()

pdf(file="descartes2.pdf", title="Descartes figure 2", height=9, width=9)
descartes(xlim=c(-1,1),ylim=c(-1,1),lwd=0.1)
dev.off()

pdf(file="descartes3.pdf", title="Descartes figure 3", height=9, width=9)
small <- 1e-9  # nominal small value for numerical stability
descartes(xlim=c(0.1,0.4),ylim=c(-1.1,-0.9),
          lwd=0.1,dolegend=FALSE,
          rays=seq(from=0.82,to=1-small,by=0.001))
points(caustic(seq(from=0.93,to=1,len=100)),type="l",col="yellow")
legend("bottomright",pch=NA,lty=1,
       col=c("red","green","blue","yellow"),
       legend=c("Cartesian ray","extremal ray","tangential ray","caustic")
       )
dev.off()

pdf(file="descartes4.pdf", title="Descartes figure 4", height=9, width=9)
descartes(xlim=c(0.5,1),ylim=c(0,0.5),lwd=0.1,dolegend=FALSE)
n <- getOption("refractive_index")
points(caustic(seq(from=0,to=sqrt((4-n^2)/3),len=100),leg=2),type="l",col="yellow")
legend("topright",pch=NA,lty=1,
       col=c("red","green","blue","yellow"),
       legend=c("Cartesian ray","extremal ray","tangential ray","caustic")
       )
dev.off()

pdf(file="fraunhofer1.pdf", title="Fraunhofer figure 1", height=9, width=9)
fraunhofer()
dev.off()

pdf(file="fraunhofer2.pdf", title="Fraunhofer figure 2", height=9, width=9)
fraunhofer(xlim=c(-0.4,0.4), ylim=c(-1,-0.7), dolegend=FALSE,
           bvals = seq(from=5.5,to=7.5,len=200),
           dvals = sort(unique(c(
               seq(from = 0.40, to = 1, len = 100),
               seq(from = 0.96, to = 1, len = 100),
               seq(from = 0.99, to = 1, len = 10)))))
points(caustic(seq(from=0.93,to=1,len=100)),type="l",col="yellow")
legend("bottomright",pch=NA,lty=1,
       col=c("red","green","blue","yellow"),
       legend=c("Cartesian ray","extremal ray","tangential ray","caustic")
       )
dev.off()

pdf(file="fraunhofer3.pdf", title="Fraunhofer figure 3", height=9, width=9)
fraunhofer(xlim=c(0.2,0.4),ylim=c(-1,-0.9),
           bvals = seq(from=6.3,to=6.8,len=100),
           dvals = seq(from = 0.4, to = 1, len = 9000),
           dolegend=FALSE)
points(caustic(seq(from=0.93,to=1,len=100)),type="l",col="yellow")
legend("bottomright",pch=NA,lty=1,
       col=c("red","green","blue","yellow"),
       legend=c("Cartesian ray","extremal ray","tangential ray","caustic")
       )
dev.off()

pdf(file="fraunhofer4.pdf", title="Fraunhofer figure 4", height=9, width=9)
fraunhofer(xlim=c(0.2,0.3),ylim=c(-1,-0.94),dolegend=FALSE,
           bvals = seq(from=6.4,to=6.6,len=100),
           dvals = seq(from = 0.4, to = 1, len = 9000))
points(caustic(seq(from=0.93,to=1,len=100)),type="l",col="yellow")
legend("bottomright",pch=NA,lty=1,
       col=c("red","green","blue","yellow"),
       legend=c("Cartesian ray","extremal ray","tangential ray","caustic")
       )
dev.off()

pdf(file="mcdonald.pdf", title="McDonald",height=5, width=9)
mcdonald()
dev.off()

pdf(file="spectrum.pdf", title="spectrum (pictorial)",height=9,width=9)
spectrum()
dev.off()

pdf(file="spectrum_realistic.pdf", title="spectrum (realistic)", height=9,width=9)
spectrum(
    RI = seq(from = 1.331,  # RI for nominal red light, 700nm
             to   = 1.338,  # RI for nominal violet light, 425nm
             len = 100)
    )
dev.off()


