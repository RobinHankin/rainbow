## This function plots a near-field raytracing diagram of a (2D)
## droplet, inspired by J. E. McDonald 1962. "Caustics of the primary
## rainbow", American Journal of Physics 31, p282

source("usefulrainbowfuncs.R")
options("refractive_index" = 4/3)


pdf(file="plot1.pdf")
descartes(xlim=c(-10,1),ylim=c(-5,1),lwd=0.1)
legend("bottomright",pch=NA,lty=1,col=c("red","blue"),legend=c("Cartesian ray","tangential ray"))
dev.off()
