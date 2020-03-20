## This function plots a near-field raytracing diagram of a (2D)
## droplet, inspired by J. E. McDonald 1962. "Caustics of the primary
## rainbow", American Journal of Physics 31, p282

source("usefulfuncs.R")
source("usefulrainbowfuncs.R")

n <- 4/3 # refractive index


plot(NA,xlab='',ylab='',asp=1,xlim=c(0.2,0.3),ylim=c(-1,-0.9))
#plot(NA,xlab='',ylab='',asp=1,xlim=c(-4,1),ylim=c(-3,1))

## First plot the bounding circle:
size <- 2.0
a <- seq(from=0,to=2*pi,len=1000)  # 'a' for angle
points(sin(a),cos(a),asp=1,type='l')

small <- 1e-9
#for(a in seq(from=0.52,to=sin(2*asin(1/n))-small,by=0.0005)){
for(a in seq(from=0.52,to=1-small,by=0.005)){
  drawray(a,lwd=0.2)
}

drawray(atan(1/n),col='red',lwd=1)
drawray(1-small,col='blue')
M <- f(1)
p <- M[3,1:2]
points(p[1],p[2],pch=16)
segments(
    p[1],p[2],
    p[1] + 10*p[2],
    p[2] - 10*p[1],
    col='blue')
             

## Axial line is indeterminate, has to be plotted explicitly:
abline(h=0)

## Mask outside of circle:
#mask(1.001)
