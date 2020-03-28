# Calculates intensity as a function of angle according to Young's
# (modified) theory.
source("usefulrainbowfuncs.R")
options("refractive_index" = 1.333)  # consistent with Laven 2017

`scattering_angle` <- function(d,n=getOption("refractive_index")){
    180 - raytracer(d,n=n)[3,3]*180/pi  # scattering angle in degrees
}

## Observe that, to 4 sig figs, fun(0.7287) = fun(0.9428) = 141, as
## per Fig 3 of Laven 2017.


`fig3` <- function(theta,n=getOption("refractive_index")){
    ## seek b: fun(b)=theta; if theta=141, should get c(0.7287,0.9428)
    b0 <- sqrt((4-n^2)/3)  ## observe that fun(b0) = 139.92 as per Laven

    ## Further observe that if n=4/3 we get f(b0) = 139.97


    
    f <- function(b){scattering_angle(b,n=n)-theta}
    c(
        uniroot(f,c(0,b0))$root,
        uniroot(f,c(b0,1))$root
    )
}

`drawfig3` <- function(theta=141,n=getOption("refractive_index")){
    ## (could use descartes() here but we need more control)
    jj <- 1.3
    plot(NA,xlim=c(-jj,jj),ylim=c(-jj,jj),asp=1,axes=FALSE,xlab="",ylab="")
    drawdrop()
    cartesian_ray(col="cyan")
    jj <- fig3(theta)
    drawray(jj[1],col="blue")
    drawray(jj[2],col="red")

    thetarad <- theta*pi/180  # theta, in radians

    x0 <- cos(thetarad)
    y0 <- -sin(thetarad)
    l <- 5
    segments(-1,0,-1,1,lty=2,col="gray")  # start point 
    segments(x0,y0,x0-y0*l,y0+x0*l,lty=2,col="gray") # end point

    ## rays are assumed to be in phase at 

    points(caustic(
        seq(from=0,to=sqrt((4-n^2)/3),len=100),leg=2),
        type="l",col="yellow")
    points(caustic(
        seq(from=sqrt((4-n^2)/3),to=1,len=100),leg=1),
        type="l",col="yellow")
    points(caustic(
        seq(from=0.93,to=1,len=100)),
        type="l",col="yellow")
}


