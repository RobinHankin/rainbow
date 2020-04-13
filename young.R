# Calculates intensity as a function of angle according to Young's
# (modified) theory.

## Reference is P. Laven 2017.  "Supernumerary arcs of rainbows:
## Young's theory of interference".  Applied Optics, 56 (19):G104-G111


source("usefulrainbowfuncs.R")
options("refractive_index" = 1.333)  # consistent with Laven 2017

`scattering_angle` <- function(d,n=getOption("refractive_index")){
    180 - raytracer(d,n=n)[3,3]*180/pi  # scattering angle in degrees
}

## Observe that, to 4 sig figs, fun(0.7287) = fun(0.9428) = 141, as
## per Fig 3 of Laven 2017.

`deriv_scattering_angle` <- function(d,small=1e-6){
    (scattering_angle(d+small/2)-scattering_angle(d-small/2))/small
}

# intensity is (classically) the absolute value of reciprocal of
# derivative of scattering angle:
`intensity` <- function(d){1/abs(deriv_scattering_angle(d))}

## fig3() is a helper function for drawfig3().  Given a scattering
## angle theta, it returns the two values of d [called 'b' by Laven]
## which result in a scattering angle of theta.  One is greater than
## b0~= 0.8608, and one is smaller.
`fig3` <- function(theta,n=getOption("refractive_index")){
    ## We seek b: fun(b)=theta; if theta=141, should get c(0.7287,0.9428)
    b0 <- sqrt((4-n^2)/3)  ## observe that fun(b0) = 139.92 as per Laven

    ## Further observe that if n=4/3 we get f(b0) = 139.97

    ## NB theta in radians
    
    f <- function(b){scattering_angle(b,n=n)-theta}

    c(
        uniroot(f,c(0,b0),tol=1e-13)$root,  # red ray in Laven's Fig 3
        uniroot(f,c(b0,1),tol=1e-13)$root   # blue ray in Laven's Fig 3
    )                                       # (cyan line is the Cartesian ray)

}

## Function drawfig3() plots Laven's figure 3:
`drawfig3` <- function(theta=141,n=getOption("refractive_index")){
    ## (could use descartes() here but we need more control)
    jj <- 1.3
    plot(NA,xlim=c(-jj,jj),ylim=c(-jj,jj),asp=1,axes=TRUE,xlab="",ylab="")
    drawdrop()
    cartesian_ray(col="cyan")
    jj <- fig3(theta)
    drawray(jj[1],col="blue")
    drawray(jj[2],col="red")

    thetarad <- theta*pi/180  # theta, in radians

    x0 <- cos(thetarad)
    y0 <- -sin(thetarad)
    l <- 5
    segments(-1, 0,-1     ,1      ,lty=2,col="gray") # start point 
    segments(x0,y0,x0-y0*l,y0+x0*l,lty=2,col="gray") # end point

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



## Function raypathlength_single() defined below is a sort of inverse
## to raydist().  Given impact parameter d, it returns the optical
## path length.  If light passes from air to glass, the frequency
## stays the same but the speed changes from c to c/n, so the
## wavelength changes from from lambda to lambda/n.  The path starts
## from (-1,d) and ends at the intersection of the ray and the second
## dotted line shown in drawfig3().
##
## NB Function raypathlength_single() does NOT account for the Gouy
## phase shift.

`raypathlength_single` <- function(d,n=getOption("refractive_index")){  
    M <- raytracer(d)   # get 'd' from fig3()

    ## First figure out where the exiting ray intersects the end line:
     
    theta <- pi-M[3,3]  # something close to 140 degrees or 2.443 rad

    ## equation for *normal* to outgoing light is (y-y0)/(x-x0) = g0:
    x0 <-  cos(theta)
    y0 <- -sin(theta)
    g0 <-  cot(theta)   # e.g.    abline(y0-g0*x0,g0,col='green')
    ## equation for outgoing light ray is (y-y1)/(x-x1) = g1:

    x1 <- M[3,1]
    y1 <- M[3,2]
    g1 <- tan(M[3,3])  # e.g.    abline(y1-g1*x1,g1,col='green')

    ## intersection point:
    x <- ((y0-y1) +g1*x1 -g0*x0)/(g1-g0)
    y <- y0 +g0*x -g0*x0
    l <- (
        + (1+M[1,1])                      *1  # initial ray,         in vacuo
        + sqrt(sum((M[1,1:2]-M[2,1:2])^2))*n  # first internal leg,  in water
        + sqrt(sum((M[2,1:2]-M[3,1:2])^2))*n  # second internal leg, in water
        + sqrt(sum((M[3,1:2]-c(x,y)  )^2))*1  # exit ray,            in vacuo
    )
    return(l)
}

## In function phase_diff(), theta is the exit angle in DEGREES;
## default theta=141.  lambda is the wavelength of the light divided
## by the radius of the drop.  Red light has lambda of 0.65
## micrometers and the drop has a notional radius of 100 um which
## gives lambda = 0.65/100 = 0.0065.

## Function phase_diff() returns the difference in phase between the
## two rays with scattering angle theta [see Laven's figure 3]

`phase_diff` <- function(theta=141,lambda=0.0065){

    jj <- fig3(theta)  # jj[1] is the red ray, jj[2] is the blue ray

    path1 <- (raypathlength_single(jj[1]) %% lambda) 
    ## Laven's Figure 7: phase advanced by pi/2 at A and B

    path2 <- (raypathlength_single(jj[2]) %% lambda) 
    ## Laven's Figure 8: phase advanced by pi/2 at A, B, and C

    delta <- (path1-path2)*2*pi
    return(1+sin(2*delta))
}

## eqn1() gives the phase delay phi between entrance and exit planes
## of the sphere; it is Laven's equation 1, p105.
`eqn1` <- function(d, roverlambda, n=getOption("refractive_index")){
    theta_i <- asin(d)
    theta_r <- asin(d/n)
    phase <- roverlambda*4*pi*(1-cos(theta_i) + 2*n*cos(theta_r))
    return(phase)
}


## 'ang' is the scattering angle
`fig10` <- function(ang, roverlambda=100/0.65){
    
    d2 <- t(sapply(ang,fig3))
    ## row "i" of d2 is the two values of 'd' that give scattering angle ang[i]
    
    int <- d2*NA
    int[] <- sapply(d2,intensity)
    ## row "i" of int is the intensities of the two rays
    
    path_length <- d2*NA
    path_length[] <- sapply(d2,eqn1,roverlambda=roverlambda)
    ## row "i" of path_length is the path lengths of the two rays
    
    ## The two rays interfere:
    intensity_interfered <- abs(
        int[,1]*exp(1i*(   path_length[,1])) + 
        int[,2]*exp(1i*(-1+path_length[,2]))
    )
    return(intensity_interfered)
}

ang <- seq(from=139,to=145,len=100)
plot(ang,fig10(ang))
abline(h=0)
