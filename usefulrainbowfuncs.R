## Fuctions here are called by rainbow.R

options("refractive_index" = 4/3)

## Minor trig ratios (not implemented in base R, for some reason):
`cosec` <- function(x){1/sin(x)}
`cot` <- function(x){1/tan(x)}
`sec` <- function(x){1/cos(x)}


## First some helper functions.  Function intersect() gives the
## intersection of line (y-y0)/(x-x0) = g with x^2+y^2=1.  It returns
## a two-row matrix with rows corresponding to Cartesian coordinates
## of the intersection points.

`intersect` <- function(v){
  x0 <- v[1]
  y0 <- v[2]
  g <- tan(v[3])

  A <- 1+g^2
  B <- 2*g*(y0-x0*g)
  C <- (y0-x0*g)^2-1

  x <- c(
      (-B+sqrt(B^2-4*A*C))/(2*A),
      (-B-sqrt(B^2-4*A*C))/(2*A)
  )

  y <- y0 + g*(x-x0)
  return(cbind(x,y))
}


## Draw a segment of a radial line
`bitofradial` <- function(x,y,inner=0.9,outer=1.1,...){
  segments(inner*x,inner*y,outer*x,outer*y,...)
}

## Function f() defined below takes a single argument d, the distance
## from optical axis of the drop to the incoming ray.  It returns a
## 3x3 matrix M with rows corresponding to the three refractive
## points. The Cartesian coordinates of point i are given by M[i,1:2]
## and the angle of the outgoing ray from that point is given by
## M[i,3].  Angles are given in radians in the sense defined by
## McDonald.  Note that this system is easily generalizable to the
## higher order bows.

## Notation follows McDonald's diagram 1 where possible, except the
## radius of the drop is unity: x^2+y^2=1.

## We follow the "Arbitrary ray".

`f` <- function(d, killreflect=FALSE){
  n <- getOption("refractive_index")
  i <- asin(d)
  
  M <- matrix(NA,3,3)
  colnames(M) <- c("x","y","angle")

  p1 <- c(-cos(i),sin(i))


  M[1,1] <- p1[1]  # x
  M[1,2] <- p1[2]  # y

  M[1,3] <- -i +asin(sin(i)/n)

  jj <- intersect(M[1,,drop=TRUE])
  dist_squared <- (jj[,1]-M[1,1])^2 + (jj[,2]-M[1,2])^2
  p2 <- jj[which.max(dist_squared),,drop=TRUE] 
  ## Now p2 is the coordinates of the second point.

  ## Fill in second row of M:
  M[2,1:2] <- p2
  M[2,3] <- -M[1,3]-2*atan2(p2[1],p2[2])

  jj <- intersect(M[2,,drop=TRUE])
  dist_squared <- (jj[,1]-M[2,1])^2 + (jj[,2]-M[2,2])^2
  p3 <- jj[which.max(dist_squared),,drop=TRUE] 
  ## Now p3 is the coordinates (x,y) of the third point.
  M[3,1:2] <- p3

  normal <- atan2(p3[2],p3[1])
  ## (interior) angle of incidence:
  incidence <- M[2,3]-normal
  if(abs(n*sin(incidence)) < 1){ # that is, if the ray can esape
      M[3,3] <- pi+(asin(n*sin(incidence)) +  normal)
  } else { # total internal reflection...
      if(killreflect){
          M[3,3] <- NA
      } else {
          M[3,3] <- normal -incidence
      }
  }      
  return(M)
}

`drawray` <- function(d,doreflect=TRUE,...){
  M <- f(d)
  segments(x0=-10,y0=d,x1=-sqrt(1-d^2),y1=d,...)
  segments(
      x0=M[1,1],y0=M[1,2],
      x1=M[2,1],y1=M[2,2],
      ...)
  segments(
      x0=M[2,1],y0=M[2,2],
      x1=M[3,1],y1=M[3,2],
      ...)

 ## separate treatment for total internal reflection:
  if(M[3,3]>0){
      l <- 10  # transmitted ray
  } else {
     l <- ifelse(doreflect,0.3,0)   # reflected ray
  }
  
  segments(
      x0=M[3,1],y0=M[3,2],
      x1=M[3,1]-l*cos(M[3,3]),
      y1=M[3,2]-l*sin(M[3,3]),
      ...)

   
#  bitofradial(M[1,1],M[1,2],lty=3)
#  bitofradial(M[2,1],M[2,2],lty=3)
#  bitofradial(M[3,1],M[3,2],lty=3)
}

tangential_ray <- function(...){
    small <- 1e-9
    ## Draw tangential ray
    drawray(1-small,col='blue',...)
    M <- f(1)
    p <- M[3,1:2]
    segments(
        p[1],p[2],
        p[1] + 10*p[2],
        p[2] - 10*p[1],
        col='blue',...)
}

`descartes` <- function(xlim=c(-5,1),ylim=c(-5,1),rays, doreflect=TRUE, dolegend=TRUE, ...){
    n <- getOption("refractive_index")
    small <- 1e-9  # nominal small value for numerical stability

    plot(NA,xlab='',ylab='',asp=1,axes=FALSE,xlim=xlim,ylim=ylim)

    ## plot droplet
    a <- seq(from=0,to=2*pi,len=1000)  # 'a' for angle
    points(sin(a),cos(a),type='l')

    ## Draw rays
    if(missing(rays)){rays <- seq(from=0.52,to=1-small,by=0.005)}
    for(a in rays){ drawray(a,doreflect=doreflect, ...) }
    ## Draw Cartesian, tangential, and maximal rays:
    drawray(atan(1/n),col='red',lwd=1)
    tangential_ray()
    drawray(sqrt(16/15-n^2/15),col="green",lwd=1)
    if(dolegend){
      legend("bottomright",pch=NA,lty=1,
             col=c("red","green","blue"),
             legend=c("Cartesian ray","extremal ray","tangential ray")
             )
    }
}  # function descartes() closes

`raydist` <- function(r,M){  # argument 'r' is the distance we follow
                             # the ray (which starts from M[1,]).
    ## This function uses the fourth column of M which gives the
    ## refractive index of the material for that leg

    n <- getOption("refractive_index")

    ## First, add start point for ray (initially moving horizontally):
    x <- rep(0,nrow(M)-1)
    for(i in seq_along(x)){
        x[i] <- (
            sqrt(sum((M[i,1:2]-M[i+1,1:2])^2)) # Cartesian distance...
            /M[i,4]  # ...divided by the refr.index for that leg
        ) 
    }
    x <- c(0,x)
    
    ## u gives how many points have we passed:
    u <- sum(r>cumsum(x))
    ## What is the last point we have passed?
    v <- M[u,,drop=TRUE]

    ## v[1:2] is the (x,y) coordinates of the last point we passed,
    ## and v[3] is the angle of the ray we are on.

    ## How far beyond v[1:2] are we?
    far <- r-sum(x[seq_len(u)])
    ## where are we?  We are at the last point plus a vector:
    if(u==4){v[3] <- v[3]-pi}
    return(v[1:2] + v[4]*far*c(cos(v[3]),sin(v[3])))
}   # raydist() closes

`fraunhofer` <- function(xlim=c(-2,1),ylim=c(-3,1),
                         bvals = seq(from=0,to=9,len=300),
                         dvals = seq(from=0.4,to=1,len=100),
                         cartesian = TRUE,
                         ...){

    n <- getOption("refractive_index")
    small <- 1e-9  # nominal small value for numerical stability
    
    plot(NA,xlab='',ylab='',asp=1,axes=FALSE,xlim=xlim,ylim=ylim)

    ## plot droplet
    a <- seq(from=0,to=2*pi,len=1000)  # 'a' for angle
    points(sin(a),cos(a),type='l')

    K <- matrix(NA,length(dvals),2)
    for(b in bvals){
        for(i in seq_along(dvals)){
            ## Add start point of ray to M:
            d <- dvals[i]
            ## ray starts horizontally at (-3,d):
            M <- rbind(c(-3,d,0),f(d,killreflect=TRUE))  
            
            ## Augment M with a fourth column giving the refractive index
            ## of the ray:
            M <- cbind(M,c(1,1/n,1/n,1)) # NB two n's
            K[i,] <- raydist(b,M)
        }
        points(K, type='l', lwd=0.4, ...)
    }

    ## Now draw the Cartesian,tangential, and maximal rays:
    if(cartesian){drawray(atan(1/n),col='red',lwd=1)}
    tangential_ray()
    drawray(sqrt(16/15-n^2/15),col="green")
    legend("bottomright",pch=NA,lty=1,
           col=c("red","green","blue"),
           legend=c("Cartesian ray","extremal ray","tangential ray")
           )
}  # function fraunhofer() closes
