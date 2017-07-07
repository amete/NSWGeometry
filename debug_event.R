# Load the event information
dataset <- read.csv("DebugEvent/event.csv")
plot.etaphi <- FALSE
save.plot <- FALSE

# Get radius for a given eta and z
get_radius <- function(eta, z) {
    theta <- 2*atan(exp(-eta))
    return(z*tan(theta))
    # alpha <- exp(-2*eta)
    # r     <- z*(1+alpha)/(1-alpha)
    # return(r*sin(acos(z/r)))
}

# Draw
if(plot.etaphi) {
    if(save.plot) {
        pdf("DebugEvent_pe.pdf")
    }
    plot(0, 0, xlim = c(-0.30,0.05), ylim = c(1.75,1.90), type="n",
         main = "Trigger Pads", xlab = "Phi [rad]", ylab = "Eta") 
} else {
    if(save.plot) {
        pdf("DebugEvent_xy.pdf")
    }
    plot(0, 0, xlim = c(2350,2650), ylim = c(-900,300), type="n",
         main = "Trigger Pads", xlab = "x [mm]", ylab = "y [mm]")
}
grid(nx = 30)

color.idx <- 0

# Loop over all 8 pads
for(idx in seq(1,8,by=1)) {
    if(idx<5) color.idx <- 1
    else color.idx <- 2
    pad.data <- dataset[dataset$Pad.idx == idx-1, ]
    x <- numeric()
    y <- numeric()
    z <- numeric()
    for(jdx in seq(1,4,by=1)) {
        corner.data <- pad.data[pad.data$Corner.idx == jdx-1,]
        x <- c(x,as.numeric(corner.data$x))
        y <- c(y,as.numeric(corner.data$y))
        z <- c(z,as.numeric(corner.data$z))
    }
    # For polygon convert 1,2,3,4 to 1,2,4,3
    x <- x[c(1,2,4,3)]
    y <- y[c(1,2,4,3)]
    z <- z[c(1,2,4,3)]
    # Compute Eta-Phi
    r           <- sqrt(x**2+y**2+z**2)
    theta       <- atan(sqrt(x**2+y**2)/z)
    phi         <- atan(y/x)
    cosTheta    <- z/r
    eta         <- -0.5*log( (1.0-cosTheta) / (1.0+cosTheta) )
    # Draw the polygon
    if(plot.etaphi) {
        polygon(phi, eta, col=rainbow(2,alpha=0.2)[color.idx])
    }
    else {
        polygon(x, y, col=rainbow(2,alpha=0.2)[color.idx])
    }
}

# Eta-lines
# require(plotrix)
# r.1 <- get_radius(1.826573,7452.33) # w.r.t. Large Pivot First Layer
# r.2 <- get_radius(1.858544,7452.33) # w.r.t. Large Pivot First Layer
# r.3 <- get_radius(1.818319,7452.33) # w.r.t. Large Pivot First Layer
# r.4 <- get_radius(1.850259,7452.33) # w.r.t. Large Pivot First Layer
# draw.circle(0,0,radius=r.1,border="red")
# draw.circle(0,0,radius=r.2,border="blue")
# draw.circle(0,0,radius=r.3,border="black")
# draw.circle(0,0,radius=r.4,border="black")

# Legend
legend("topright", inset=.05, title="Trigger Wedges",
       c("Inner","Outer"), fill=rainbow(2,alpha=0.5), horiz=TRUE)
if(save.plot) {
    dev.off()
}