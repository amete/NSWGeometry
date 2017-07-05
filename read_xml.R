require(XML)
# i.e. see http://www.informit.com/articles/article.aspx?p=2215520 for reading XMLs

# Controls
draw.nsw <- T
draw.pads <- F
#nn_glob <- 8   

# Define a rotation matrix to help w/ plotting later on
rotate <- function(M, angle = pi/16.) {
    rotmatrix <- matrix(c(cos(angle),sin(angle),-sin(angle),cos(angle)),ncol=2)
    M2 <- t(rotmatrix %*% t(M))
    return(M2)
}

# Cartesian to polar coordinates, note singularities!
cart2pol <- function(x, y, z) {
    r     <- sqrt(x**2+y**2+z**2)
    theta <- atan(y/x)
    phi   <- atan(sqrt(x**2+y**2)/z)
    return(list(r=r,theta=theta,phi=phi))
}

# Get radius for a given eta and z
get_radius <- function(eta, z) {
    theta <- 2*atan(exp(-eta))
    return(z*tan(theta))
}

# Read in the XML
data <- xmlTreeParse("NSW_sTGC_r19.xml", useInternal = TRUE)
xmltop <- xmlRoot(data)

# xmltop[[3]][[1]][[i]] 
# i => 2-8 Small Pivot
# i => 9-15 Small Confirm
# i => 16-22 Large Pivot
# i => 23-29 Large Confirm

# Functions that draws the NSW based on the XML input

# Draw pads
draw_pads <- function(xmltop, index, layer=1, col="black", angle=0.) {

    # index corresponds to a specific module, i.e. sTG1-QL1P etc.
    sWidth      <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[index]],"sWidth"))
    lWidth      <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[index]],"lWidth"))
    Length      <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[index]],"Length"))
    yCutout     <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[index]],"yCutout"))
    radius      <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[index+1]],"radius"))
    sPadWidth   <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[index]][[1]],"sPadWidth"))
    lPadWidth   <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[index]][[1]],"sPadWidth"))
    padH        <- as.numeric(strsplit(xmlGetAttr(xmltop[[3]][[1]][[index]][[1]],"padH"),";")[[1]][[layer]])
    nPadH       <- as.numeric(strsplit(xmlGetAttr(xmltop[[3]][[1]][[index]][[1]],"nPadH"),";")[[1]][[layer]])
    rankPadH    <- as.numeric(strsplit(xmlGetAttr(xmltop[[3]][[1]][[index]][[1]],"rankPadH"),";")[[1]][[layer]])
    firstPadH   <- as.numeric(strsplit(xmlGetAttr(xmltop[[3]][[1]][[index]][[1]],"firstPadH"),";")[[1]][[layer]])
    rankPadPhi  <- as.numeric(strsplit(xmlGetAttr(xmltop[[3]][[1]][[index]][[1]],"rankPadPhi"),";")[[1]][[layer]]) # What is this?
    nPadPhi     <- as.numeric(strsplit(xmlGetAttr(xmltop[[3]][[1]][[index]][[1]],"nPadPhi"),";")[[1]][[layer]])
    anglePadPhi <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[index]][[1]],"anglePadPhi"))
    firstPadRow <- as.numeric(strsplit(xmlGetAttr(xmltop[[3]][[1]][[index]][[1]],"firstPadRow"),";")[[1]][[layer]])
    firstPadPhiDivision_A <- as.numeric(strsplit(xmlGetAttr(xmltop[[3]][[1]][[index]][[1]],"firstPadPhiDivision_A"),";")[[1]][[layer]])
    PadPhiShift_A <- as.numeric(strsplit(xmlGetAttr(xmltop[[3]][[1]][[index]][[1]],"PadPhiShift_A"),";")[[1]][[layer]])

    # Beginning of the first pad :  radius+y1-0.5*Length
    offset <- radius-0.5*Length
    tanT <- (0.5*(lWidth-sWidth))/(Length-yCutout)
    # Bottom corners
    xx <- c(-0.5*sWidth,0.5*sWidth)
    yy <- c(offset,offset)
    # Rotate if necessary
    if(angle!=0) {
        M1 <- cbind(xx,yy)
        M1R <- rotate(M1,angle)
        xx <- M1R[,1]
        yy <- M1R[,2]
    }
    lines(xx,yy,col=col)
    # First Pad needs special treatment
    xx <- firstPadH*tanT*c(-1,1)+c(-0.5*sWidth,0.5*sWidth)
    yy <- c(offset+firstPadH,offset+firstPadH)
    # Rotate if necessary
    if(angle!=0) {
        M1 <- cbind(xx,yy)
        M1R <- rotate(M1,angle)
        xx <- M1R[,1]
        yy <- M1R[,2]
    }
    lines(xx,yy,col=col)
    # Eta-lines
    for(i in seq(2,nPadH,by=1)) {
        value <- firstPadH+(i-1)*padH
        if(value > Length) value <- Length
        if(value < Length-yCutout) {
            xx <- value*tanT*c(-1,1)+c(-0.5*sWidth,0.5*sWidth)
            yy <- c(offset+value,offset+value)
        } else {
            xx <- c(-0.5*lWidth,0.5*lWidth)
            yy <- c(offset+value,offset+value)
        }
        # Rotate if necessary
        if(angle!=0) {
            M1 <- cbind(xx,yy)
            M1R <- rotate(M1,angle)
            xx <- M1R[,1]
            yy <- M1R[,2]
        }
        # Actual drawing
        lines(xx,yy,col=col) # The last row spills over!
    }
    # Phi-lines
    # Angle is measured from y-axis in counter-clockwise direction - needs special attention!
    #firstPadPhiDivision_A <- firstPadPhiDivision_A+pi/2.
    tanP <- tan(firstPadPhiDivision_A/180*pi)
    xx <- tanP*c(offset,offset+Length)
    yy <- c(offset,offset+Length)
    # Rotate if necessary
    if(angle!=0) {
        M1 <- cbind(xx,yy)
        M1R <- rotate(M1,angle)
        xx <- M1R[,1]
        yy <- M1R[,2]
    }
    # Actual drawing
    lines(xx,yy,col=col)
    if(nPadPhi>2) {
        for(i in seq(1,nPadPhi-2,by=1)) {
            tanP <- tan((firstPadPhiDivision_A+i*anglePadPhi)/180*pi)
            xx <- tanP*c(offset,offset+Length)
            yy <- c(offset,offset+Length)
            # Rotate if necessary
            if(angle!=0) {
                M1 <- cbind(xx,yy)
                M1R <- rotate(M1,angle)
                xx <- M1R[,1]
                yy <- M1R[,2]
            }
            # Actual drawing
            lines(xx,yy,col=col)
        }
    }
}

# Module corners
get_module_corners <- function(xmltop, idx) {
    # Get the module corners and return
    sWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[idx]],"sWidth"))
    lWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[idx]],"lWidth"))
    Length  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[idx]],"Length"))
    yCutout <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[idx]],"yCutout"))
    radius  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[idx+1]],"radius"))
    if(yCutout>0) {
        x <- c(sWidth*0.5,-sWidth*0.5,-lWidth*0.5,-lWidth*0.5,lWidth*0.5,lWidth*0.5)
        y <- c(0.,0.,Length-yCutout,Length,Length,Length-yCutout)
        y <- radius+y-0.5*Length
    } else {
        x <- c(sWidth*0.5,-sWidth*0.5,-lWidth*0.5,lWidth*0.5)
        y <- c(0.,0.,Length,Length)
        y <- radius+y-0.5*Length  
    }
    return(list(x=x,y=y))
}

# Get the z-position
get_z_position <- function(xmlroot,quad = "LP", layer = 1) {
    # Get center
    if("LP" %in% quad)      { module.center <-  as.numeric(xmlGetAttr(xmltop[[2]][[29]],"value")) }
    else if("LC" %in% quad) { module.center <-  as.numeric(xmlGetAttr(xmltop[[2]][[31]],"value")) }
    else if("SP" %in% quad) { module.center <-  as.numeric(xmlGetAttr(xmltop[[2]][[25]],"value")) }
    else if("SC" %in% quad) { module.center <-  as.numeric(xmlGetAttr(xmltop[[2]][[27]],"value")) }
    else {
        print("Unknown z position config...")
        module.center <- -99999
    }
    # Get offset
    offset <- as.numeric(strsplit(xmlGetAttr(xmltop[[2]][[33]],"values"),";")[[1]][[layer]])
    return(module.center+offset)
}

# Large sector - pivot
draw_large_sectors <- function(xmltop, firstOnly = F, with.pads = F, layer = 1) {
    #
    # Large Sectors
    #

    # sTG1-QL1P
    corners <- get_module_corners(xmltop,17)
    x1 <- corners$x
    y1 <- corners$y
    z1 <- get_z_position(xmltop, quad = "LP", layer = layer)
    # sTG1-QL2P
    corners <- get_module_corners(xmltop,19)
    x2 <- corners$x
    y2 <- corners$y
    z2 <- get_z_position(xmltop, quad = "LP", layer = layer)
    # sTG1-QL3P
    corners <- get_module_corners(xmltop,21)
    x3 <- corners$x
    y3 <- corners$y
    z3 <- get_z_position(xmltop, quad = "LP", layer = layer)

    # Draw
    for(i in seq(1,16,by=2)) {
        if(firstOnly && i>1 ) break
        angle <- 2*pi/16*(i-1)
        # Draw sTG1-QS1P
        # Draw sTG1-QS1P
        # Draw sTG1-QS1P
        M1 <- cbind(x1,y1)
        M1R <- rotate(M1,angle)
        # points(M1R[,1], M1R[,2], col="blue", cex = 0.1, pch=19)
        polygon(M1R[,1],M1R[,2], col=rgb(0.54, 0.81, 0.94, 0.5))
        # polygon(M1R[,1],M1R[,2], col=rainbow(16)[(i-nn_glob*2)%%16]) 
        # Pads
        if(with.pads) {
            draw_pads(xmltop,index=17,layer=layer,col="black",angle=angle)
        }
        # Draw sTG1-QS2P
        # Draw sTG1-QS2P
        # Draw sTG1-QS2P
        M2 <- cbind(x2,y2)
        M2R <- rotate(M2,angle)
        # points(M2R[,1], M2R[,2], col="green", cex = 0.1, pch=19)
        polygon(M2R[,1],M2R[,2], col=rgb(1, 0.65, 0, 0.5)) # orange
        # polygon(M2R[,1],M2R[,2], col=rgb(0.54, 0.81, 0.94, 0.5))
        # polygon(M2R[,1],M2R[,2], col=rainbow(16)[(i-nn_glob*2)%%16]) 
        # Pads
        if(with.pads) {
            draw_pads(xmltop,index=19,layer=layer,col="black",angle=angle)
        }
        # Draw sTG1-QS2P
        # Draw sTG1-QS2P
        # Draw sTG1-QS2P
        M3 <- cbind(x3,y3)
        M3R <- rotate(M3,angle)
        # points(M3R[,1], M3R[,2], col="red", cex = 0.1, pch=19)
        polygon(M3R[,1],M3R[,2], col=rgb(0.54, 0.81, 0.94, 0.5))
        # polygon(M3R[,1],M3R[,2], col=rainbow(16)[(i-nn_glob*2)%%16]) 
        # Pads
        if(with.pads) {
            draw_pads(xmltop,index=21,layer=layer,col="black",angle=angle)
        }
    }
}

# Small sector - pivot
draw_small_sectors <- function(xmltop, firstOnly = F, with.pads = F, layer = 1) {
    #
    # Small Sectors
    #
    
    # Draw sTG1-QS1P
    corners <- get_module_corners(xmltop,3)
    x1 <- corners$x
    y1 <- corners$y
    z1 <- get_z_position(xmltop, quad = "SP", layer = layer)
    # Draw sTG1-QS2P
    corners <- get_module_corners(xmltop,5)
    x2 <- corners$x
    y2 <- corners$y
    z2 <- get_z_position(xmltop, quad = "SP", layer = layer)
    # Draw sTG1-QS3P
    corners <- get_module_corners(xmltop,7)
    x3 <- corners$x
    y3 <- corners$y
    z3 <- get_z_position(xmltop, quad = "SP", layer = layer)

    # Small Sectors
    for(i in seq(1,16,by=2)) {
        if(firstOnly && i>1 ) break
        angle <- 2*pi/16*(i)
        if(firstOnly) angle <- 0 # Don't rotate
        # Draw sTG1-QS1P
        # Draw sTG1-QS1P
        # Draw sTG1-QS1P
        M1 <- cbind(x1,y1)
        M1R <- rotate(M1,angle)
        # points(M1R[,1], M1R[,2], col="blue", cex = 0.1, pch=19)
        polygon(M1R[,1],M1R[,2], col=rgb(1, 0.65, 0, 0.5))
        # polygon(M1R[,1],M1R[,2], col=rgb(0.54, 0.81, 0.94, 0.5))
        # polygon(M1R[,1],M1R[,2], col=rainbow(16)[(i+nn_glob*2)%%16]) 
        # Pads
        if(with.pads) {
            draw_pads(xmltop,index=3,layer=layer,col="black",angle=angle)
        }
        # Draw sTG1-QS2P
        # Draw sTG1-QS2P
        # Draw sTG1-QS2P
        M2 <- cbind(x2,y2)
        M2R <- rotate(M2,angle)
        # points(M2R[,1], M2R[,2], col="green", cex = 0.1, pch=19)
        polygon(M2R[,1],M2R[,2], col=rgb(0.54, 0.81, 0.94, 0.5)) # blue
        # polygon(M2R[,1],M2R[,2], col=rainbow(16)[(i+nn_glob*2)%%16]) 
        # Pads
        if(with.pads) {
            draw_pads(xmltop,index=5,layer=layer,col="black",angle=angle)
        }
        # Draw sTG1-QS3P
        # Draw sTG1-QS3P
        # Draw sTG1-QS3P
        M3 <- cbind(x3,y3)
        M3R <- rotate(M3,angle)
        # points(M3R[,1], M3R[,2], col="red", cex = 0.1, pch=19)
        polygon(M3R[,1],M3R[,2], col=rgb(1, 0.65, 0, 0.5))
        # polygon(M3R[,1],M3R[,2], col=rgb(0.54, 0.81, 0.94, 0.5))
        # polygon(M3R[,1],M3R[,2], col=rainbow(16)[(i+nn_glob*2)%%16]) 
        # Pads
        if(with.pads) {
            draw_pads(xmltop,index=7,layer=layer,col="black",angle=angle)
        }
    }
}

# Draw the entire NSW
draw_nsw <- function(xmltop) {
    
    # First plot the canvas
    pdf("SingleWheelPivot_r19.pdf")
    plot(0, 0, xlim = c(-5000,5000), ylim = c(-5000,5000), type="n",
         main = "ATLAS NSW Layout", xlab = "x [mm]", ylab = "y [mm]")
    grid(nx = 30)
    
    # Plot the large sectors
    draw_large_sectors(xmltop, with.pads = T) 
    
    # Plot the small sectors
    draw_small_sectors(xmltop, with.pads = T) 
    
    # Close the device    
    dev.off()
}

if(draw.nsw) {
    draw_nsw(xmltop)
}

# Main control function for drawing the pads
if(draw.pads) {
 
    # Draw eta lines
    draw.eta.lines <- F
       
    # First plot the canvas
    pdf("LargeSectorPivot_L1_r19.pdf")
    plot(0, 0, xlim = c(-2000,2000), ylim = c(500,5000), type="n",
         main = "ATLAS NSW Layout (Large Pivot - Layer 1)", xlab = "x [mm]", ylab = "y [mm]")
    grid(nx = 30)
    # pdf("SmallSectorPivot_L1_r19.pdf")
    # plot(0, 0, xlim = c(-2000,2000), ylim = c(500,5000), type="n",
    #      main = "ATLAS NSW Layout (Small Pivot - Layer 1)", xlab = "x [mm]", ylab = "y [mm]")
    # grid(nx = 30)
    
    # Plot the large sectors
    draw_large_sectors(xmltop,firstOnly = T, with.pads = T)
    # draw_small_sectors(xmltop,firstOnly = T, with.pads = T)
    
    # Draw eta circles
    if(draw.eta.lines) {
        require(plotrix)
        r.1 <- get_radius(1.45,7474.-16.45) # w.r.t. Large Pivot First Layer
        r.2 <- get_radius(1.90,7474.-16.45) # w.r.t. Large Pivot First Layer
        r.3 <- get_radius(2.40,7474.-16.45) # w.r.t. Large Pivot First Layer
        r.4 <- get_radius(3.00,7474.-16.45) # w.r.t. Large Pivot First Layer
        draw.circle(0,0,radius=r.1,border="red")
        text(x = 1750, y=4350., labels = expression(paste(eta," = 1.45")), col = "red")
        draw.circle(0,0,radius=r.2,border="red")
        text(x = 1500, y=2500., labels = expression(paste(eta," = 1.90")), col = "red")
        draw.circle(0,0,radius=r.3,border="red")
        text(x = 1050, y=1500., labels = expression(paste(eta," = 2.40")), col = "red")
        draw.circle(0,0,radius=r.4,border="red")
        text(x =  750, y= 750., labels = expression(paste(eta," = 3.00")), col = "red")
    }
        
    # Close the device    
    dev.off()   
}


# 
# # i.e. get simple global parameters, their names and values
# for(i in seq(1,xmlSize(xmltop[[2]]))) {
#     name  <- xmlGetAttr(xmltop[[2]][[i]],"name")
#     if(is.null(name)) next
#     # Different values per layer
#     if(grepl("NSW_sTGC_LayerDeltaZ",name) ||
#        grepl("NSW_sTGC_PadSide",name) ||
#        grepl("NSW_sTGC_StripSide",name)) {
#         values <- xmlGetAttr(xmltop[[2]][[i]],"values")
#         print(sprintf("%*s : %s",27,name,values))
#     }
#     # Single values
#     else if(grepl("NSW",name)) {
#         value <- xmlGetAttr(xmltop[[2]][[i]],"value")
#         print(sprintf("%*s : %s",27,name,value))
#     } 
# }
