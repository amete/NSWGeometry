require(XML)
# i.e. see http://www.informit.com/articles/article.aspx?p=2215520 for reading XMLs

# Controls
draw.nsw <- F
draw.pads <- T

# Define a rotation matrix to help w/ plotting later on
rotate <- function(M, angle = pi/16.) {
    rotmatrix <- matrix(c(cos(angle),sin(angle),-sin(angle),cos(angle)),ncol=2)
    M2 <- t(rotmatrix %*% t(M))
    return(M2)
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

# Large sector - pivot
draw_large_sectors <- function(xmltop, firstOnly = F) {
    #
    # Large Sectors
    #
    
    # sTG1-QL1P
    sWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[17]],"sWidth"))
    lWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[17]],"lWidth"))
    Length  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[17]],"Length"))
    yCutout <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[17]],"yCutout"))
    radius  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[18]],"radius"))
    x1 <- c(sWidth*0.5,-sWidth*0.5,-lWidth*0.5,lWidth*0.5)
    y1 <- c(0.,0.,Length,Length)
    y1 <- radius+y1-0.5*Length
    # sTG1-QL2P
    sWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[19]],"sWidth"))
    lWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[19]],"lWidth"))
    Length  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[19]],"Length"))
    yCutout <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[19]],"yCutout"))
    radius  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[20]],"radius"))
    x2 <- c(sWidth*0.5,-sWidth*0.5,-lWidth*0.5,lWidth*0.5)
    y2 <- c(0.,0.,Length,Length)
    y2 <- radius+y2-0.5*Length
    # sTG1-QL3P
    sWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[21]],"sWidth"))
    lWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[21]],"lWidth"))
    Length  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[21]],"Length"))
    yCutout <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[21]],"yCutout"))
    radius  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[22]],"radius"))
    x3 <- c(sWidth*0.5,-sWidth*0.5,-lWidth*0.5,-lWidth*0.5,lWidth*0.5,lWidth*0.5)
    y3 <- c(0.,0.,Length-yCutout,Length,Length,Length-yCutout)
    y3 <- radius+y3-0.5*Length
    
    # Draw
    for(i in seq(1,16,by=2)) {
        if(firstOnly && i>1 ) break
        angle <- 2*pi/16*(i-1)
        # Draw sTG1-QS1P
        M1 <- cbind(x1,y1)
        M1R <- rotate(M1,angle)
        points(M1R[,1], M1R[,2], col="blue", cex = 0.1, pch=19)
        polygon(M1R[,1],M1R[,2], col=rgb(0, 0, 1, 0.5))
        # Draw sTG1-QS2P
        M2 <- cbind(x2,y2)
        M2R <- rotate(M2,angle)
        points(M2R[,1], M2R[,2], col="green", cex = 0.1, pch=19)
        polygon(M2R[,1],M2R[,2], col=rgb(0, 1, 0, 0.5))
        # Draw sTG1-QS3P
        M3 <- cbind(x3,y3)
        M3R <- rotate(M3,angle)
        points(M3R[,1], M3R[,2], col="red", cex = 0.1, pch=19)
        polygon(M3R[,1],M3R[,2], col=rgb(1, 0, 0, 0.5))
    }
}

# Small sector - pivot
draw_small_sectors <- function(xmltop) {
    #
    # Small Sectors
    #
    
    # Draw sTG1-QS1P
    sWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[3]],"sWidth"))
    lWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[3]],"lWidth"))
    Length  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[3]],"Length"))
    yCutout <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[3]],"yCutout"))
    radius  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[4]],"radius"))
    x1 <- c(sWidth*0.5,-sWidth*0.5,-lWidth*0.5,lWidth*0.5)
    y1 <- c(0.,0.,Length,Length)
    y1 <- radius+y1-0.5*Length
    # Draw sTG1-QS2P
    sWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[5]],"sWidth"))
    lWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[5]],"lWidth"))
    Length  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[5]],"Length"))
    yCutout <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[5]],"yCutout"))
    radius  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[6]],"radius"))
    x2 <- c(sWidth*0.5,-sWidth*0.5,-lWidth*0.5,lWidth*0.5)
    y2 <- c(0.,0.,Length,Length)
    y2 <- radius+y2-0.5*Length
    # Draw sTG1-QS3P
    sWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[7]],"sWidth"))
    lWidth  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[7]],"lWidth"))
    Length  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[7]],"Length"))
    yCutout <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[7]],"yCutout"))
    radius  <- as.numeric(xmlGetAttr(xmltop[[3]][[1]][[8]],"radius"))
    yCutout <- 0
    x3 <- c(sWidth*0.5,-sWidth*0.5,-lWidth*0.5,lWidth*0.5)
    y3 <- c(0.,0.,Length,Length)
    y3 <- radius+y3-0.5*Length
    
    # Small Sectors
    for(i in seq(1,16,by=2)) {
        angle <- 2*pi/16*(i)
        # Draw sTG1-QS1P
        M1 <- cbind(x1,y1)
        M1R <- rotate(M1,angle)
        points(M1R[,1], M1R[,2], col="blue", cex = 0.1, pch=19)
        polygon(M1R[,1],M1R[,2], col=rgb(0, 0, 1, 0.5))
        # Draw sTG1-QS2P
        M2 <- cbind(x2,y2)
        M2R <- rotate(M2,angle)
        points(M2R[,1], M2R[,2], col="green", cex = 0.1, pch=19)
        polygon(M2R[,1],M2R[,2], col=rgb(0, 1, 0, 0.5))
        # Draw sTG1-QS3P
        M3 <- cbind(x3,y3)
        M3R <- rotate(M3,angle)
        points(M3R[,1], M3R[,2], col="red", cex = 0.1, pch=19)
        polygon(M3R[,1],M3R[,2], col=rgb(1, 0, 0, 0.5))
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
    draw_large_sectors(xmltop)
    
    # Plot the small sectors
    draw_small_sectors(xmltop)

    # Close the device    
    dev.off()
}

if(draw.nsw) {
    draw_nsw(xmltop)
}

# Draw pads
draw_pads <- function(xmltop,index,layer=1,col="black") {

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
    lines(c(-0.5*sWidth,0.5*sWidth),c(offset,offset),col=col)
    lines(firstPadH*tanT*c(-1,1)+c(-0.5*sWidth,0.5*sWidth),c(offset+firstPadH,offset+firstPadH),col=col)
    # Eta-lines
    for(i in seq(2,nPadH,by=1)) {
        value <- firstPadH+(i-1)*padH
        if(value > Length) value <- Length
        if(value < Length-yCutout) {
            lines(value*tanT*c(-1,1)+c(-0.5*sWidth,0.5*sWidth),
                c(offset+value,offset+value),col=col) # The last row spills over!
        } else {
            lines(c(-0.5*lWidth,0.5*lWidth),
                  c(offset+value,offset+value),col=col) # The last row spills over!            
        }
    }
    # Phi-lines
    tanP <- tan(firstPadPhiDivision_A/180*pi)
    lines(tanP*c(offset,offset+Length),c(offset,offset+Length),col=col)
    for(i in seq(1,nPadPhi-2,by=1)) {
        tanP <- tan((firstPadPhiDivision_A+i*anglePadPhi)/180*pi)
        lines(tanP*c(offset,offset+Length),c(offset,offset+Length),col=col)
    }
}

if(draw.pads) {
    
    # First plot the canvas
    pdf("LargeSectorPivot_L1_r19.pdf")
    plot(0, 0, xlim = c(-2000,2000), ylim = c(500,5000), type="n",
         main = "ATLAS NSW Layout (Large Pivot - Layer 3)", xlab = "x [mm]", ylab = "y [mm]")
    grid(nx = 30)
    
    # Plot the large sectors
    draw_large_sectors(xmltop,T)
    
    # Draw the pads now 
    # - first layer only for the time being 
    # - also ignore the frames for the time-being

    # sTG1-QL1P
    draw_pads(xmltop,17,1)
    #draw_pads(xmltop,17,3,col="red")
    
    # sTG1-QL2P
    draw_pads(xmltop,19,1)
    #draw_pads(xmltop,19,3,col="blue")
    
    # sTG1-QL3P
    draw_pads(xmltop,21,1)
    #draw_pads(xmltop,21,3,col="green")
    
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