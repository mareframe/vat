#' Animated Atlantis plots for vadt
#'
#' Create animated Atlantis plots of all the tracers
#' 
#' This function will create GIFs of all Atlantis tracers by default. WARNING: This can take a while and is not necessary to use \code{vadt}. It is probably much, much better to specify certain groups! This function depends on having the unix command convert installed locally. Convert is part of imagemagick and can be downloaded here: http://www.imagemagick.org/.
#' 
#' @param bgm An Atlantis bgm file
#' @param bmass BoxBiomass.txt file from Atlantis out 
#' @param interval Speed of animination (unit in seconds)
#' @param codes Vector of containing all the Atlantis functional group codes needed to create GIFs
#' @param savedir Where should the gifs be saved?
#' @import ggplot2
#' @importFrom animation ani.options
#' @importFrom animation saveGIF
#' @importFrom gridExtra grid.arrange
#' @export
#' @seealso \code{\link{create_vadt}}, \code{\link{vadt}}
#' @examples
#' \dontrun{
#' bgm <- "/path/to/atlantis.bgm"
#' bmass <- "/path/to/outputBoxBiomass.txt"
#' codes <- read.csv("functionalGroup.csv", header = T, stringsAsFactors = FALSE)
#' savedir <- "/home/chris/"
#' animate_vadt(bgm = bgm, bmass = bmass, codes = codes$Code, savedir = savedir)
#' }
#' 
animate_vadt <- function(bgm, bmass, interval = .3, codes, savedir){
  if(exists("savedir")  == FALSE)
    savedir <- getwd()
  bgm <- readLines(bgm)
  bio_agg <- read.table(bmass, header = T)
  
  colnames(bio_agg)[2] <- "boxid"
  numboxes <- length(grep("# Box number", bgm))
  
  codes <- bio_agg[,codes]
  bio_agg <- cbind(bio_agg[,1:2], codes)
  
  # Extract the box vertices
  vertices <- data.frame()
  for(i in 1:numboxes){
    vert_tmp <- grep(paste("box", i - 1, ".vert ", sep = ""), bgm)
    vertices <- rbind(vertices, cbind(i - 1, bgm[vert_tmp]))
  }
  
  
  # extract lat and long
  coords_tmp <- strsplit(as.character(vertices$V2), " ")
  x <- sapply(coords_tmp,`[`,2)
  y <- sapply(coords_tmp,`[`,3)
  
  # recombine into data.frame
  map_base <- data.frame(boxid = vertices$V1, x = x, y = y)
  map_base$x <- as.double(as.character(map_base$x))
  map_base$y <- as.double(as.character(map_base$y))
  islands <- grep("botz\t0", bgm, value = T)
  if(length(islands)>0){
    islands <- strsplit(islands, "[.]")
    islands <- sapply(islands,`[`, 1)
    islands <- strsplit(islands, "box")
    islands <- sapply(islands,`[`, 2)
  }
  
  ani.options(interval = interval, ani.height = 600, ani.width = 600)
  
  # Begin animation loop
  # Note the brackets within the parentheses
  for(j in colnames(bio_agg)[c(-1,-2)]){
    saveGIF({
    
      # For the most part, it’s safest to start with graphical settings in
      # the animation loop, as the loop adds a layer of complexity to
      # manipulating the graphs. For example, the layout specification needs to
      # be within animation loop to work properly.
      # layout(matrix(c(1, rep(2, 5)), 6, 1))
      max_value <- max(bio_agg[,j])
      min_value <- min(bio_agg[,j])
      mid_value <- median(bio_agg[,j])
      
      # Adjust the margins a little
      #  par(mar=c(4,4,2,1) + 0.1)
   for(i in unique(bio_agg$Time)){
        plot1 <- qplot(1,5) + coord_cartesian(xlim = c(100,max(bio_agg$Time))) + theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none", panel.background=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.line = element_line(colour = "black"), axis.line.y = element_blank()) +
          geom_vline(xintercept = i, size = 2, colour = "black") +
          geom_vline(xintercept = i - 10, size = 2, colour = "black") +
          geom_vline(xintercept = i - 20, size = 2, colour = "black") 
        
        agg_tmp <- bio_agg[bio_agg$Time == i, c("Time", "boxid", j)]
        agg_map_data <- merge(map_base, agg_tmp)
        
        if(is.character(islands)){
          islands <- as.numeric(islands)
          agg_map_data[agg_map_data$boxid %in% islands, j] <- NA
        }
        #y <- agg_map_data[,5] > 0
        #y <- agg_map_data[,5][y]
        names(agg_map_data) <- c("boxid", "x", "y", "Time", "box_fill")     
        plot2 <- ggplot(data = agg_map_data, aes(x = x, y = y, fill = box_fill)) + geom_polygon(aes(group = boxid, fill = box_fill), colour = "black") + theme_bw() + xlab("") + ylab("") + scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL)   + guides(fill = guide_legend(keywidth = 2, keyheight = 1, override.aes = list(colour = NULL))) + theme(legend.title=element_blank(), legend.position = "bottom") + scale_fill_gradient2(limits = c(min_value, max_value), high = "blue", low = "red", midpoint = mid_value) 
        
        grid.arrange(plot2, plot1, nrow=2, ncol=1, heights = c(5.5, .5))
   }}, movie.name = paste(savedir,j,"-aggbio.gif", sep = ""))
  }
}


