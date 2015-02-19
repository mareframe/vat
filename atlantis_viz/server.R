# server.R
# Load packages
library("ggplot2")
library("stringr")
library("scales")
library("dplyr")


# create static plot  -------------------
numboxes <- length(grep("# Box number", bgm))

# extract the box vertices
vertices <- data.frame()
for(i in 1:numboxes){
  vert_tmp <- grep(paste("box", i - 1, ".vert ", sep = ""), bgm)
  vertices <- rbind(vertices, cbind(i - 1, bgm[vert_tmp]))
}

# extract lat and long
coords_tmp <- str_split(vertices$V2, pattern = " ")
x <- sapply(coords_tmp,`[`,2)
y <- sapply(coords_tmp,`[`,3)

# recombine into data.frame
map_base <- data.frame(boxid = vertices$V1, x = x, y = y)
map_base$x <- as.double(as.character(map_base$x))
map_base$y <- as.double(as.character(map_base$y))

shinyServer(
  function(input, output, session) {
    # Olive-type map
    output$map <- renderPlot({
    tmp <- vars[[input$var]]
    if(length(dim(tmp)) == 3){
      tmp <- vars[[input$var]][input$layer,,input$time]
    } else tmp <- vars[[input$var]][,input$time]
    
      # This is for Iceland Atlantis only
      # boxes 19 and 52 are islands, so color code them differently
      if(length(tmp) == 53)
        tmp[c(20,53)] <- NA
      data_tmp <- data.frame(boxid = 0:(numboxes - 1), tmp)
      
      unagg_map_data <- merge(map_base, data_tmp)
      ggplot(data = unagg_map_data, aes(x = x, y = y)) +
       geom_polygon(aes(group = boxid, fill = tmp), colour = "black") +
       theme_bw() + xlab("Longitude") + ylab("Latitude") +
       scale_fill_gradient2(low = muted("blue"), high = muted("cornflowerblue")) +
       theme(legend.title=element_blank()) 
      })
    
    output$agg_image <- renderImage({
      filename <- normalizePath(file.path('./images',
                                          paste(input$aggbio, "-aggbio.gif", sep = "")))
      list(src = filename)
      }, deleteFile = FALSE)
    
    # Relative biomass map
    output$rel_map <- renderPlot({
      qplot(y = relative[[input$rel_var]], x = Time, data = relative, geom = "line") +
        ylab("") +  theme_bw()
    })

    # SSB map
   output$ssb_map <- renderPlot({
      qplot(y = ssb[[input$ssb_var]], x = Time, data = ssb, geom = "line") +
        ylab("") +  theme_bw()
    }) 
   # YOY map
   output$yoy_map <- renderPlot({
      qplot(y = yoy[[input$yoy_var]], x = Time, data = yoy, geom = "line") +
      ylab("") +  theme_bw()
    })
   
   output$epibenthic <- renderPlot({
     ggplot(epi.m, aes(variable, Predator, fill = value)) + geom_tile() + 
       scale_fill_gradient(low = "blue",  high = "yellow") + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Prey") + ggtitle("Epibenthic")
   })
   
   output$wc <- renderPlot({
    ggplot(wc.m, aes(variable, Predator, fill = value)) + geom_tile() + 
       scale_fill_gradient(low = "blue",  high = "yellow") + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Prey") + ggtitle("Water Column")
   })
   
   output$sed <- renderPlot({
     ggplot(sed.m, aes(variable, Predator, fill = value)) + geom_tile() + 
       scale_fill_gradient(low = "blue",  high = "yellow") + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Prey") + ggtitle("Sediment")
   })
  
   output$structn <- renderPlot({
     dat_sn <- structN[grep(input$sn, structN$.id),]
     qplot(y = V1, x = Time, group = .id, color = .id, data = dat_sn, geom = "line") +  
       scale_x_continuous(breaks=seq(round(min(dat_sn$Time)), round(max(dat_sn$Time)), 5)) + ylab("Structural Nitrogen (mg N)") +  theme(legend.position="none")
   })
   
   output$reserven <- renderPlot({
     dat_rn <- reserveN[grep(input$sn, reserveN$.id),]
     qplot(y = V1, x = Time, group = .id, color = .id, data = dat_rn, geom = "line") +  
       scale_x_continuous(breaks=seq(round(min(dat_rn$Time)), round(max(dat_rn$Time)), 5)) + ylab("Reserve Nitrogen (mg N)") +  theme(legend.position="none")
   })
   
   output$totalnum <- renderPlot({
     dat_totn <- totalnums[grep(input$sn, totalnums$.id),]
     qplot(y = V1, x = Time, group = .id, color = .id, data = dat_totn, geom = "line") +  
       scale_x_continuous(breaks=seq(round(min(dat_totn$Time)), round(max(dat_totn$Time)), 5)) + ylab("Total Numbers") +  theme(legend.position="none")
   })
   
   output$totalbio <- renderPlot({
     sn <- structN[grep(input$sn, structN$.id),]
     dat_tn <- totalnums[grep(input$sn, totalnums$.id),]
     dat_tn$V1 <- (sn$V1*5.7*20/1000000000000) * dat_tn$V1
     qplot(y = V1, x = Time, group = .id, color = .id, data = dat_tn, geom = "line") +  
       scale_x_continuous(breaks=seq(round(min(dat_tn$Time)), round(max(dat_tn$Time)), 5)) + ylab("Total Biomass (Thousand Tons)") +  theme(legend.position="none")
   })
   
  })
