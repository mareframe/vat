#' The visualizing Atlantis toolbox
#' 
#' The visualizing Atlantis toolbox is an interactive Shiny-based toolbox. It includes various plots, both aggregrated and unaggregated, that are useful for diagnostic, tuning, and visualizing output. To use \code{vat}, the user must first run the \code{create_vat} function which will create an object of class \code{vat} which can be fed to the \code{vat} function. The user must also run the \code{vat_animate} to create the animated GIFs required my the function. If you don't want to use the animated plots, you can just make the \code{anim} option and the warning message. 
#' 
#' 
#'@param obj Object of class vat returned by create_vat
#'@param anim Directory to stored animated plot created by vat_animate function
#'@export
#'@seealso \code{\link{create_vat}}, \code{\link{vat_animate}}
#'\dontrun{
#' anim <- "/path/to/animinated/gifs"
#' obj <- create_vat(outdir = "/atlantis/output_dir/", fgfile = "/atlantis/functionalgroup.csv", ncout = "output_atlantis")
#' vat(obj, anim)
#'}

vat <- function(obj, anim){
  require("shiny")
  require("ggplot2")
  require("markdown")
  require("scales")
  shinyApp(
    ui = navbarPage("vat",theme = "http://cddesja.github.com/vat/theme/spacelab2.min.css",
                    # Starting "Welcome" Tab"
                    tabPanel("Welcome",
                             fluidRow(column(12,
                                             h1("Visualising Atlantis Toolbox", align = "center"))),
                             p(),
                             p(),
                             p(),
                             fluidRow(column(2),
                                      column(8,
                             includeMarkdown("http://cddesja.github.com/vat/markdown/iceatlantis.md")),
                             column(2)),
                    fluidRow(column(2),
                             column(1,
                                    img(src = "http://cddesja.github.com/vat/images/MareFrame-Logo.jpg")),
                             column(6),
                             column(1,
                                    img(src = "http://cddesja.github.com/vat/images/hi.gif", height = 45, width = 45)),
                             column(2))),
                    tabPanel("Plots to Display",
                             fluidRow(column(3),
                                      column(6,
                                             h3("Which plots should be displayed?", align = "center")),
                                      column(3)),
                             fluidRow(
                               column(3),
                               column(3, wellPanel(checkboxInput(
                                 "disagg", label = "Interactive spatial"))),
                               column(3, wellPanel(checkboxInput(
                                               "anim", 
                                               label = "Animated spatial"))),
                               column(3)),
                             fluidRow(
                               column(3),
                               column(3, wellPanel(checkboxInput(
                               "nitrogen", label = "Age disaggregated"))),
                               column(3, wellPanel(checkboxInput(
                                 "diet", 
                                 label = "Diet availability"))),
                               column(3)),
                             fluidRow(column(3),
                             column(3, wellPanel(checkboxInput(
                               "agg", label = "Aggregated/summary"))))),                  
                    # Disaggregated Spatial Maps
                    navbarMenu("Diaggregated Spatial Plots",
                               tabPanel("Interactive Spatial Plots",
                                        conditionalPanel(
                                          condition = "input.disagg == true",
                                        sidebarLayout(
                                          sidebarPanel(selectInput("disagg_var",
                                                                   label = "Choose an unaggregrated functional group to display",
                                                                   selected = obj$var_names[1],
                                                                   choices = obj$var_names),
                                                       sliderInput("layer", 
                                                                   label = "Choose a layer to display",
                                                                   min = 1,
                                                                   step = 1,
                                                                   max = obj$max_layers, 
                                                                   value = 1,
                                                                   round = TRUE),
                                                       sliderInput("time",
                                                                   label = "Choose a time to display",
                                                                   min = 1,
                                                                   step = 1,
                                                                   max = obj$max_time, 
                                                                   value = 1,
                                                                   round = TRUE)),
                                          mainPanel(
                                            plotOutput("map"))))),
                               tabPanel("Animated Spatial Plots",
                                        conditionalPanel(
                                          condition = "input.anim == true",
                                          column(5,
                                                          wellPanel(selectInput("aggbio",
                                                                   label = "Choose a functional group to display",
                                                                   selected = obj$bioagg_names[1],
                                                                   choices = obj$bioagg_names))),
                                                   column(7,
                                                          plotOutput("agg_image", inline = TRUE, "100%", "550px"))))),
                    
                    # The diagnostic plots UI
                    navbarMenu("Diagnostic Plots",
                               tabPanel("Age disaggregated plots",
                                        conditionalPanel(
                                          condition = "input.nitrogen == true",
                                        fluidRow(column(2, 
                                                        wellPanel(selectInput("sn",
                                                                              label = "Functional Group",
                                                                              choices = obj$rs_names))),
                                                 column(5,
                                                        plotOutput("structn", height = "300px")),
                                                 column(5,
                                                        plotOutput("reserven", height = "300px"))),
                                        fluidRow(column(2),
                                                 column(5,
                                                        plotOutput("totalnum", height = "300px")),
                                                 column(5,
                                                        plotOutput("totalbio", height = "300px"))))),
                               
                               tabPanel("Diet Plots",
                                        conditionalPanel(
                                          condition = "input.diet == true",
                                        fluidRow(column(3),
                                                 column(6,
                                                        wellPanel(selectInput("diet_var",
                                                          label = "Choose an unaggregrated functional group to display",
                                                          selected = NULL,
                                                          choices = unique(obj$diet_m$Habitat)))),
                                                 column(3)),
                                        fluidRow(column(12,
                                                        plotOutput("diet_matrix", height = "500px"))))),
                               
                               tabPanel("Aggregated Plots",
                                        conditionalPanel(
                                          condition = "input.agg == true",
                                         fluidRow(column(4,
                                                         wellPanel(
                                                           selectInput("rel_var",
                                                                       label = "Choose a relative biomass to display",
                                                                       choices = obj$rel_names))),
                                                  column(4,
                                                         wellPanel(
                                                           selectInput("ssb_var",
                                                                       label = "Choose a SSB biomass to display",
                                                                       choices = obj$ssb_names))),
                                                  column(4,
                                                         wellPanel(
                                                           selectInput("yoy_var",
                                                                       label = "Chose a YOY biomass to display",
                                                                       choices = obj$yoy_names))),
                                                  fluidRow(column(4,
                                                                  plotOutput("rel_map", height = "300px")),
                                                           column(4,
                                                                  plotOutput("ssb_map", height = "300px")),
                                                           column(4,
                                                                  plotOutput("yoy_map", height = "300px")))))))),
    server = function(input, output) {
      
      # Disaggregated spatial plot
      output$map <- renderPlot({
        tmp <- obj$disagg[[input$disagg_var]]
        if(length(dim(tmp)) == 3){
          tmp <- obj$disagg[[input$disagg_var]][input$layer,,input$time]
        } else tmp <- obj$disagg[[input$disagg_var]][,input$time]
        
        # Plot islands with a different color
        if(is.character(obj$islands)){
          islands <- as.numeric(obj$islands)
          tmp[islands + 1] <- NA
        }
        data_tmp <- data.frame(boxid = 0:(obj$numboxes - 1), tmp)
        
        unagg_map_data <- merge(obj$map_base, data_tmp)
        ggplot(data = unagg_map_data, aes(x = x, y = y)) +
          geom_polygon(aes(group = boxid, fill = tmp), colour = "black") +
          theme_bw() + xlab("Longitude") + ylab("Latitude") +
          scale_fill_gradient2(low = muted("blue"), high = muted("cornflowerblue")) +
          theme(legend.title=element_blank())})
      
      
      output$agg_image <- renderImage({
        filename <- normalizePath(file.path(anim,
                                            paste(input$aggbio, "-aggbio.gif", sep = "")))
        list(src = filename)
      }, deleteFile = FALSE)
      
      # Relative biomass map
      output$rel_map <- renderPlot({
        qplot(y = obj$rel_bio[[input$rel_var]], x = Time, data = obj$rel_bio, geom = "line") +
          ylab("") +  theme_bw()})
      
      # SSB map
      output$ssb_map <- renderPlot({
        qplot(y = obj$ssb[[input$ssb_var]], x = Time, data = obj$ssb, geom = "line") +
          ylab("") +  theme_bw()}) 
      
      # YOY map
      output$yoy_map <- renderPlot({
        qplot(y = obj$yoy[[input$yoy_var]], x = Time, data = obj$yoy, geom = "line") +
          ylab("") +  theme_bw()})
      
      # Diet matrix plot
      output$diet_matrix <- renderPlot({
        diet_dat <- subset(obj$diet_m, Habitat == input$diet_var)
        ggplot(diet_dat, aes(variable, Predator, fill = value)) + geom_tile() + 
          scale_fill_gradient(low = "blue",  high = "yellow") + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Prey")})
      
      # Structural nitrogen
      output$structn <- renderPlot({
        dat_sn <- obj$structN[grep(input$sn, obj$structN$.id),]
        qplot(y = V1, x = Time, group = .id, color = .id, data = dat_sn, geom = "line") +  
          scale_x_continuous(breaks=seq(round(min(dat_sn$Time)), round(max(dat_sn$Time)), 5)) + ylab("Structural Nitrogen (mg N)") +  theme(legend.position="none")})
      
      # Reserve nitrogen
      output$reserven <- renderPlot({
        dat_rn <- obj$reserveN[grep(input$sn, obj$reserveN$.id),]
        qplot(y = V1, x = Time, group = .id, color = .id, data = dat_rn, geom = "line") +  
          scale_x_continuous(breaks=seq(round(min(dat_rn$Time)), round(max(dat_rn$Time)), 5)) + ylab("Reserve Nitrogen (mg N)") +  theme(legend.position="none")})
      
      # Total Nums
      output$totalnum <- renderPlot({
        dat_totn <- obj$totalnums[grep(input$sn, obj$totalnums$.id),]
        qplot(y = V1, x = Time, group = .id, color = .id, data = dat_totn, geom = "line") +  
          scale_x_continuous(breaks=seq(round(min(dat_totn$Time)), round(max(dat_totn$Time)), 5)) + ylab("Total Numbers/Nitrogen (mg N)") +  theme(legend.position="none")})
      
      # Total Biomass
      output$totalbio <- renderPlot({
        sn <- obj$structN[grep(input$sn, obj$structN$.id),]
        dat_tn <- obj$totalnums[grep(input$sn, obj$totalnums$.id),]
        dat_tn$V1 <- (sn$V1*5.7*20/1000000000000) * dat_tn$V1
        qplot(y = V1, x = Time, group = .id, color = .id, data = dat_tn, geom = "line") +  
          scale_x_continuous(breaks=seq(round(min(dat_tn$Time)), round(max(dat_tn$Time)), 5)) + ylab("Total Biomass (Thousand Tons)") +  theme(legend.position="none")})
    }
  )
}