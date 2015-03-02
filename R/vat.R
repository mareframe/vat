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
    ui = navbarPage("vat",
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
                    tabPanel("Atlantis Glossary",
                             dataTableOutput('fun_group_atl')),
                    tabPanel("Plots to Display",
                             
                             fluidRow(column(3),
                                      column(6,
                                             h3("Which plots should be displayed?", align = "center")),
                                      column(3)),
                             
                             fluidRow(column(2),
                                      column(4, wellPanel(checkboxInput(
                                        "disagg", label = "Interactive spatial biomass"))),
                                      column(4, wellPanel(checkboxInput(
                                        "anim", 
                                        label = "Animated spatial biomass"))),
                                      column(2)),
                             
                             fluidRow(column(2),
                                      column(4, wellPanel(checkboxInput(
                                        "nitrogen", label = "Age disaggregated"))),
                                      column(4, wellPanel(checkboxInput(
                                        "diet", 
                                        label = "Diet availability"))),
                                      column(2)),
                             
                             fluidRow(column(2),
                                      column(4, wellPanel(checkboxInput(
                                        "agg", label = "Vertebrate summaries"))),
                                      column(4, wellPanel(checkboxInput(
                                        "invert", label = "Non-vertebrate summaries"))),
                                      column(2))),     
                    
                    # Disaggregated Spatial Maps
                    navbarMenu("Spatial Plots",
                               tabPanel("Interactive Spatial Biomass",
                                        conditionalPanel(
                                          condition = "input.disagg == true",
                                        sidebarLayout(
                                          sidebarPanel(selectInput("disagg_var",
                                                                   label = "Functional Group",
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
                               tabPanel("Animated Spatial Biomass",
                                        conditionalPanel(
                                          condition = "input.anim == true",
                                          column(5,
                                                          wellPanel(selectInput("aggbio",
                                                                   label = "Functional Group",
                                                                   selected = obj$bioagg_names[1],
                                                                   choices = obj$bioagg_names))),
                                                   column(7,
                                                          plotOutput("agg_image", inline = TRUE, "100%", "550px"))))),
                    
                    # The diagnostic plots UI
                    navbarMenu("Diagnostic Plots",
                               tabPanel("Age Disaggregated",
                                        conditionalPanel(
                                          condition = "input.nitrogen == true",
                                          fluidRow(column(4),
                                                   column(4,wellPanel(selectInput("sn",
                                                                              label = "Functional Group",
                                                                              choices = obj$rs_names)))),
                                          fluidRow(column(1),
                                                   column(5,
                                                        plotOutput("structn", height = "300px")),
                                                 column(5,
                                                        plotOutput("reserven", height = "300px"))),
                                        fluidRow(column(1),
                                                 column(5,
                                                        plotOutput("totalnum", height = "300px")),
                                                 column(5,
                                                        plotOutput("totalbio", height = "300px")))))),
                               
                               tabPanel("Diet Availability",
                                        conditionalPanel(
                                          condition = "input.diet == true",
                                        fluidRow(column(3),
                                                 column(6,
                                                        wellPanel(selectInput("diet_var",
                                                          label = "Choose a habitat type to display",
                                                          selected = NULL,
                                                          choices = unique(obj$diet_m$Habitat)))),
                                                 column(3)),
                                        fluidRow(column(12,
                                                        plotOutput("diet_matrix", height = "500px"))))),
                               
                               tabPanel("Vertebrate Summaries",
                                        conditionalPanel(
                                          condition = "input.agg == true",
                                         fluidRow(column(4),
                                                  column(4,
                                                         wellPanel(
                                                           selectInput("ssb_var",
                                                                       label = "Functional Group",
                                                                       choices = obj$rs_names))),
                                                  column(4)),
                                                  fluidRow(column(1),
                                                           column(5,
                                                                  plotOutput("rel_map", height = "300px")),
                                                           column(5,
                                                                  plotOutput("tot_map", height = "300px"))),
                                                  fluidRow(column(1),
                                                           column(5,
                                                                  plotOutput("ssb_map", height = "300px")),
                                                           column(5,
                                                                  plotOutput("yoy_map", height = "300px"))))),
                               
                               tabPanel("Non-vertbrate Summaries",
                                        conditionalPanel(
                                          condition = "input.invert == true",
                                          fluidRow(column(4),
                                                   column(4,
                                                          wellPanel(
                                                            selectInput("invert_var",
                                                                        label = "Functional Group",
                                                                        choices = obj$invert_names$Name))),
                                                   column(4)),
                                          fluidRow(column(1),
                                                   column(5,
                                                          plotOutput("invert_rbio", height = "300px")),
                                                   column(5,
                                                          plotOutput("invert_tbio", height = "300px"))),
                                          fluidRow(column(1),
                                                   column(5,
                                                          plotOutput("invertgraze", height = "300px")),
                                                   column(5,
                                                          plotOutput("invertprod", height = "300px"))))))),
    server = function(input, output) {
      
      # Disaggregated spatial plot
      
      output$fun_group_atl = renderDataTable({
        obj$fun_group
      })
      
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
        qplot(y = obj$rel_bio[[match(input$ssb_var, names(obj$rel_bio))]], x = Time, data = obj$rel_bio, geom = "line") +
          ylab("") +  theme_bw() + ggtitle("Relative Biomass")}) 
      
      # Total biomass map
      output$tot_map <- renderPlot({
        qplot(y = obj$tot_bio[[match(input$ssb_var, names(obj$tot_bio))]], x = Time, data = obj$tot_bio, geom = "line") +
          ylab("") +  theme_bw() + ggtitle("Total Biomass")}) 
      
      # SSB map
      output$ssb_map <- renderPlot({
        qplot(y = obj$ssb[[match(input$ssb_var, names(obj$ssb))]], x = Time, data = obj$ssb, geom = "line") +
          ylab("") +  theme_bw() + ggtitle("Spawning Stock Biomass")}) 
      
      # YOY map
      output$yoy_map <- renderPlot({
        qplot(y = obj$yoy[[match(input$ssb_var, names(obj$yoy))]], x = Time, data = obj$yoy, geom = "line") +
          ylab("") +  theme_bw() + ggtitle("YOY Biomass")})
      
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
          scale_x_continuous(breaks=seq(round(min(dat_totn$Time)), round(max(dat_totn$Time)), 5)) + ylab("Total Numbers") +  theme(legend.position="none")})
      
      # Total Biomass
      output$totalbio <- renderPlot({
        sn <- obj$structN[grep(input$sn, obj$structN$.id),]
        dat_tn <- obj$totalnums[grep(input$sn, obj$totalnums$.id),]
        dat_tn$V1 <- (3.65*sn$V1*5.7*20/1000000000) * dat_tn$V1
        qplot(y = V1, x = Time, group = .id, color = .id, data = dat_tn, geom = "line") +  
          scale_x_continuous(breaks=seq(round(min(dat_tn$Time)), round(max(dat_tn$Time)), 5)) + ylab("Total Biomass (Tons)") +  theme(legend.position="none")}) 
      
      # Invertebrate rel plots
      output$invert_rbio <- renderPlot({
        invert_rbio <- obj$rel_bio[,c(1,match(input$invert_var, names(obj$rel_bio)))]
        colnames(invert_rbio) <- c("Time", "Biomass")
        qplot(y = Biomass, x = Time, data = invert_rbio, geom = "line") +
          ylab("") +  theme_bw() + ggtitle("Relative Biomass")})
      
      output$invert_tbio <- renderPlot({
        invert_tbio <- obj$tot_bio[,c(1,match(input$invert_var, names(obj$rel_bio)))]
        colnames(invert_tbio) <- c("Time", "Biomass")
        qplot(y = Biomass, x = Time, data = invert_tbio, geom = "line") +
          ylab("") +  theme_bw() + ggtitle("Total Biomass")})
      
      output$invertgraze <- renderPlot({
        graze_dat <- obj$invert_l[grep(input$invert_var, obj$invert_l$id),]
        graze_dat <- graze_dat[grep("Grazing", graze_dat$id),]
        qplot(y = value, x = as.numeric(as.character(variable)), data = graze_dat, geom = "line") + xlab("Time") + 
          ylab("") +  theme_bw() + ggtitle("Grazing")}) 
      
      output$invertprod <- renderPlot({
        prod_dat <- obj$invert_l[grep(input$invert_var, obj$invert_l$id),]
        prod_dat <- prod_dat[grep("Prodn", prod_dat$id),]
        qplot(y = value, x = as.numeric(as.character(variable)), data = prod_dat, geom = "line") + xlab("Time") + 
          ylab("") +  theme_bw() + ggtitle("Production")}) 
    }
  )
}