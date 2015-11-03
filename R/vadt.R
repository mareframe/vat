#' The visualizing Atlantis diagnostic tool
#' 
#' The visualizing Atlantis diagnostic tool is an interactive Shiny-based tool for model tuning and calibration. It includes various plots, both aggregrated and unaggregated, that are useful for diagnostic, tuning, and visualizing output. To use \code{vadt}, the user must first run the \code{create_vadt} function which will create an object of class \code{vadt} which can be fed to the \code{vadt} function. The user must also run the \code{animate_vadt} to create the animated GIFs required my the function. 
#' 
#' 
#'@param obj Object of class vat returned by create_vat
#'@param anim Directory to stored animated plot created by vat_animate function (defaults to NULL)
#'@import ggplot2
#'@import shiny
#'@importFrom scales muted
#'@importFrom DT datatable
#'@export
#'@seealso \code{\link{create_vadt}}, \code{\link{animate_vadt}}
#'@examples
#'\dontrun{
#' anim <- "/path/to/animinated/gifs"
#' obj <- create_vadt(outdir = "/atlantis/output_dir/",
#' fgfile = "/atlantis/functionalgroup.csv", 
#' ncout = "output_atlantis")
#' vadt(obj, anim = NULL)
#' }
#'

vadt <- function(obj, anim = NULL){
  shinyApp(
    ui = navbarPage("vadt",
                    # Starting "Welcome" Tab"
                      tabPanel("Welcome",
                             fluidRow(column(12,
                                             h1("Visualizing Atlantis Diagnostic Tool", align = "center"))),
                             p(),
                             p(),
                             p(),
                             fluidRow(column(2),
                                      column(8,
                                             includeMarkdown("http://mareframe.github.io/vat_documentation/markdown/vat_diagnostic.md")),
                                      column(2)),
                             br(),
                             fluidRow(column(2),
                                      column(1,
                                             img(src = "http://mareframe.github.io/vat_documentation/images/mareframe_logo.png", height = 50)),
                                      column(4),
                                      column(1,
                                             img(src = "http://mareframe.github.io/vat_documentation/images/eu.jpg", height = 50)),
                                      column(2))),
                    # tabPanel("Functional Groups",
                    #          dataTableOutput('fun_group_atl')),
                    # 
                    # Disaggregated Spatial Maps
                    navbarMenu("Spatial Plots",
                               tabPanel("Vertebrate Distribution by Boxes",
                                        fluidRow(column(4),
                                                 column(4,wellPanel(selectInput("erla_plot_select",
                                                                                label = "Functional Group",
                                                                                choices = names(obj$erla_plots))))),
                                        fluidRow(column(12,
                                                        plotOutput("vert_erla_plot", height = "1200px")))),
                               tabPanel("Within a Box Distribution",
                                        fluidRow(column(4),
                                                 column(4,wellPanel(selectInput("biomass_box_sel",
                                                                                label = "Box",
                                                                                choices = unique(obj$biomass_by_box$Box))))),
                                        fluidRow(column(12,
                                                        plotOutput("within_box_plot", height = "1200px")))),
                               tabPanel("Interactive Plots",
                                        navlistPanel(widths = c(2, 10),
                                                     tabPanel("Vertebrates",
                                                              fluidRow(
                                                                column(4,
                                                                       wellPanel(
                                                                         selectInput("disagg_var",
                                                                                     label = "",
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
                                                                                     round = TRUE))),
                                                                column(8,
                                                                       plotOutput("map", height = "450px")))),
                                                     tabPanel("Invertebrates",
                                                              fluidRow(column(4,
                                                                              wellPanel(
                                                                                selectInput("invert_sm",
                                                                                            label = "",
                                                                                            selected = obj$invert_mnames[1],
                                                                                            choices = obj$invert_mnames),
                                                                                uiOutput("ui_invert"),
                                                                                sliderInput("invert_time",
                                                                                            label = "Choose a time to display",
                                                                                            min = 1,
                                                                                            step = 1,
                                                                                            max = obj$max_time, 
                                                                                            value = 1,
                                                                                            round = TRUE))),
                                                                       column(8,
                                                                              plotOutput("invert_map", height = "450px")))),
                                                     tabPanel("Tracers",
                                                              fluidRow(column(4,
                                                                              wellPanel(
                                                                                selectInput("trace_sm",
                                                                                            label = "",
                                                                                            selected = obj$trace_names[1],
                                                                                            choices = obj$trace_names),
                                                                                uiOutput("ui_trace"),
                                                                                sliderInput("trace_time",
                                                                                            label = "Choose a time to display",
                                                                                            min = 1,
                                                                                            step = 1,
                                                                                            max = obj$max_time, 
                                                                                            value = 1,
                                                                                            round = TRUE))),
                                                                       column(8,
                                                                              plotOutput("trace_map", height = "450px")))))),
                               
                              
                               
                               tabPanel("Animated Spatial Biomass (tons)",
                                        column(5,
                                               if(is.null(anim) == FALSE){
                                               wellPanel(selectInput("aggbio",
                                                                     label = "Functional Group",
                                                                     selected = obj$bioagg_names[1],
                                                                     choices = obj$bioagg_names))}),
                                        column(7,
                                               if(is.null(anim) == FALSE){
                                               plotOutput("agg_image", inline = TRUE, "100%", "550px")}))),
                    tabPanel("Age Disaggregated",
                                        fluidRow(column(4),
                                                 column(4,wellPanel(selectInput("sn",
                                                                                label = "Functional Group",
                                                                                choices = obj$rs_names)))),
                                        fluidRow(column(1),
                                                 column(5,
                                                        plotOutput("structn", height = "450px")),
                                                 column(5,
                                                        plotOutput("totalprop", height = "450px"))),
                                        fluidRow(column(1),
                                                 column(5,
                                                        plotOutput("reserven", height = "450px")),
                                                 column(5,
                                                        plotOutput("totalnum", height = "450px"))),
                                        fluidRow(column(1),
                                                 column(5,
                                                        plotOutput("lw_plot", height = "450px")),
                                                 column(5,
                                                        plotOutput("totalbio", height = "450px"))),
                                        fluidRow(column(1),
                                                 column(5,
                                                        plotOutput("totalfirst", height = "450px")))),
                    
                    # The diagnostic plots UI
                    
                    navbarMenu("Diet Data",
                           tabPanel("Consumption by Predator",
                                     navlistPanel(widths = c(2, 10),
                                                  tabPanel("By Age Class",
                                                           fluidRow(column(4),
                                                                    column(4,
                                                                           if(is.null(obj$tot_pred) == FALSE){
                                                                             wellPanel(selectInput("diet_pred_unagg",
                                                                                                   label = "Predator",
                                                                                                   choices = obj$fgnames))}
                                                                    )),
                                                           fluidRow(column(12,
                                                                           if(is.null(obj$tot_pred) == FALSE)
                                                                             plotOutput("diet_pred_plot", height = "800px")))),
                                                  tabPanel("Over Age Class",
                                                           fluidRow(column(4),
                                                                    column(4,
                                                                           if(is.null(obj$tot_pred) == FALSE){
                                                                             wellPanel(selectInput("diet_pred_agg",
                                                                                                   label = "Predator",
                                                                                                   choices = obj$fgnames))}
                                                                    )),
                                                           fluidRow(column(12,
                                                                           if(is.null(obj$tot_pred) == FALSE)
                                                                             plotOutput("diet_predator_collapsed", height = "800px")))))),   
                               tabPanel("Consumption by Prey",
                                        navlistPanel(widths = c(2, 10),
                                                              tabPanel("By Age Class",
                                                                       fluidRow(column(4),
                                                                                column(4,
                                                                                       if(is.null(obj$tot_pred) == FALSE){
                                                                                         wellPanel(selectInput("diet_prey_unagg",
                                                                                                               label = "Prey",
                                                                                                               choices = obj$fgnames))}
                                                                                )),
                                                                       fluidRow(column(12,
                                                                                       if(is.null(obj$tot_pred) == FALSE)
                                                                                         plotOutput("diet_prey_plot", height = "600px")))),
                                                              tabPanel("Over Age Class",
                                                                       fluidRow(column(4),
                                                                                column(4,
                                                                                       if(is.null(obj$tot_pred) == FALSE){
                                                                                         wellPanel(selectInput("diet_prey_agg",
                                                                                                               label = "Prey",
                                                                                                               choices = obj$fgnames))}
                                                                                )),
                                                                       fluidRow(column(12,
                                                                                       if(is.null(obj$tot_pred) == FALSE)
                                                                                         plotOutput("diet_prey_collapsed", height = "600px")))))),
                           tabPanel("Diet by Predator and Prey",
                                    fluidRow(column(2),
                                             column(4,
                                                    if(is.null(obj$tot_pred) == FALSE){
                                                      wellPanel(
                                                        selectInput("diet_dispred",
                                                                    label = "Predator",
                                                                    choices = obj$fgnames))}),
                                             column(4,
                                                    if(is.null(obj$tot_pred) == FALSE){
                                                      wellPanel(
                                                        selectInput("diet_disprey",
                                                                    label = "Prey",
                                                                    choices = obj$fgnames))}),
                                             column(2)),
                                    fluidRow(column(1),
                                             column(5,
                                                    if(is.null(obj$tot_pred) == FALSE)
                                                      plotOutput("diet_pprey", height = "600px")),
                                             column(5,
                                                    if(is.null(obj$tot_pred) == FALSE)
                                                      plotOutput("diet_pprey_collapsed", height = "600px")))),

                            tabPanel("Mean Diet Data",
                                        fluidRow(
                                          column(2),
                                          column(4,
                                                 if(is.null(obj$tot_pred) == FALSE){
                                                 selectInput("mean_diet_pred", 
                                                             "Predator:", 
                                                             c("All", 
                                                               unique(as.character(obj$tot_pred$Predator))))}
                                          ),
                                          column(4, 
                                                 if(is.null(obj$tot_pred) == FALSE){
                                                 selectInput("mean_diet_prey", 
                                                             "Prey:", 
                                                             c("All", 
                                                               unique(as.character(obj$tot_pred$Prey))))}
                                                 )),
                                        if(is.null(obj$tot_pred) == FALSE)
                                        dataTableOutput('diet_table'))
                                 ),
                    navbarMenu("Summaries",
                               tabPanel("Biomass Facet Plots",
                                        navlistPanel(widths = c(2, 10),
                                                     tabPanel("Vertebrates",
                                                              fluidRow(
                                                                column(4),
                                                                column(4,
                                                                       selectInput("tot_vert_scale", 
                                                                                   "Scale Type", 
                                                                                   c("Unadjusted","Log Scale")))),
                                                              fluidRow(
                                                              column(12,
                                                                       plotOutput("tot_vert_sum", height = "800px")))),
                                                     tabPanel("Invertebrates and Other Tracers",
                                                              fluidRow(
                                                                column(4),
                                                                column(4,
                                                                        selectInput("tot_invert_scale", 
                                                                                          "Scale Type", 
                                                                                          c("Unadjusted","Log Scale")))),
                                                              fluidRow( 
                                                              column(12,
                                                                              plotOutput("tot_invert_sum", height = "800px")))))),
                               tabPanel("Vertebrates",
                                        fluidRow(column(4),
                                                 column(4,
                                                        wellPanel(
                                                          selectInput("ssb_var",
                                                                      label = "Functional Group",
                                                                      choices = obj$rs_names))),
                                                 column(4)),
                                        fluidRow(column(1),
                                                 column(5,
                                                        plotOutput("tot_map", height = "300px")),
                                                 column(5,
                                                        plotOutput("ssb_map", height = "300px"))),
                                        fluidRow(column(1),
                                                 column(5,
                                                        plotOutput("rel_map", height = "300px")),
                                                 column(5,
                                                        plotOutput("yoy_map", height = "300px")))),
                               tabPanel("Invertebrates and Other Tracers",
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
                                                        plotOutput("invertprod", height = "300px")))))),
    server = function(input, output) {
      
      # -----------------------------------------
      # SPATIAL PLOTS
      # -----------------------------------------
      
      # output$fun_group_atl = renderDataTable({
      #   datatable(obj$fun_group, rownames = FALSE)
      # })
      # 
      output$map <- renderPlot({
        tmp <- obj$disagg[[input$disagg_var]]
        tmp.min <- min(tmp)
        tmp.max <- max(tmp)
        tmp.mid <- median(tmp)
        tmp <- obj$disagg[[input$disagg_var]][input$layer,,input$time]
        
        # Plot islands with a different color
        if(is.character(obj$islands)){
          islands <- as.numeric(obj$islands)
          tmp[islands + 1] <- NA
        }
        data_tmp <- data.frame(boxid = 0:(obj$numboxes - 1), tmp)
        
        unagg_map_data <- merge(obj$map_base, data_tmp)
        ggplot(data = unagg_map_data, aes(x = x, y = y)) +
          geom_polygon(aes(group = boxid, fill = tmp), colour = "black") +
          theme_bw() + xlab("") + ylab("") +
          scale_fill_gradient2(limits = c(tmp.min, tmp.max), midpoint = tmp.mid, low = muted("red"), high = muted("blue")) +
          theme(legend.title=element_blank(), plot.background = element_blank()) + scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
      })
      
      output$ui_invert <- renderUI({
        tmp <- obj$invert_vars[[input$invert_sm]]
        if(length(dim(tmp)) == 3)
          sliderInput("invert_layer", 
                      label = "Choose a layer to display",
                      min = 1,
                      step = 1,
                      max = obj$max_layers, 
                      value = 1,
                      round = TRUE)
      })
      
      output$ui_trace <- renderUI({
        tmp <- obj$trace_vars[[input$trace_sm]]
        if(length(dim(tmp)) == 3)
          sliderInput("trace_layer", 
                      label = "Choose a layer to display",
                      min = 1,
                      step = 1,
                      max = obj$max_layers, 
                      value = 1,
                      round = TRUE)
      })
      
      # Tracer plot
      output$trace_map <- renderPlot({
        tmp <- obj$trace_vars[[input$trace_sm]]
        tmp.min <- min(tmp)
        tmp.max <- max(tmp)
        tmp.mid <- median(tmp)
        if(length(dim(tmp)) == 3){
          tmp <- obj$trace_vars[[input$trace_sm]][input$trace_layer,,input$trace_time]
        } else tmp <- obj$trace_vars[[input$trace_sm]][,input$trace_time]
        
        # Plot islands with a different color
        if(is.character(obj$islands)){
          islands <- as.numeric(obj$islands)
          tmp[islands + 1] <- NA
        }
        data_tmp <- data.frame(boxid = 0:(obj$numboxes - 1), tmp)
        
        unagg_map_data <- merge(obj$map_base, data_tmp)
        ggplot(data = unagg_map_data, aes(x = x, y = y)) +
          geom_polygon(aes(group = boxid, fill = tmp), colour = "black") +
          theme_bw() + xlab("") + ylab("") +
          scale_fill_gradient2(limits = c(tmp.min, tmp.max), low = muted("red"), midpoint = tmp.mid, high = muted("blue")) +
          theme(legend.title=element_blank()) + scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
      })
      
      output$within_box_plot <- renderPlot({
        tmp <- filter(obj$biomass_by_box, Box == input$biomass_box_sel)
        qplot(y = round(value), x = Time, geom = "line", data = tmp) + facet_wrap(~ Name, scales = "free", ncol = 5) + theme_bw()
      })
      
      ## Erla's Vertebrate Number Plots
      output$vert_erla_plot <- renderPlot({
        tmp <- obj$erla_plots[[input$erla_plot_select]]
        tmp$Time <- as.numeric(as.character(tmp$Time)) * obj$toutinc / 365 + obj$startyear
        if(is.null(tmp$Layer)){
        ggplot(data = tmp, aes(y = number, x = Time)) + geom_line(size = 1) + ylab("") + xlab("Time") + facet_wrap(~ Box, ncol = 5) + scale_color_manual(values = cbpalette) + theme_bw() + guides(fill = guide_legend(override.aes = list(colour = NULL))) +  
          scale_x_continuous(breaks=floor(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2)))))  + theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2))
      } else {
        if(nlevels(tmp$Layer) == 7){
        cbpalette <- c("#a50026", "#d73027", "#f46d43", "#fdae61","#74add1", "#4575b4", "#313695")
        ggplot(data = tmp, aes(y = number, x = Time, group = Layer, color = Layer)) + geom_line(size = 1) + ylab("") + xlab("Time") + facet_wrap(~ Box, ncol = 5) + scale_color_manual(values = cbpalette) + theme_bw() + guides(fill = guide_legend(override.aes = list(colour = NULL))) +  
          scale_x_continuous(breaks=floor(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2)))))  + theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2)) 
        } else {
          ggplot(data = tmp, aes(y = number, x = Time, group = Layer, color = Layer)) + geom_line(size = 1)  + ylab("") + xlab("Time") + facet_wrap(~ Box, ncol = 5) + theme_bw() + guides(fill = guide_legend(override.aes = list(colour = NULL))) +  
            scale_x_continuous(breaks=floor(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) + theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2))
        }
      }
        })
      
      # Invertebrate plot
      output$invert_map <- renderPlot({
        tmp <- obj$invert_vars[[input$invert_sm]]
        tmp.min <- min(tmp)
        tmp.max <- max(tmp)
        tmp.mid <- median(tmp)
        if(length(dim(tmp)) == 3){
          tmp <- obj$invert_vars[[input$invert_sm]][input$invert_layer,,input$invert_time]
        } else tmp <- obj$invert_vars[[input$invert_sm]][,input$invert_time]
        
        # Plot islands with a different color
        if(is.character(obj$islands)){
          islands <- as.numeric(obj$islands)
          tmp[islands + 1] <- NA
        }
        data_tmp <- data.frame(boxid = 0:(obj$numboxes - 1), tmp)
        
        unagg_map_data <- merge(obj$map_base, data_tmp)
        ggplot(data = unagg_map_data, aes(x = x, y = y)) +
          geom_polygon(aes(group = boxid, fill = tmp), colour = "black") +
          theme_bw() + xlab("") + ylab("") +
          scale_fill_gradient2(limits = c(tmp.min, tmp.max), low = muted("red"), midpoint = tmp.mid, high = muted("blue")) +
          theme(legend.title=element_blank()) + scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
      })
      
      
      output$agg_image <- renderImage({
        filename <- normalizePath(file.path(anim,
                                            paste(input$aggbio, "-aggbio.gif", sep = "")))
        list(src = filename)
      }, deleteFile = FALSE)
      
      # -----------------------------------------
      # AGE DISAGGREGATED TAB
      # -----------------------------------------
      
    
      
      # Structural nitrogen
      output$structn <- renderPlot({
        sn_ids <- paste(input$sn, 1:10, "_StructN", sep = "")
        dat_sn <- subset(obj$structN, .id %in% sn_ids)
        ggplot(data = dat_sn, aes(y = V1, x = Time)) + geom_line(aes(group = .id, color = .id), size = 2, alpha = .75) +  
          scale_x_continuous(breaks=seq(round(min(dat_sn$Time)), round(max(dat_sn$Time)), 5)) + ylab("Structural Nitrogen (mg N)")  + scale_color_brewer(name = "Ageclass", type = "div",palette = 5, labels = 1:10)  + theme_bw() + guides(fill = guide_legend(override.aes = list(colour = NULL))) + theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2))})
      
      # Reserve nitrogen
      output$reserven <- renderPlot({
        rn_ids <- paste(input$sn, 1:10, "_ResN", sep = "")
        dat_rn <- subset(obj$reserve, .id %in% rn_ids)
        ggplot(data = dat_rn, aes(y = V1, x = Time)) + geom_line(aes(group = .id, color = .id), size = 2, alpha = .75) +  
          scale_x_continuous(breaks=seq(round(min(dat_rn$Time)), round(max(dat_rn$Time)), 5)) + ylab("Reserve Nitrogen (mg N)")  + scale_color_brewer(name = "Ageclass",type = "div",palette = 5, labels = 1:10) + theme_bw() + guides(fill = guide_legend(override.aes = list(colour = NULL)))+ theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2))})
      
      
      # Total Biomass
      output$totalbio <- renderPlot({
        sn_ids <- paste(input$sn, 1:10, "_StructN", sep = "")
        sn <- subset(obj$structN, .id %in% sn_ids)
        totn_ids <- paste(input$sn, 1:10, "_Nums", sep = "")
        dat_tn <- subset(obj$totalnums, .id %in% totn_ids)
        dat_tn$V1 <- (3.65*sn$V1*5.7*20)/1e3 * dat_tn$V1 / 1e6
        ggplot(data = dat_tn, aes(y = V1, x = Time)) + geom_line(aes(color = .id), size = 2, alpha = .75) + scale_x_continuous(breaks=seq(round(min(dat_tn$Time)), round(max(dat_tn$Time)), 5)) + ylab("Total Biomass (Tons)") + scale_color_brewer(name = "Ageclass", type = "div",palette = 5, labels = 1:10) + theme_bw()  + guides(fill = guide_legend(override.aes = list(colour = NULL)))+ theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2, color = "black"))
      }) 
      
      
      # First year
      output$totalfirst <- renderPlot({
        totn_ids <- paste(input$sn, 1:10, "_Nums", sep = "")
        dat_totn <- subset(obj$totalnums, .id %in% totn_ids)
        dat_first <- dat_totn[grep("1_", dat_totn$.id),]
        ggplot(dat_first, aes(y = V1, x = Time))  + geom_line(size = 2, alpha = .75)  + ylab("Age Class 1 Recruits")  + scale_color_discrete() + theme_bw()+  scale_x_continuous(breaks=seq(round(min(dat_totn$Time)), round(max(dat_first$Time)), 5))  + guides(fill = guide_legend(override.aes = list(colour = NULL))) + theme(panel.background=element_blank(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.line = element_line(size = .2)) + theme(legend.position="none")
      })
      
      # Tot number
      output$totalnum <- renderPlot({
        totn_ids <- paste(input$sn, 1:10, "_Nums", sep = "")
        dat_totn <- subset(obj$totalnums, .id %in% totn_ids)
        ggplot(dat_totn, aes(y = V1, x = Time, group = .id, color = .id)) + geom_line(size = 2, alpha = .75)  + scale_color_brewer(name = "Ageclass",type = "div",palette = 5, labels = 1:10) + ylab("Total numbers")  + theme_bw()+  scale_x_continuous(breaks=seq(round(min(dat_totn$Time)), round(max(dat_totn$Time)), 5))  + guides(fill = guide_legend(override.aes = list(colour = NULL))) + theme(panel.background=element_blank(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.line = element_line(size = .2))
      })
      
      
      # Total Prop
      output$totalprop <- renderPlot({
        totn_ids <- paste(input$sn, 1:10, "_Nums", sep = "")
        dat_totn <- subset(obj$totalnums, .id %in% totn_ids)
        ggplot(dat_totn, aes(y = V1, x = Time)) + geom_density(stat = "identity", aes(fill = .id), position = "fill", binwidth = 100, alpha = .75, lwd = .2)  + scale_fill_brewer(name = "Ageclass",type = "div",palette = 5, labels = 1:10 ) + ylab("Proportion of total numbers")  + theme_bw()+  scale_x_continuous(breaks=seq(round(min(dat_totn$Time)), round(max(dat_totn$Time)), 5))  + guides(fill = guide_legend(override.aes = list(colour = NULL))) + theme(panel.background=element_blank(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.line = element_line(size = .2))
      })
      
      ## Length-At-Age plot
      output$lw_plot <- renderPlot({
        sn_ids <- paste(input$sn, 1:10, "_StructN", sep = "")
        lw_data <- subset(obj$structN, .id %in% sn_ids)
        lw_data$wt_grams <- 3.65*lw_data$V1*5.7*20/1000
        fg_name <- obj$fun_group[obj$fun_group$Name == input$sn, 1]
        param_a <- obj$ab_params[grep(fg_name, obj$ab_params$a_name), 2]
        param_b <- obj$ab_params[grep(fg_name, obj$ab_params$b_name), 4]
        lw_data$length <- (lw_data$wt_grams/param_a)^(1/param_b)
        
        ggplot(data = lw_data, aes(y = length, x = Time)) + geom_line(aes(group = .id, color = .id), size = 2, alpha = .75) +  scale_x_continuous(breaks=seq(round(min(lw_data$Time)), round(max(lw_data$Time)), 5)) + ylab("Length-At-Age (cm)") + scale_color_brewer(name = "Ageclass", type = "div",palette = 5, labels = 1:10) + theme_bw()  + guides(fill = guide_legend(override.aes = list(colour = NULL)))+ theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2, color = "black")) 
      })
      
      # -----------------------------------------
      # DIET DATA TAB
      # -----------------------------------------
      
      # Diet Table 
      output$diet_table <- renderDataTable({
        diet_tab <- obj$tot_pred
        if (input$mean_diet_pred != "All"){
          diet_tab <- diet_tab[diet_tab$Predator == input$mean_diet_pred,]
        }
        if (input$mean_diet_prey != "All"){
          diet_tab <- diet_tab[diet_tab$Prey == input$mean_diet_prey,]
        }
        options(scipen = 999)
          datatable(diet_tab,rownames = FALSE,
                    caption = tags$caption(
                      style = 'caption-side: bottom; text-align: center;',
                      'Diet Matrix '))
        })
      
        # Diet Predator by prey
      output$diet_pprey <- renderPlot({
        data_dpp <- subset(obj$diet_l, Predator == input$diet_dispred & Prey == input$diet_disprey)
        ggplot(data = data_dpp, aes(x = Time/365, y = eaten, color = as.character(Cohort))) + geom_line(size = 1, alpha = .75) + scale_color_brewer(name = "Ageclass", type = "div",palette = 5, labels = 1:10) + xlab("Years since simulation started") + ylab("Consumed") + ggtitle(paste("Consumption of ", data_dpp[1,5], " by ", data_dpp[1,1], " by Age Class", sep = "")) + theme_bw() + guides(fill = guide_legend(override.aes = list(colour = NULL)))+ theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2))
        })
        
      # Diet Predator by prey collapsed over age class
      output$diet_pprey_collapsed <- renderPlot({
        data_dpp <- filter(obj$diet_l, Predator == input$diet_dispred & Prey == input$diet_disprey)
        ggplot(data = data_dpp, aes(x = Time/365, y = eaten)) + stat_summary(aes(group =1), fun.y = sum, geom = "line", size = 1, alpha = .75) + xlab("Years since simulation started") + ylab("Consumed") + ggtitle(paste("Consumption of ", data_dpp[1,5], " by ", data_dpp[1,1], " Collapsed over Age Class", sep = "")) + theme_bw() + guides(fill = guide_legend(override.aes = list(colour = NULL)))+ theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2))
      })
      
      # Consumption by Predator
      output$diet_pred_plot <- renderPlot({
        data_dpp <- subset(obj$diet_l, Predator == input$diet_pred_unagg)
        ggplot(data = data_dpp, aes(x = Time/365, y = eaten, color = as.character(Cohort))) + geom_line(size = 1, alpha = .75) + scale_color_brewer(name = "Ageclass", type = "div",palette = 5, labels = 1:10) + xlab("Years since simulation started") + ylab("Consumed") + ggtitle(paste("Consumption by ", data_dpp[1,1], " by Age Class", sep = "")) + facet_wrap(~ Prey) + theme_bw() + guides(fill = guide_legend(override.aes = list(colour = NULL)))+ theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2))
      })
      
      # Consumption by Predator collapsed
      output$diet_predator_collapsed <- renderPlot({
        data_dpp <- subset(obj$diet_l, Predator == input$diet_pred_agg)
        ggplot(data = data_dpp, aes(x = Time/365, y = eaten))+ stat_summary(aes(group =1), fun.y = sum, geom = "line", size = 1, alpha = .75) + xlab("Years since simulation started") + ylab("Consumed") + ggtitle(paste("Consumption by ", data_dpp[1,1], " Collapsed over Age Class", sep = "")) + facet_wrap(~ Prey) + theme_bw() + guides(fill = guide_legend(override.aes = list(colour = NULL)))+ theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2))
      })
      
      # Consumption by Prey
      output$diet_prey_plot <- renderPlot({
        data_dpp <- subset(obj$diet_l, Prey == input$diet_prey_unagg)
        ggplot(data = data_dpp, aes(x = Time/365, y = eaten, color = as.character(Cohort))) + geom_line(size = 1, alpha = .75) + scale_color_brewer(name = "Ageclass", type = "div",palette = 5, labels = 1:10) + xlab("Years since simulation started") + ylab("Consumed") + ggtitle(paste("Consumption of ", data_dpp[1,5], " by Age class", sep = "")) + facet_wrap(~ Predator) + theme_bw() + guides(fill = guide_legend(override.aes = list(colour = NULL)))+ theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2))
      })
      
      # Consumption by Prey collapsed
      output$diet_prey_collapsed <- renderPlot({
        data_dpp <- subset(obj$diet_l,Prey == input$diet_prey_agg)
        ggplot(data = data_dpp, aes(x = Time/365, y = eaten)) + stat_summary(aes(group =1), fun.y = sum, geom = "line", size = 1, alpha = .75) + xlab("Years since simulation started") + ylab("Consumed") + ggtitle(paste("Consumption of ", data_dpp[1,5], " collapsed over age class", sep = "")) + facet_wrap(~ Predator) + theme_bw() + guides(fill = guide_legend(override.aes = list(colour = NULL)))+ theme(panel.background=element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.key = element_rect(colour = NA),axis.line = element_line(size = .2))
      })
      
      # -----------------------------------------
      # SUMMARIES TAB
      # -----------------------------------------
      
      # Total biomass trellis
      output$tot_vert_sum <- renderPlot({
        if(input$tot_vert_scale == "Unadjusted"){
        qplot(x = Time, y = value, data = obj$tot_bio_v, geom = "line") + facet_wrap(~Name, ncol = 4, scales = "free") + xlab("Year") + ylab("Total Biomass") + theme_bw()
        } else {
          qplot(x = Time, y = log(value), data = obj$tot_bio_v, geom = "line") + facet_wrap(~Name, ncol = 4, scales = "free_x") + xlab("Year") + ylab("Log(Total Biomass)") + theme_bw()
        }
      })
      output$tot_invert_sum <- renderPlot({
        if(input$tot_invert_scale == "Unadjusted"){
        qplot(x = Time, y = value, data = obj$tot_bio_i, geom = "line") + facet_wrap(~Name, ncol = 4, scales = "free") + xlab("Year") + ylab("Total Biomass") + theme_bw()
        } else {
            qplot(x = Time, y = log(value), data = obj$tot_bio_i, geom = "line") + facet_wrap(~Name, ncol = 4, scales = "free_x") + xlab("Year") + ylab("Log(Total Biomass)") + theme_bw()
        }
      })
      
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