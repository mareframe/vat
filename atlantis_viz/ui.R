# ui.R
library("markdown")
shinyUI(navbarPage("VAT",
                   tabPanel("Welcome",
                            fluidRow(column(12,
                                            h2("Visualizing Atlantis Toolbox", align = "center"),
                                            includeMarkdown("markdown/iceatlantis.md"))),
                            fluidRow( column(1,
                                            img(src = "mareframe_logo.png", height = 90, width = 90)),
                                     column(10),
                                     column(1,
                                            img(src = "hi.gif", height = 90, width = 90))))
,
                   navbarMenu("Spatial Maps",
                   tabPanel("Disaggregated Spatial Plots",
                            sidebarLayout(
                              sidebarPanel(selectInput("var",
                                                   label = "Choose an unaggregrated functional group to display",
                                                   selected = "Cod1_Nums",
                                                   choices = var_names),
                                       sliderInput("layer", 
                                                   label = "Choose a layer to display",
                                                   min = 1, 
                                                   max = max_layers, 
                                                   value = 1,
                                                   format = "#"  # prevents decimal
                                                   ),
                                       sliderInput("time",
                                                   label = "Choose a time to display",
                                                   min = 1, 
                                                   max = max_time, 
                                                   value = 1,
                                                   format = "#"  # prevents decimal
                                                   )
                                       ),
                              mainPanel(
                                plotOutput("map")
                                )
                              )
),
                  tabPanel("Animated Plots",
                    sidebarLayout(
                              sidebarPanel(selectInput("aggbio",
                                                   label = "Choose a functional group to display",
                                                   selected = "FCD",
                                                   choices = bio_agg_names)),
                              mainPanel(
                                imageOutput("agg_image", inline = TRUE, "100%", "550px")
                              )
                              )
                    )
),
                    navbarMenu("Diagnostic Plots",
                               tabPanel("Nitrogen, Numbers, and Biomass At Age",
                                        fluidRow(column(2,
                                                        wellPanel(
                                                          selectInput("sn",
                                                                      label = "Select Functional Group",
                                                                      choices = rs_names))),
                                                 column(5,
                                                                 plotOutput("structn", height = "300px")),
                                                          column(5,
                                                                 plotOutput("reserven", height = "300px"))),
                                        fluidRow(column(2),
                                                 column(5,
                                                        plotOutput("totalnum", height = "300px")),
                                                 column(5,
                                                       plotOutput("totalbio", height = "300px")))),
                                        
                  tabPanel("Diet Plots",
                           fluidRow(
                             plotOutput("epibenthic", "100%", "550px")),
                           fluidRow(
                             plotOutput("wc", "100%", "550px")),
                             fluidRow(
                               plotOutput("sed", "100%", "550px")))
                  ,tabPanel("Aggregated Plots",
                            fluidRow(column(4,
                                            wellPanel(
                                              selectInput("rel_var",
                                                          label = "Choose a relative biomass to display",
                                                          choices = rel_names))),
                                     column(4,
                                            wellPanel(
                                              selectInput("ssb_var",
                                                          label = "Choose a SSB biomass to display",
                                                          choices = ssb_names))),
                                     column(4,
                                            wellPanel(
                                              selectInput("yoy_var",
                                                          label = "Chose a YOY biomass to display",
                                                          choices = yoy_names))),
                            fluidRow(column(4,
                                            plotOutput("rel_map", height = "300px")),
                                     column(4,
                                            plotOutput("ssb_map", height = "300px")),
                                     column(4,
                                            plotOutput("yoy_map", height = "300px"))))))))
