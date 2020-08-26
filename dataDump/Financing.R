library(gargle)
library(devtools)
library(rsconnect)
library(shiny)
library(shinyforms)
library(shinyalert)
library(shinydashboard)
library(htmltools)
library(markdown)
library(shinythemes)
library(shinyBS)
##devtools::install_github("carlganz/shinyCleave")
library(shinyCleave)
##install.packages("shinyjs")
library(shinyjs)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(shinydashboardPlus)
##install.packages("rlist")
library(rlist)
##install.packages("zoo")
library(zoo)

##install_github("eastcoasting/shinyWidgets")
library("shinyWidgets")
##setwd("C:/Users/MichaelOwen/Documents/Tinkering/ICI")


gefproj <- read.csv("financingData.csv", stringsAsFactors = FALSE)
gefproj$IPLC <- as.character(as.integer(gefproj$IPLC))


library(grid)
library(scales)
element_custom <- function() {
  structure(list(), class = c("element_custom", "element_text"))
}
element_grob.element_custom <- function(element, label="", ...)  {
  disect <- strsplit(label, "\\n")[[1]]
  labels <- lapply(disect, function(x) tryCatch(parse(text=x), 
                                                error = function(e) x))
  hl <-  unit(rep(1, length(labels)), 'strheight', data=labels) + unit(0.1,"line")
  yl <- c(list(unit(0,"line")), 
          lapply(seq_along(labels[-length(labels)]), function(ii) sum(hl[1:ii])))
  
  cl <- do.call(gList, Map(function(label, ii) 
    textGrob(label, y = unit(1,"npc")-yl[[ii]], hjust=0, x=0, vjust=1), 
    label = labels, ii = seq_along(labels)))
  
  gTree(children = cl, cl="sl", heights = hl, gp=gpar(col="grey50",fontsize=11))
}
heightDetails.sl <- function(x) sum(x$heights)

## format for thousands/millions


##label_number_si(accuracy=.1)



nmsY <- names(gefproj[c(7,8,10)])
nmsX <- names(gefproj[c(3,4)])


ui <- navbarPage(title = "ICI Geographies EoI Scoring",
                 theme = shinytheme("flatly"), 
tabPanel("Financing Landscape", value = "panel-8",
         tags$head(
           tags$style(type = "text/css", "#map-map {height: calc(100vh - 80px) !important;}"),
           tags$style(HTML(".shiny-output-error-validation {
                             font-weight: bold; font-size: 25px;}"))),
                                  fluidRow(
                                    
                                    column(12,
                                           plotlyOutput("financingPlot")),
                                    column(2,
                                           uiOutput("var_select")),
                                   column(2, selectInput('x', 'X-Axis', choices = nmsX, selected = "Grant")),
                                   column(2,     selectInput('y', 'Y-Axis', choices = nmsY, selected = "Fund.Source")),
                                   column(2,   checkboxGroupInput('projRegion', 'Subset by Geographic Scope', 
                                                                  choices=c('National','Transnational'),
                                                                  selected=c('National','Transnational')
                                   )),
                                   column(12,
                                    DT::dataTableOutput("gefprojs")),  style = "width: 100%"
                                  ))
)
                 
                 
                 
           



server = function(input, output, session) {

###Select Inputs  
  output$var_select<-renderUI({
    selectInput("ind_var_select","Country",
                choices =c(list("",
                  "East Africa Drylands" = list("Burundi", "Djibouti", "Ethiopia", "Kenya", "Rwanda", "Tanzania", "Uganda"),
                                "Coastal East Africa" = list("Somalia", "Kenya", "Tanzania", "Mozambique", "South Africa"),
                                "Congo Basin" = list("Cameroon", "Central African Republic", "DR Congo", "Republic of the Congo", "Equatorial Guinea", "Gabon"),
                                "Mesoamerica" = list("Mexico", "Guatemala", "Honduras", "Belize", "El Salvador", "Nicaragua", "Panama", "Costa Rica"),
                                "Andes/Amazon" = list("Colombia", "Ecuador", "Peru", "Brazil", "Bolivia", "Guyana", "Suriname"),
                                "Southern Cone" = list("Argentina", "Chile"),
                                "Gran Chaco" = list("Bolivia", "Paraguay", "Argentina", "Chile"),
                                "Himalayas" = list("Nepal", "Bhutan", "India", "Pakistan"),
                                "South East Asia (Mainland)" = list("Cambodia", "Laos", "Thailand", "Vietnam", "Myanmar", "Peninsular Malaysia"),
                                "South East Asia (Islands)" = list("Indonesia", "Island Malaysia", "Philippines", "East Timor"),
                                "Melanesia" = list("Vanuatu", "Solomon Islands", "Fiji", "Papua New Guinea", "West Papua"),
                                "Polynesia" = list("Easter Island", "Samoa", "Tonga", "Cook Islands", "Tuvalu", "Tokelau", "Niue", "Wallis", "Futuna"))),
                multiple = FALSE, selected = NULL)
  })
###GEF  
  
  
  
  dataplot <- reactive({
    dataplot <- filter(if (is.null(input$ind_var_select)){
      gefproj
    }  else {
      gefproj %>% filter(grepl(input$ind_var_select, Countries))
    } 
    )
    
    
  })
  
  subsetScope <- reactive({
    validate(
      need( (!is.null(input$projRegion)), "No current financing data in database for the selected country.")
    )
    subset(dataplot(), subset = Scope %in% input$projRegion)
  })

  
  
  sumProjects <- reactive({
    sumProjects <-subsetScope()[[input$x]] %>% sum()
    label_number_si(accuracy=0.1)(sumProjects)
  })

  
  output$financingPlot <- renderPlotly({
    
    finalFunding <- subsetScope()
    finalFunding <- finalFunding %>% group_by(!!as.name(input$y)) %>% mutate(Total = as.numeric(sum(!! as.name(input$x))))
    validate(
      need( nrow(finalFunding) > 0, "No current financing data in database for the selected country.")
    )
      p <- ggplot(finalFunding, aes_string(x = input$x, y = input$y, fill = input$y)) + 
      geom_col(aes(text=paste('Total:', label_number_si(accuracy=.1)(finalFunding$Total))), position = position_stack(), alpha = 0.8) +
      scale_x_continuous(labels = label_number_si(accuracy=.1), expand = c(0, 0)) +
      labs(title = paste0("Current Cofinancing Landscape for ", input$ind_var_select, " <br /> (", length(subsetScope()$Project), " Projects - ", "US$", sumProjects(), ")"),
           x = "Funding (US$)",
           y = "")+
      (theme_grey() %+replace% theme(plot.caption = element_custom())) +
      theme(plot.title = element_text(face = "bold", size = 20, color = "#1b3955"),
            axis.text=element_text(size=12),
            axis.title = element_text(size = 12, face = "italic"),
            plot.subtitle = element_text(size=14, color = "black"),
           ## plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
            panel.grid.minor = element_blank(),
            legend.position = "none") +
      theme(legend.title = element_blank())
    
    
    toWebGL(ggplotly(p, tooltip = "text")) %>% 
      config(displaylogo = FALSE, 
             toImageButtonOptions = list(
               format = "png",
               filename = "financingPlot",
               width = 2400,
               height = 600
             ),
             modeBarButtonsToRemove = c(
               'sendDataToCloud',
               'select2d',
               'autoScale2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian',
               'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'pan2d'
             )) %>%
      layout(autosize = TRUE, margin = list(t = 100))
  })
  
  
  output$gefprojs <- DT::renderDataTable(
    datatable(subsetScope(), escape = FALSE, rownames = FALSE, 
    filter = list(position = 'top', clear = FALSE),
    options = list(
      search = list(regex = TRUE, caseInsensitive = FALSE),
      pageLength = 10
    ), 
    caption = 'Table 3: National or Transnational Projects.')%>%
      formatCurrency(c('Grant', 'Cofinancing'), digits = 0)
    ) 
  
  
  
}

shinyApp(ui = ui, server = server)

