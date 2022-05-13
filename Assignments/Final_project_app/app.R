
library(tidyverse)
library(dplyr)
library(plyr)
library(plotly)
library(shiny)

dmnd <- read.csv("https://raw.githubusercontent.com/VigneshReddy79/STA553-DataViz/main/Datasets/dmnd.csv",
                 header = TRUE)

dmnd <- dmnd[,-1]
colnames(dmnd)[c(5,7:10)] <- c("total_depth_percent","price ($)","length (mm)","width (mm)","depth (mm)")


choices = unique(dmnd$cut)
x.choice = names(dmnd)[c(1,5:10)]
y.choice = names(dmnd)[c(1,5:10)]




ui <- fluidPage(
    titlePanel( h4("Analysis of Diamonds data", 
                  align = "center", style = "color:navy", br(),br())),
    sidebarLayout(
        sidebarPanel( width = 3,
            helpText(
                "This side bar helps to analyze different aspects of data"
            ),
            hr(),
            helpText(
                "This selection corresponds to all tabs"
            ),
            radioButtons(inputId = "cut", 
                         label = "Diamond Cut",
                         choices = c(choices,"All"),
                         inline = FALSE,
                         selected = "All"),
            hr(),
            helpText(
                "This selection corresponds to all tabs"
            ),
            selectInput(inputId="Y",
                        label="Response Variable: Y",
                        choices=y.choice,
                        selected=x.choice[4]),
            helpText(
                "This selection corresponds to tabs 1,4,5 & 6"
            ),
            selectInput(inputId="X",
                        label="Predictor Variable: X",
                        choices=x.choice,
                        selected=x.choice[1]),
            hr(),
            helpText(
                "This selection corresponds to tab 6"
            ),
            numericInput(inputId = "newX", 
                         label = "New Value for Prediction:",
                         value=5, 
                         min=1, 
                         max=18800,
                         step=0.5)
        ),
        mainPanel( width = 6,
            tabsetPanel(type = "tabs",
                        tabPanel("1.Scatter Plot", plotlyOutput("plotly"), textOutput("plotly_txt")),
                        tabPanel("2.Stats by Clarity", textOutput("stats_clarity_txt"), plotlyOutput("stats_clarity")),
                        tabPanel("3.Stats by Color", textOutput("stats_color_txt"), plotlyOutput("stats_color")),
                        tabPanel("4.Regression Plot", textOutput("plot_txt"), plotOutput("plot")),
                        tabPanel("5.Regression Coefficients", textOutput("table_txt"),tableOutput("table")),
                        tabPanel("6.Prediction", textOutput("pred_txt"), plotOutput("predPlt"))
            )
        )
    )
)

server <- function(input, output) {
    
    workDat = function(dat){
        if (input$cut == "Ideal") {
            workingData = dmnd[which(dmnd$cut == "Ideal"),]
        } else if (input$cut == "Premium") {
            workingData = dmnd[which(dmnd$cut == "Premium"),]
        } else if (input$cut == "Good") {
            workingData = dmnd[which(dmnd$cut == "Good"),]
        } else if (input$cut == "Very Good") {
            workingData = dmnd[which(dmnd$cut == "Very Good"),]
        } else if (input$cut == "Fair") {
            workingData = dmnd[which(dmnd$cut == "Fair"),]
        } else {
            workingData = dmnd
        }
        workingData
    }
    
    output$plotly <- renderPlotly({
        
        plot_ly( data = workDat(),
                 x = ~workDat()[[input$X]],
                 y = ~workDat()[[input$Y]],
                 color = ~workDat()$cut,
                 size = ~workDat()$carat,
                 type = "scatter",
                 mode = "markers",
                 alpha = 0.75,
                 hovertemplate = paste('<i><b>',input$X,'<b></i>: %{x}',
                                       '<br><b>',input$Y,'</b>:  %{y}',
                                       '<br><b> carat: ',workDat()$carat,"</b>",
                                       '<br><b>',input$cut,'</b>')
        ) %>% layout( showlegend = FALSE,
                      plot_bgcolor ='#E2E2E2',
                      title = list( text = paste("<b>Relationship between",
                                                 input$Y, "and", input$X, "</b>"),
                                    font = list( size = 20,
                                                 color = "darkred") ),
                      xaxis = list( title = paste("<b>", input$X, "</b>"),
                                    gridcolor = 'white'),
                      yaxis = list( title = paste("<b>",input$Y, "</b>"),
                                    gridcolor = 'white'),
                      margin = list( b = 15, l = 15, t = 35, r = 15) 
        )
    })
    
    output$plotly_txt <- renderText({
        "\n *Note: The point size is proportional to diamond carat."
    })
    
    output$stats_clarity_txt <- renderText({
        "This tab displays some descriptive statistics of the diamonds data by clarity. \n"
    })
    
    output$stats_clarity <- renderPlotly({
        
        fig1 <- plot_ly( data = workDat(), 
                         labels = ~workDat()$clarity, 
                         type = "pie",
                         name = "By Clarity",
                         textposition = 'inside',
                         textinfo = 'label+percent',
                         insidetextfont = list(color = '#FFFFFF'),
                         marker = list(colors = colors,
                                       line = list(color = '#FFFFFF', width = 1)),
                         domain = list(row = 0, column = 0)
        )
        fig1_ <- subplot( fig1, margin = 0.05)  %>% 
            layout( showlegend = FALSE,
                    grid = list( rows=2, columns=2),
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
            )
        fig2 <- plot_ly( type = 'bar',
                         name = paste("Avg", input$Y, "by clarity"),
                         x = ~workDat()$clarity,
                         y = ~workDat()[[input$Y]],
                         marker = list( size = 10,
                                        color = 'blue',
                                        opacity = 0.8),
                         hovertemplate =  paste('<i><b>Avg',input$Y,'<b></i>: %{y}',
                                                '<br><i><b>Color:', workDat()$clarity, '<b></i>'),
                         transforms = list( list( type = 'aggregate',
                                                  groups = ~workDat()$clarity,
                                                  aggregations = list( list( target = 'y', 
                                                                             func = 'avg', 
                                                                             enabled = T))
                         )
                         )
        )
        fig3 <- plot_ly( data = workDat(),
                         type = "histogram",
                         x = ~workDat()[[input$Y]],
                         name = paste("Distribution of", input$Y),
                         hovertemplate =  paste('<i><b>',input$Y,'<b></i>: %{x}',
                                                '<br><b>Frequency: </b> %{y}')
        )
        fig4 <- plot_ly( type = "box",
                         y = ~workDat()[[input$Y]],
                         x = ~workDat()$clarity,
                         name = "Box plot"
        )
        
        
        annotations = list( list( x = 0.25,
                                  y = 1,
                                  text = "Frequency of diamonds by clarity",
                                  xref = "paper", 
                                  yref = "paper",
                                  xanchor = "center",
                                  yanchor = "bottom",
                                  showarrow = FALSE
        ),
        list( x = 0.8,
              y = 1,
              text = paste("Average",input$Y),
              xref = "paper", 
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
        ),
        list( x = 0.5,  
              y = 0.45,  
              text = paste("Histogram/Box plot showing the distribution of", input$Y), 
              xref = "paper",  
              yref = "paper",  
              xanchor = "center",  
              yanchor = "bottom",  
              showarrow = FALSE
        )
        )
        fig <- subplot( fig1_, fig2, fig3, fig4 ,nrows = 2, margin = 0.05) %>% 
            layout( #title = "Specs with Subplot Title", 
                plot_bgcolor='#e5ecf6', 
                xaxis = list( zerolinecolor = '#ffff', 
                              zerolinewidth = 2,
                              gridcolor = 'ffff'), 
                yaxis = list( zerolinecolor = '#ffff',
                              zerolinewidth = 2,
                              gridcolor = 'ffff'),
                bargap = 0.1,
                annotations = annotations,
                showlegend=FALSE,
                showlegend2=FALSE 
            ) 
        
        
    })
    
    output$stats_color_txt <- renderText({
        "This tab displays some descriptive statistics of the diamonds data by color. \n"
    })
    
    output$stats_color <- renderPlotly({
        
        fig1 <- plot_ly( data = workDat(), 
                         labels = ~workDat()$color, 
                         type = "pie",
                         name = "By Color",
                         textposition = 'inside',
                         textinfo = 'label+percent',
                         insidetextfont = list(color = '#FFFFFF'),
                         marker = list(colors = colors,
                                       line = list(color = '#FFFFFF', width = 1)),
                         domain = list(row = 0, column = 0)
        )
        fig1_ <- subplot( fig1, margin = 0.05)  %>% 
            layout( showlegend = FALSE,
                    grid = list( rows=2, columns=2),
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
            )
        fig2 <- plot_ly( type = 'bar',
                         name = paste("Avg", input$Y, "by color"),
                         x = ~workDat()$color,
                         y = ~workDat()[[input$Y]],
                         marker = list( size = 10,
                                        color = 'blue',
                                        opacity = 0.8),
                         hovertemplate =  paste('<i><b>Avg',input$Y,'<b></i>: %{y}',
                                                '<br><i><b>Color:', workDat()$color, '<b></i>'),
                         transforms = list( list( type = 'aggregate',
                                                  groups = ~workDat()$color,
                                                  aggregations = list( list( target = 'y', 
                                                                             func = 'avg', 
                                                                             enabled = T))
                         )
                         )
        )
        fig3 <- plot_ly( data = workDat(),
                         type = "histogram",
                         x = ~workDat()[[input$Y]],
                         name = paste("Distribution of", input$Y),
                         hovertemplate =  paste('<i><b>',input$Y,'<b></i>: %{x}',
                                                '<br><b>Frequency: </b> %{y}')
        )
        fig4 <- plot_ly( type = "box",
                         y = ~workDat()[[input$Y]],
                         x = ~workDat()$color,
                         name = "Box plot"
        )
        
        
        annotations = list( list( x = 0.25,
                                  y = 1,
                                  text = "Frequency of diamonds by color",
                                  xref = "paper", 
                                  yref = "paper",
                                  xanchor = "center",
                                  yanchor = "bottom",
                                  showarrow = FALSE
        ),
        list( x = 0.8,
              y = 1,
              text = paste("Average",input$Y),
              xref = "paper", 
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
        ),
        list( x = 0.5,  
              y = 0.45,  
              text = paste("Histogram/Box plot showing the distribution of", input$Y), 
              xref = "paper",  
              yref = "paper",  
              xanchor = "center",  
              yanchor = "bottom",  
              showarrow = FALSE
        )
        )
        fig <- subplot( fig1_, fig2, fig3, fig4 ,nrows = 2, margin = 0.05) %>% 
            layout( #title = "Specs with Subplot Title", 
                plot_bgcolor='#e5ecf6', 
                xaxis = list( zerolinecolor = '#ffff', 
                              zerolinewidth = 2,
                              gridcolor = 'ffff'), 
                yaxis = list( zerolinecolor = '#ffff',
                              zerolinewidth = 2,
                              gridcolor = 'ffff'),
                bargap = 0.1,
                annotations = annotations,
                showlegend=FALSE,
                showlegend2=FALSE 
            ) 
        
    })
    
    output$plot_txt <- renderText({
        "This tab displays the scatter plot with regression line. \n"
    })
    
    output$plot <- renderPlot({
        regdata = workDat()
        if (input$cut == "All"){
            m0 = lm(dmnd[[input$Y]]~dmnd[[input$X]])
            plot(dmnd[[input$X]], dmnd[[input$Y]],
                 main = paste("Relationship between", input$Y, "and", input$X),
                 type = "p",
                 pch = 19,
                 col = "blue",
                 xlab = input$X,
                 ylab = input$Y
            )
            abline(m0, lwd = 2, col ="red")
        } else {
            m1 = lm(workDat()[[input$Y]]~workDat()[[input$X]])
            plot(workDat()[[input$X]], workDat()[[input$Y]],
                 main = paste("Relationship between", input$Y, "and", input$X),
                 type = "p",
                 pch = 19,
                 col = "blue",
                 xlab = input$X,
                 ylab = input$Y
            )
            abline(m1, lwd = 2, col = "red")
        }
    })
    
    output$table_txt <- renderText({
        "This tab summarizes the inferential statistics in the regression model. \n "
    })
    
    output$table <- renderTable({
        br()
        br()
        br()
        m0 = lm(workDat()[[input$Y]] ~workDat()[[input$X]])
        regcoef = data.frame(coef(summary(m0)))
        regcoef$Pvalue = regcoef[,names(regcoef)[4]]
        regcoef$Variable = c("Intercept", input$X)
        regcoef[,c(6, 1:3, 5)]
    })
    
    output$pred_txt <- renderText({
        "This tab displays the regression results.\n"
    })
    
    output$predPlt <- renderPlot({
        m3 = lm(workDat()[[input$Y]] ~ workDat()[[input$X]])
        pred.y = coef(m3)[1] + coef(m3)[2]*input$newX
        plot(workDat()[[input$X]], workDat()[[input$Y]], 
             xlab = input$X,
             ylab = input$Y,
             main = paste("Relationship between", input$Y, "and", input$X)
        )
        abline(m3,col = "red",lwd = 1,lty=2)
        points(input$newX, pred.y, pch = 19, col = "red", cex = 2)
    })
}

shinyApp(ui = ui, server = server)

