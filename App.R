
library(shiny)
library(ggplot2)

ui <- fluidPage(
        tags$h1("Play Around With Probability Distributions"),
        tags$p("Click on any one of the below tabs. ",
               "Play around with the myriad options, and click on ", 
               strong("Display"), 
               " to view the distribution."),
        tabsetPanel(              
                tabPanel(title = "Normal",
                         wellPanel(
                         fluidRow(column(4,
                                         sliderInput(inputId = "num_n", 
                                                     label = "Choose number of observations",
                                                     value = 50,
                                                     min = 20, max = 200, step = 10)),
                                  column(3, offset = 1,
                                         radioButtons(inputId = "mean_n",
                                                      inline = TRUE,
                                                      label = "Choose a value for the mean",
                                                      choices = c(0,5,10,50,100))),
                                  column(3, offset = 1,
                                         radioButtons(inputId = "sd_n",
                                                      inline = TRUE,
                                                      label = "Choose the standard deviation",
                                                      choices = c(1,2,5,10,20)))),
                         actionButton(inputId = "renorm", label = "Display"),
                         plotOutput("norm")
                )),
                tabPanel(title = "Uniform",
                         wellPanel(
                         fluidRow(column(4,
                                         sliderInput(inputId = "num_u", 
                                                     label = "Choose number of observations",
                                                     value = 50,
                                                     min = 20, max = 200, step = 10)),
                                  column(3, offset = 1,
                                         radioButtons(inputId = "min_u",
                                                      inline = TRUE,
                                                      label = "Choose the minumum value",
                                                      choices = c(0,5,10,50,100))),
                                  column(3, offset = 1,
                                         radioButtons(inputId = "range_u",
                                                      inline = TRUE,
                                                      label = "Choose the range",
                                                      choices = c(5,10,20,30,50)))),
                         actionButton(inputId = "reunif", label = "Display"),
                         plotOutput("unif")
                )),
                tabPanel(title = "Poisson",
                         wellPanel(
                         fluidRow(column(4,
                                         sliderInput(inputId = "num_p", 
                                                     label = "Choose number of observations",
                                                     value = 50,
                                                     min = 20, max = 200, step = 10)),
                                  column(4, offset = 1,
                                         radioButtons(inputId = "lambda_p",
                                                      inline = TRUE,
                                                      label = "Choose value of lambda",
                                                      choices = c(5,10,50,100,200)))),
                         actionButton(inputId = "repois", label = "Display"),
                         plotOutput("pois")
                ))
        )
        
)

server <- function(input, output) {
        
        rv <- reactiveValues(
                norm = integer(0),
                unif = integer(0),
                pois = integer(0))
        
        observeEvent(input$renorm, { rv$norm <- rnorm(input$num_n, 
                                                      as.integer(input$mean_n), 
                                                      as.integer(input$sd_n)) })
        observeEvent(input$reunif, { rv$unif <- runif(input$num_u, 
                                                      as.integer(input$min_u), 
                                                      as.integer(input$min_u) + as.integer(input$range_u)) })
        observeEvent(input$repois, { rv$pois <- rpois(input$num_p, 
                                                      as.integer(input$lambda_p)) })
        
        output$norm <- renderPlot({
                ggplot(data.frame(X = rv$norm), aes(X)) +
                        geom_histogram(binwidth = 1, boundary = 0,
                                       fill = "white", 
                                       colour = "seagreen4") +
                        labs(x = "Distribution of observations",
                             y = "Count",
                             title = "Random draws from a Normal Distribution")
        })
        output$unif <- renderPlot({
                ggplot(data.frame(X = rv$unif), aes(X)) +
                        geom_histogram(binwidth = 1, boundary = 0,
                                       fill = "white", 
                                       colour = "seagreen4") +
                        labs(x = "Distribution of observations",
                             y = "Count",
                             title = "Random draws from a Uniform Distribution") +
                        xlim(isolate({as.integer(input$min_u)}), 
                             isolate({as.integer(input$min_u) + 
                                             as.integer(input$range_u)}))
        })
        output$pois <- renderPlot({
                ggplot(data.frame(X = rv$pois), aes(X)) +
                        geom_histogram(binwidth = 1, boundary = 0,
                                       fill = "white", 
                                       colour = "seagreen4") +
                        labs(x = "Distribution of observations",
                             y = "Count",
                             title = "Random draws from a Poisson Distribution")
        })
}

shinyApp(server = server, ui = ui)




