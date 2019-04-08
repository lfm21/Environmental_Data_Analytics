library(shiny)
library(ggplot2)
x <- read.csv()


# ui
ui <- fluidPage(
   
   # title
   titlePanel('normal distribution'),
   # normal distribution formula
   headerPanel(withMathJax('$$f(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}} e^\\frac{-(x-\\mu)^{2}}{2\\sigma^2}$$')), 
   
   # sidebar with slider and checkbox inputs
   sidebarLayout(
      sidebarPanel(
         sliderInput('n',
                     'sample size',
                     min = 0,
                     max = 900,
                     value = 100),
         sliderInput('mu',
                     withMathJax('$$\\mu$$'), 
                     min = -5,
                     max = 5,
                     value = 0,
                     step = 0.1),
         sliderInput('sigma', 
                     withMathJax('$$\\sigma$$'),
                     min = 0.01,
                     max = 10,
                     value = 5,
                     step = 0.1),
         checkboxInput('histogram', label = 'histogram', value = TRUE), 
         checkboxInput('density', label = 'density', value = TRUE),
         textInput("title", label = "it's called a box")
      ),
      
      # main plot
      mainPanel(
         plotOutput('dist')
      )
   )
)

# server
server <- function(input, output) {
   
   output$dist <- renderPlot({ # adjust the number of bins based on the sample size
     
     n <- input$n
     if(n < 25){
       bins <- 25  #if we have >25 sample size, give me 10 bins, else do 20/50
     } else if(n < 50){
       bins <- 25
     } else if(n <= 500){
       bins <- 50
     }
     
     set.seed(1001) # prevent output from changing based on plot type; set the seed to always be the same, even if someone changces the numebrs

      x <- rnorm(input$n, input$mu, input$sigma) # determine distribution
      
      # draw the histogram with the specified number of bins
      
     p <- ggplot() + scale_x_continuous(limits = c(-20, 20))
     if(input$histogram) p <- p + geom_histogram(aes(x, y = ..density..), bins = bins, colour = 'black', fill = 'white') # add hist if checked
     if(input$density) p <- p + geom_density(aes(x), alpha=.2, fill="#FF6666") # add density if checked
     p + theme_minimal() +
       labs(title = input$title)
   })
}

# run application
shinyApp(ui = ui, server = server)

