

library(shiny)
library(magrittr)
library(dplyr)
library(shinythemes)
library(ggplot2)


ui <- shinyUI(fluidPage(theme=shinytheme("cosmo"),
   
   titlePanel("Quadratic equation solver"),
   HTML("This aplication will solve quadratic equation having the form <strong>ax<sup>2</sup> + bx + c = 0</strong>"),
   div("Where x represents an unknown, and a, b, and c represent known numbers such that a is not equal to 0."),
   hr(),
   

    sidebarLayout(
      sidebarPanel(
        div("Enter coefficients:"),
          div(style="display:inline-block; width:80px", textInput("A", label = "A", value = 2)),
          div(style="display:inline-block; width:80px", textInput("B", label = "B", value = 18)),
          div(style="display:inline-block; width:80px", textInput("C", label = "C", value = 5)),
    
        div("Coefficients will be used to determine the Discriminant.")
                  ),

      
      mainPanel(
        
        h3("Data"),
          div("Your equation is..."),
        
        div(
          strong(
                    htmlOutput(style="display:inline-block", "my_A"), HTML("* x <sup>2</sup> +"), htmlOutput(style="display:inline-block", "my_B"), HTML("* x + "), htmlOutput(style="display:inline-block", "my_C"), HTML("= 0")
                )
          ),
        

        div("Discriminant is..."),
          strong(
            textOutput("my_D")
        ),
        
         div("Solution is..."),
          strong(
            textOutput("my_S")
                ),


            div(h3("Visual representation"), plotOutput('plot'))
      )
      
   ),
   
  hr(),
  
  HTML("Github link - <a href='https://github.com/seozoidberg/for_shiny'>https://github.com/seozoidberg/for_shiny</a>")
  
))

server <- shinyServer(function(input, output) {

  AA <- reactive(as.numeric(input$A))
  BB <- reactive(as.numeric(input$B))
  CC <- reactive(as.numeric(input$C))
  

  discr <- reactive( BB()^2 - 4 * AA() * CC())
  output$my_D <- renderPrint({ cat (discr()) })
    output$my_D2 <- reactive( discr())
  
  solution <- reactive({
    
    if(discr() == 0) { -BB() / 2 * AA() } else
      
    if(discr() > 0) { c((-BB() + sqrt(discr())) / (2 * AA()), (-BB() - sqrt(discr())) / (2 * AA())) } else
    {"no solution"}

                      })
  

  output$my_S <- renderPrint({ cat (solution()) })

  
  output$my_A <- renderPrint({ cat (AA()) })
  output$my_B <- renderPrint({ cat (BB()) })
  output$my_C <- renderPrint({ cat (CC()) })


  output$plot <- renderPlot({
    
f <- function(xx) AA()*xx^2 + BB()*xx + CC()

mi <- solution()-10
ma <- solution()+10

p <- ggplot(data.frame(x=c(mi, ma)), aes(x)) + stat_function(fun=f) + geom_vline(xintercept = solution(), colour = "red", linetype = "longdash") + geom_hline(yintercept = 0, colour = "red", linetype = "longdash")
  print(p)

                          })
  
})


shinyApp(ui = ui, server = server)

