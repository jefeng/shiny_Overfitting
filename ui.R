library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Coeffecient Value",
             fluidRow(
               column(8,uiOutput("formula")),
               column(4, sliderInput("ndeg","power of x",
                                     min = 1, max = 10, value = 3)),
               column(12, plotOutput("polyReg")))),
    tabPanel("Train/Test Error",
             fluidRow(
               
               column(8,uiOutput("formula2")),
               column(4, sliderInput("ndeg2","power of x",
                                     min = 1, max = 10, value = 3)),
               column(12, plotOutput("testError"))))
    )
  )
)