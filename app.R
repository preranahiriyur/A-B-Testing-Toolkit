library(shiny)
library(shinythemes)

ui <- navbarPage(title='A/B Testing Toolkit',
                theme = shinytheme("sandstone"),
    tabPanel(title='Calculate Sample Size',
      sidebarLayout(
        sidebarPanel(
          numericInput(inputId = "baseline", 
                        label = "Baseline Rate/ Probability", 
                        value = 10, min = 0, max = 100),
          br(),
          numericInput(inputId = "effectsize", 
                       label = "Minimum Detectable Effect Size", 
                       value = 10, min = 0, max = 100),
          radioButtons("relative", label = "",
                       choices = list("Relative" = T, "Absolute" = F), 
                       selected = F),
          br(), 
          sliderInput(inputId = "power",
                      label = "Power", 
                      value = 80,
                      min = 0, max = 100),
          br(),
          sliderInput(inputId = "conflevel", 
                      label = "Confidence Level",
                      value = 95,
                      min = 0, max = 100), 
          br(),
          actionButton(inputId = "calculate",
                       label = "Calculate", 
                       class="btn btn-primary", 
                       style="align:center; padding:5px 100px")
        ),
        mainPanel(
        fluidRow(
        sidebarPanel(width = 12,
          h3("Sample Size:"),
          h2(align ="center",style = "font-size: 400%; letter-spacing: 3px;", textOutput(outputId = "samplesize"))), 
        sidebarPanel(width=12, 
                     h3("Formula:"), 
                     tags$img(src='ss_formula.png', height=400, width=600),
                     br(),
                     tags$a(href='https://towardsdatascience.com/required-sample-size-for-a-b-testing-6f6608dd330a', 
                            "Source: Towards Data Science", style="color:blue")))
        )
    )
  ), 
  tabPanel(title='Calculate Significance', 
           fluidRow(
             sidebarPanel(
               h3("Group A"),
               br(), 
               numericInput(inputId = "numA", 
                        label = "Number of Participants", 
                        value = 100), 
               numericInput(inputId = "XA", 
                            label = "Number of Successes", 
                            value = 50)),
             sidebarPanel(
               h3("Group B"),
               br(),
               numericInput(inputId = "numB", 
                        label = "Number of Participants", 
                        value = 100), 
               numericInput(inputId = "XB", 
                            label = "Number of Successes", 
                            value = 50)), 
            sidebarPanel(
              actionButton(inputId = "analyze",
                           label = "Analyze", 
                           class="btn btn-primary", 
                           style="align:center; padding:5px 100px"), 
              br(), 
              br(),
              p("Calculate the p-value and confidence interval for the 
                difference in proportion."),
              p("The difference is statistically significant at 5% level if
                the p-value is less that 0.05 OR if the confidence interval
                does not contain 0."), 
              p("The confidence interval shows the range within which the 
                difference would lie 95% of the time.")
              )), 
            fluidRow(
              sidebarPanel(
                h3("Confidence Interval:"),
                h1(style="text-align:center;", textOutput(outputId = "confint"))
              ), 
              sidebarPanel(
                h3("p-value:"),
                h1(style="text-align:center;", textOutput(outputId = "pvalue"))
              )
            )
           )
)

server <- function(input, output, session) {
  observeEvent(input$numA, {
    updateSliderInput(session, "XA", max = input$numA)
  })
  
  observeEvent(input$numB, {
    updateSliderInput(session, "XB", max = input$numA)
  })
  
  alpha <- reactive({1-input$conflevel/100})
  power <- reactive({input$power/100})
  p1 <- reactive({input$baseline/100})
  
  # effectsize <- reactiveValues()
  # 
  # observe({
  #   effectsize$x <- input$relative
  #   effectsize$y <- ifelse(state$x == T, input$effectsize/100*input$baseline/100, input$effectsize/100)
  # })
  x <- reactive({input$relative})
  effectsize <- reactive({input$effectsize/100})
  effect <- reactive(if (x() == T) effectsize()*p1() else effectsize())
  p2 <- reactive({p1()-effect()})
  
  ss <- eventReactive(input$calculate,{
    k <- 1 
    p <- (p1()+k*p2())/(1+k)
    q <- (1-p)
    q1 <- 1-p1()
    q2 <- 1-p2()
    n1 <- (sqrt(p*q*(1+1/k))*qnorm(1-alpha()/2) + sqrt(p1()*q1 + p2()*q2/k)*qnorm(power()))^2/effect()^2
    n2 <- k*n1
    #from towardsdatascience article 
    round(n1)
  }
  )
  
  output$samplesize <- renderText({
    paste(ss())
  })
  
  ci <- eventReactive(input$analyze, {
    pAhat <- {input$XA/input$numA}
    pBhat <-{input$XB/input$numB}
    phatpooled <- {(input$XA+input$XB)/(input$numA+input$numB)}
    sepooled <- {sqrt(phatpooled*(1-phatpooled)*((1/input$numA)+(1/input$numB)))}
    dhat <- pBhat - pAhat
    marginoferr <- {sepooled*qnorm(1-alpha()/2)}
    confint <- c(dhat-marginoferr, dhat+marginoferr)
    paste(round(confint[1],2), ",", round(confint[2], 2))
  })
  
  pval <- eventReactive(input$analyze, {
    pAhat <- {input$XA/input$numA}
    pBhat <-{input$XB/input$numB}
    phatpooled <- {(input$XA+input$XB)/(input$numA+input$numB)}
    sepooled <- {sqrt(phatpooled*(1-phatpooled)*(1/input$numA+1/input$numB))}
    dhat <- pBhat - pAhat
    marginoferr <- {sepooled*qnorm(1-alpha()/2)}
    stddhat <- dhat/sepooled
    if (pAhat > pBhat) {
      round(2*pnorm(stddhat, lower.tail=TRUE), 3)
    }
    else{
    round(2*pnorm(stddhat, lower.tail=FALSE), 3)
    }
  })
  
  output$pvalue <- renderText({
    pval()
  })
  
  output$confint <- renderText({
    ci()
  })
  
}

shinyApp(ui, server)