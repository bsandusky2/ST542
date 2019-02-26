source("SubsetData.R", local = TRUE)
cats<- as.data.frame(cats[,c(1:7,10,11)])
cats<- cats[complete.cases(cats),]
nms<- names(cats)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel("PCR Positive Cats"),
  mainPanel( height = "400px"),
  tabsetPanel(type = "tabs",
              tabPanel("Plots", plotOutput("P"),
                       selectInput('x', 'X - Axis', choices = nms, selected = "Breed_Group"),
                       selectInput('y', 'Legend', choices = nms, selected = "Results")
              ),
              tabPanel("Regression", verbatimTextOutput("summary"),
                       sidebarPanel(
                         p("Select the inputs for the Independent Variable"),
                         checkboxGroupInput("IndVariables","Select Regressors", choices = nms[-7],selected = nms[1]),
                         p("Dependent Variable is set to PCR Test results"),
                         h4(textOutput("caption")),
                         verbatimTextOutput(outputId = "Matrix")
                       ),
                       mainPanel(
                         verbatimTextOutput(outputId = "fit"),
                         plotOutput("ROCPLOT")
                       )
              ),
              tabPanel("Prediction", tableOutput("table"),
                       sidebarPanel(
                         p("Input Values for Independent Variables. These will be used to determine probability of a positive test results"),
                         selectInput("Sex", "Sex", choices = levels(cats$Sex), selected = "F"),
                         selectInput("Housing", "Housing", choices = levels(cats$Housing), selected = "Multiple"),
                         selectInput("Collection", "Collection", choices = levels(cats$Collection), selected = "Flushed"),
                         selectInput("Smear", "Smear", choices = c(0,1), selected = 0),
                         selectInput("Flag", "Flagellated", choices = c(0,1), selected = 0),
                         selectInput("Treat", "Treatment", choices = c(0,1), selected = 0),
                         selectInput("Age", "Age Group", choices = levels(cats$AgeGroup), selected = "Young"),
                         selectInput("Breed", "Breed Group", choices = levels(cats$Breed_Group), selected = "Purebred")
                       ),
                       mainPanel(
                         p("The probability of a cat testing positive for T.foetus given the model selected on the Regression tab is:"),
                         verbatimTextOutput(outputId = "Prediction")
                       )
              )
  )
)

server <- function(input, output) {
  output$P <- renderPlot({
     xvar = as.factor(cats[,input$x])
     yvar = as.factor(cats[,input$y])
    ggplot(cats, aes_(x = xvar, fill = yvar)) +
      geom_bar(position = "dodge") +
      theme_classic() +
      labs(x = input$x, fill = input$y)
  }, height = 400, width = 700)
  
  checkedVal <- reactive({
    perm.vector <- as.vector(input$IndVariables)
    predForm<-ifelse(length(perm.vector)>0,
                     predictors<-paste(perm.vector,collapse="+"),
                     "1")
    lmForm<-paste("Results~",predForm,sep="")
  })
  
  fitModel<-reactive({
    fitFormula<-as.formula(checkedVal())
    glm(fitFormula,data=cats, family= binomial(link = "logit"), na.action = na.exclude)
  })
  
  preddata<- reactive({
    Sex<- input$Sex
    Housing<- input$Housing
    Collection<- input$Collection
    Smear<- as.numeric(input$Smear)
    Flagellated<- as.numeric(input$Flag)
    Treatment<- as.numeric(input$Treat)
    AgeGroup<- input$Age
    Breed_Group<- input$Breed
    predictiondata<- data.frame(Sex, Housing, Collection,Smear, Flagellated, Treatment,AgeGroup,Breed_Group)
    predict(fitModel(),predictiondata, type = "response")
  })
  
  output$caption <- renderText({
    checkedVal()
  })
  
  output$fit <- renderPrint({
    summary(fitModel())
  })
  
  output$Prediction<- renderPrint({
    preddata()
  })
  
  output$Matrix <- renderPrint({
    fit<- ifelse(fitModel()$fitted.values>= .5,1,0)
    fit<- as.factor(fit)
    res<- as.factor(cats$Results)
    confusionMatrix(fit, res, positive = "1") 
    
  })
  
  rock<- reactive({
    predict(fitModel(), type = "response")
  })
  
  output$ROCPLOT <- renderPlot({
    plot(roc(cats$Results ~rock() , data = cats),auc.polygon = TRUE, print.auc = TRUE,auc.polygon.col = "cornflowerblue")}, height = 400, width = 600)
  
}

shinyApp(ui = ui, server = server)

