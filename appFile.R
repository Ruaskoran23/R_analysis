#practising on dashboards using shiny
library(shiny)
library(tidyverse)

 
#code block for UI
UI <- fluidPage(
  titlePanel("DIABETES DIAGNOSTICS USING 100+ PATIENTS RECORDS"),#addition of title
 #first fluidrow with input and output
   fluidRow(
    column(3, 
           wellPanel(
      p("Choose variables to see correlation on the right:"),
      
      selectInput('continuous_variable',
                  'Select one of the following',
                  choices = c('FBS', 'HbA1c', 'BMI', 'Age')),
      p("A positive correlation indicates a strong diabetic factor between variables"),
      p("FBS = fasting blood sugar (mmol/L)", br(),
        "BMI = Body Mass Index", br(),
        "HbA1C = Hemoglobin A1C")
    )
    ),
    column(9, 
           plotOutput('p1'),
           plotOutput('p2')) #plotting of the output of the first image
  ),
 
  #second fluidrow with input and output
  fluidRow(
    column(3, 
           wellPanel(
             p("Select an option to view how it affects diagnosis status"),
             selectInput('categorical_variable',
                          'Select one supposed contributing factor:',
                          choices = c('Gender', 
                                      'Smoking',
                                      'Exercise',
                                      'Diet'
                                      )
                          )
           )
           ),
    column(9, 
           plotOutput('p3'))
  )
  
 
)

#loading of csv file into dataframe

dcf<- read_csv('diabetes.csv')
#add the created data frame into a reactive function
df1 <- reactive(dcf)

#code for the server logic
Server <- function(input, output){
  #first output plot using the added input parameters in UI
  #using box plot and histograms to represent the continuous variables
  output$p1 <- renderPlot({
   if(input$continuous_variable == 'FBS'){
     ggplot(df1(), aes_string(x = input$continuous_variable,
                              y = 'Age',
                              color = 'Blood_Pressure')) + 
       geom_point()+ #Adds a scatter plot
       stat_smooth( position = 'identity',
                    method = 'lm',
                    color = 'red') + 
       facet_wrap(~Diagnosis) + 
       labs(title = 'DISTRIBUTION OF DATA') + 
       theme_minimal()
   }else{
     ggplot(df1(), aes_string(x = input$continuous_variable)) + 
       geom_histogram(binwidth = 3,
                      color = 'red') + #Adds histogram
       facet_wrap(~Diagnosis) +
       labs(title = 'DISTRIBUTION OF DATA') + 
       theme_minimal()
   }
  })

#plotting the second output
  output$p2 <- renderPlot({
    if(input$continuous_variable == 'BMI'){
      ggplot(df1(), aes_string(x = input$continuous_variable,
                               y = 'HbA1c',
                               color = 'Blood_Pressure')) + 
        geom_point() + 
        stat_smooth(position = 'identity',
                    method = 'lm',
                    color = 'red') +
        facet_wrap(~Diagnosis) + 
        labs(title = 'CORRELATION OF DATA') + 
        theme_minimal()
    }
  })
  
#output of plot p3
  output$p3 <- renderPlot({
    ggplot(df1(), aes_string(x = input$categorical_variable,
                             fill = 'Diagnosis')) + 
      geom_bar(position = 'stack') + 
      facet_wrap(~Blood_Pressure) +
      labs(title = 'STATE OF DIABETES DIAGNOSIS USING BLOOD PRESSURE') + 
      theme_minimal()
             
  })
}

#running the shiny app
shinyApp(ui = UI, server = Server)