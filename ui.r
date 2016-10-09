library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",
  titlePanel("Word prediction"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$script(HTML('
                                 Shiny.addCustomMessageHandler("jsCode",
                                 function(message) {
                                 eval(message.code);
                                 }
                                 );
                                 '))),
      h3("Please choose "),
      radioButtons("button1",label="choose",choices=list("Prediction without candidates"=1,"Prediction with candidates input"=2)),
      textInput("text1",label="candidate1"),
      textInput("text2",label="candidate2"),
      textInput("text3",label="candidate3"),
      textInput("text4",label="candidate4"),
      h3("Instruction"),
      p("The app predicts the next word based on the last N non-stopwords you input. And it only predicts non-stopwords. But if it cannot find any non-stopword for prediction, it will predict 5 most common stopwords.
        You can type in your phrase or sentence into the text input box. If you add a space at the end, it will start to predict. Or you can press the button below. You can also provide the candidates, and press the button
         'prediction with candidates', it will only predict within the candidates.")
      ),
    
    
    mainPanel(
      h3("Please enter your sentence"),
      textInput("sentence",label="Type in your sentence and add space at the end or press the button below"),
      actionButton("submit1", "Predict"),
      actionButton("submit2", "Predict with candidates"),
      h4(" "),
      h3("Suggested next word:"),
      textOutput("hints"),
      textOutput("outputtext"),
      h3("Suggested next word from candidates:"),
      textOutput("hints2"),
      textOutput("outputtext2"),
      
      img(src="example1.png", height = 220, width = 400),
      img(src="example2.png", height = 330, width = 400)
    )
    
      ) 
    )
  )
