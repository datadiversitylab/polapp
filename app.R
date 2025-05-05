library(shiny)
library(DT)

# Initialize data storage
responses <- reactiveVal(data.frame(
  ID = numeric(0),
  Answer = character(0),
  Votes = numeric(0),
  stringsAsFactors = FALSE
))

password <- "conference123" # Set your desired password here

ui <- fluidPage(
  titlePanel("Conference interactive survey"),
  sidebarLayout(
    sidebarPanel(
      textInput("answer", "Enter your idea:"),
      actionButton("submit", "Submit"),
      hr(),
      passwordInput("pwd", "Enter password to restart survey:"),
      actionButton("restart", "Restart survey")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Vote", 
                 h4("Vote for existing ideas:"),
                 DTOutput("vote_table")
        ),
        tabPanel("Top ideas",
                 h4("Top Ideas by votes"),
                 DTOutput("results_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Submit a new answer
  observeEvent(input$submit, {
    req(input$answer)
    current <- responses()
    new_entry <- data.frame(
      ID = ifelse(nrow(current)==0, 1, max(current$ID) + 1),
      Answer = input$answer,
      Votes = 0,
      stringsAsFactors = FALSE
    )
    responses(rbind(current, new_entry))
    updateTextInput(session, "answer", value = "")
  })
  
  # Restart survey
  observeEvent(input$restart, {
    req(input$pwd == password)
    responses(data.frame(ID = numeric(0), Answer = character(0), Votes = numeric(0), stringsAsFactors = FALSE))
    updateTextInput(session, "pwd", value = "")
  })
  
  # Display voting table
  output$vote_table <- renderDT({
    datatable(responses()[, c("ID", "Answer", "Votes")], selection = "single", rownames = FALSE)
  })
  
  # Voting action
  observeEvent(input$vote_table_rows_selected, {
    selected <- input$vote_table_rows_selected
    if(length(selected)){
      current <- responses()
      current$Votes[selected] <- current$Votes[selected] + 1
      responses(current)
    }
  })
  
  # Display results
  output$results_table <- renderDT({
    datatable(responses()[order(-responses()$Votes), c("Answer", "Votes")], rownames = FALSE)
  })
}

shinyApp(ui, server)