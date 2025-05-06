library(shiny)
library(DT)

# Data storage
responses <- reactiveVal(data.frame(
  ID = numeric(0),
  Answer = character(0),
  Votes = numeric(0),
  stringsAsFactors = FALSE
))

password <- "conference1"

ui <- fluidPage(
  titlePanel("Conference interactive survey"),
  tabsetPanel(
    tabPanel("Guest panel",
             sidebarLayout(
               sidebarPanel(
                 textInput("answer", "Enter your idea:"),
                 actionButton("submit", "Submit")
               ),
               mainPanel(
                 h4("Vote for existing ideas:"),
                 DTOutput("vote_table_public")
               )
             )
    ),
    tabPanel("Admin panel",
             sidebarLayout(
               sidebarPanel(
                 passwordInput("admin_pwd", "Admin password:"),
                 conditionalPanel(
                   condition = "input.admin_pwd == 'conference1'",
                   actionButton("restart", "Restart survey")
                 )
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.admin_pwd == 'conference1'",
                   h4("Admin overview:"),
                   DTOutput("admin_table")
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  user_votes <- reactiveVal(integer())
  
  # Submit a new answer
  observeEvent(input$submit, {
    req(input$answer)
    current <- responses()
    new_entry <- data.frame(
      ID = ifelse(nrow(current) == 0, 1, max(current$ID) + 1),
      Answer = input$answer,
      Votes = 0,
      stringsAsFactors = FALSE
    )
    responses(rbind(current, new_entry))
    updateTextInput(session, "answer", value = "")
  })
  
  # Restart survey
  observeEvent(input$restart, {
    req(input$admin_pwd == password)
    responses(data.frame(ID = numeric(0), Answer = character(0), Votes = numeric(0), stringsAsFactors = FALSE))
    user_votes(integer())
    updateTextInput(session, "admin_pwd", value = "")
  })
  
  # Public voting table
  output$vote_table_public <- renderDT({
    current <- responses()
    votes_by_user <- user_votes()
    current$YourVotes <- votes_by_user[as.character(current$ID)]
    current$YourVotes[is.na(current$YourVotes)] <- 0
    datatable(current[, c("Answer", "YourVotes")], selection = "single", rownames = FALSE, options = list(dom = 't'))
  })
  
  # User voting action
  observeEvent(input$vote_table_public_rows_selected, {
    selected <- input$vote_table_public_rows_selected
    if(length(selected)){
      current <- responses()
      current$Votes[selected] <- current$Votes[selected] + 1
      responses(current)
      
      votes_by_user <- user_votes()
      id_char <- as.character(current$ID[selected])
      votes_by_user[id_char] <- ifelse(is.na(votes_by_user[id_char]), 1, votes_by_user[id_char] + 1)
      user_votes(votes_by_user)
    }
  })
  
  # Admin table
  output$admin_table <- renderDT({
    datatable(responses()[order(-responses()$Votes), -1], rownames = FALSE)
  })
}

shinyApp(ui, server)