library(shiny)
library(DT)

# Global storage
global_data <- reactiveValues(
  conferences = list(),      # Conference ID -> list of questions
  settings = list(),         # Conference ID -> vote settings
  user_counts = list()       # Conference ID -> user session tracking
)

password <- "conference1"

ui <- fluidPage(
  titlePanel("Multi-Question Conference Polling"),
  
  fluidRow(
    column(6,
           selectInput("conf_id_select", "Select Conference ID:", choices = NULL)
    ),
    column(6,
           passwordInput("admin_pwd", "Admin password:"),
           conditionalPanel(
             condition = "input.admin_pwd == 'conference1'",
             textInput("new_conf_id", "Create New Conference ID:"),
             actionButton("create_conf", "Create")
           )
    )
  ),
  
  uiOutput("user_count_ui"),
  
  tabsetPanel(
    tabPanel("Guest Panel",
             uiOutput("question_ui_guest"),
             sidebarLayout(
               sidebarPanel(
                 textInput("answer", "Suggest an answer:"),
                 actionButton("submit_answer", "Submit")
               ),
               mainPanel(
                 h4("Vote for an answer:"),
                 DTOutput("vote_table"),
                 textOutput("vote_warning")
               )
             )
    ),
    tabPanel("Admin Panel",
             conditionalPanel(
               condition = "input.admin_pwd == 'conference1'",
               fluidRow(
                 column(6,
                        textInput("new_question_text", "New Question Text:"),
                        actionButton("add_question", "Add Question")
                 ),
                 column(6,
                        selectInput("delete_question_id", "Delete Question:", choices = NULL),
                        actionButton("delete_question", "Delete")
                 )
               ),
               fluidRow(
                 column(6,
                        numericInput("max_votes", "Max votes per user:", value = 5, min = 1),
                        checkboxInput("allow_multiple", "Allow multiple votes per option", value = FALSE),
                        actionButton("apply_settings", "Apply Settings")
                 ),
                 column(6,
                        selectInput("restart_question_id", "Restart Question Responses:", choices = NULL),
                        actionButton("restart_question", "Restart")
                 )
               ),
               uiOutput("question_ui_admin"),
               h4("Admin View of Answers:"),
               DTOutput("admin_table")
             )
    )
  )
)

server <- function(input, output, session) {
  session_id <- paste0("user_", as.integer(Sys.time()), "_", sample(10000, 1))
  user_votes <- reactiveVal(list())
  vote_message <- reactiveVal("")
  
  # Update Conference ID dropdowns
  observe({
    confs <- names(global_data$conferences)
    updateSelectInput(session, "conf_id_select", choices = confs, selected = isolate(input$conf_id_select))
  })
  
  # Update Admin's question dropdowns (delete/restart)
  observe({
    req(input$conf_id_select)
    conf <- input$conf_id_select
    questions <- global_data$conferences[[conf]]
    if (length(questions) == 0) return()
    
    question_ids <- names(questions)
    question_texts <- sapply(questions, function(q) q$text)
    names(question_ids) <- question_texts
    
    updateSelectInput(session, "delete_question_id", choices = question_ids)
    updateSelectInput(session, "restart_question_id", choices = question_ids)
  })
  
  # Track users
  observe({
    req(input$conf_id_select)
    conf <- input$conf_id_select
    
    if (is.null(global_data$user_counts[[conf]])) {
      global_data$user_counts[[conf]] <- list()
    }
    global_data$user_counts[[conf]][[session_id]] <- TRUE
    
    session$onSessionEnded(function() {
      global_data$user_counts[[conf]][[session_id]] <- NULL
    })
  })
  
  output$user_count_ui <- renderUI({
    conf <- input$conf_id_select
    if (is.null(global_data$user_counts[[conf]])) return(NULL)
    h5(paste0("Users online for ", conf, ": ", length(global_data$user_counts[[conf]])))
  })
  
  # Admin: Create Conference ID
  observeEvent(input$create_conf, {
    req(input$admin_pwd == password)
    id <- trimws(input$new_conf_id)
    if (id == "" || id %in% names(global_data$conferences)) {
      showNotification("Invalid or duplicate Conference ID", type = "error")
      return()
    }
    
    global_data$conferences[[id]] <- list()
    global_data$settings[[id]] <- list(max_votes = 5, allow_multiple = FALSE)
    global_data$user_counts[[id]] <- list()
    
    updateTextInput(session, "new_conf_id", value = "")
    updateSelectInput(session, "conf_id_select", selected = id)
    showNotification(paste("Created Conference ID:", id))
  })
  
  # Admin: Add Question
  observeEvent(input$add_question, {
    req(input$admin_pwd == password, input$conf_id_select, input$new_question_text)
    conf <- input$conf_id_select
    question_id <- paste0("q", as.integer(Sys.time()))
    global_data$conferences[[conf]][[question_id]] <- list(
      text = input$new_question_text,
      responses = data.frame(ID = integer(), Answer = character(), Votes = integer(), stringsAsFactors = FALSE)
    )
    updateTextInput(session, "new_question_text", value = "")
    showNotification("Question added.")
  })
  
  # Admin: Delete Question
  observeEvent(input$delete_question, {
    req(input$admin_pwd == password, input$conf_id_select, input$delete_question_id)
    global_data$conferences[[input$conf_id_select]][[input$delete_question_id]] <- NULL
    showNotification("Question deleted.", type = "warning")
  })
  
  # Admin: Restart Question
  observeEvent(input$restart_question, {
    req(input$admin_pwd == password, input$conf_id_select, input$restart_question_id)
    conf <- input$conf_id_select
    qid <- input$restart_question_id
    if (!is.null(global_data$conferences[[conf]][[qid]])) {
      global_data$conferences[[conf]][[qid]]$responses <- data.frame(ID = integer(), Answer = character(), Votes = integer(), stringsAsFactors = FALSE)
      showNotification("Responses reset.")
    }
  })
  
  # Admin: Apply Vote Settings
  observeEvent(input$apply_settings, {
    req(input$admin_pwd == password, input$conf_id_select)
    global_data$settings[[input$conf_id_select]] <- list(
      max_votes = input$max_votes,
      allow_multiple = input$allow_multiple
    )
  })
  
  # Guest: Render Question Selector
  output$question_ui_guest <- renderUI({
    req(input$conf_id_select)
    conf <- input$conf_id_select
    questions <- global_data$conferences[[conf]]
    if (length(questions) == 0) return(h5("No questions yet."))
    
    selectInput("question_id_guest", "Choose a question:", choices = setNames(names(questions), sapply(questions, `[[`, "text")))
  })
  
  # Admin: Render Question Selector
  output$question_ui_admin <- renderUI({
    req(input$conf_id_select)
    conf <- input$conf_id_select
    questions <- global_data$conferences[[conf]]
    if (length(questions) == 0) return(h5("No questions available."))
    
    selectInput("question_id_admin", "Select a question to view:", choices = setNames(names(questions), sapply(questions, `[[`, "text")))
  })
  
  # Guest: Submit Answer
  observeEvent(input$submit_answer, {
    req(input$conf_id_select, input$question_id_guest, input$answer)
    conf <- input$conf_id_select
    qid <- input$question_id_guest
    
    responses <- global_data$conferences[[conf]][[qid]]$responses
    new_id <- if (nrow(responses) == 0) 1 else max(responses$ID) + 1
    
    new_entry <- data.frame(
      ID = new_id,
      Answer = input$answer,
      Votes = 0,
      stringsAsFactors = FALSE
    )
    
    responses <- rbind(responses, new_entry)
    global_data$conferences[[conf]][[qid]]$responses <- responses
    updateTextInput(session, "answer", value = "")
  })
  
  # Guest: Voting Table
  output$vote_table <- renderDT({
    req(input$conf_id_select, input$question_id_guest)
    conf <- input$conf_id_select
    qid <- input$question_id_guest
    responses <- global_data$conferences[[conf]][[qid]]$responses
    if (nrow(responses) == 0) return(NULL)
    
    votes_by_user <- user_votes()[[qid]]
    if (is.null(votes_by_user)) votes_by_user <- integer()
    
    id_chars <- as.character(responses$ID)
    your_votes <- votes_by_user[id_chars]
    your_votes[is.na(your_votes)] <- 0
    
    responses$YourVotes <- your_votes
    datatable(responses[, c("Answer", "YourVotes")], selection = "single", rownames = FALSE, options = list(dom = 't'))
  })
  
  output$vote_warning <- renderText({ vote_message() })
  
  # Guest: Voting Logic
  observeEvent(input$vote_table_rows_selected, {
    req(input$conf_id_select, input$question_id_guest)
    conf <- input$conf_id_select
    qid <- input$question_id_guest
    selected <- input$vote_table_rows_selected
    if (length(selected) == 0) return()
    vote_message("")
    
    settings <- global_data$settings[[conf]]
    responses <- global_data$conferences[[conf]][[qid]]$responses
    votes_all <- user_votes()
    votes <- votes_all[[qid]]
    if (is.null(votes)) votes <- integer()
    
    id_char <- as.character(responses$ID[selected])
    total_votes <- sum(votes, na.rm = TRUE)
    current_vote <- votes[id_char]
    if (is.na(current_vote)) current_vote <- 0
    
    max_votes <- if (!is.null(settings)) settings$max_votes else Inf
    allow_multiple <- if (!is.null(settings)) settings$allow_multiple else TRUE
    
    if (total_votes >= max_votes) {
      vote_message(paste("Vote limit reached (", max_votes, ")"))
      return()
    }
    if (!allow_multiple && current_vote >= 1) {
      vote_message("Only one vote allowed per option.")
      return()
    }
    
    responses$Votes[selected] <- responses$Votes[selected] + 1
    global_data$conferences[[conf]][[qid]]$responses <- responses
    
    votes[id_char] <- current_vote + 1
    votes_all[[qid]] <- votes
    user_votes(votes_all)
  })
  
  # Admin: Results Table
  output$admin_table <- renderDT({
    req(input$conf_id_select, input$question_id_admin)
    conf <- input$conf_id_select
    qid <- input$question_id_admin
    responses <- global_data$conferences[[conf]][[qid]]$responses
    if (nrow(responses) == 0) return(NULL)
    datatable(responses[order(-responses$Votes), -1], rownames = FALSE)
  })
}

shinyApp(ui, server)
