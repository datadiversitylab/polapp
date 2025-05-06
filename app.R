library(shiny)
library(DT)

# Global storage
global_data <- reactiveValues(
  conferences = list(),
  settings = list(),
  user_counts = list(),
  admin_tokens = list(),
  admin_logged_in = reactiveVal(FALSE)
)

ui <- fluidPage(
  titlePanel("Multi-Question Conference Polling"),
  
  tabsetPanel(
    tabPanel("Guest Panel",
             fluidRow(
               column(6,
                      selectInput("conf_id_select_guest", "Select Conference ID:", choices = NULL)
               )
             ),
             uiOutput("user_count_ui_guest"),
             conditionalPanel(
               condition = "input.conf_id_select_guest != ''",
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
             )
    ),
    tabPanel("Admin Panel",
             fluidRow(
               column(6,
                      selectInput("conf_id_select", "Select Conference ID:", choices = NULL),
                      textInput("admin_token_input", "Admin Token:"),
                      actionButton("login_admin", "Log In as Admin")
               ),
               column(6,
                      textInput("new_conf_id", "Create New Conference ID:"),
                      actionButton("create_conf", "Create Conference")
               )
             ),
             uiOutput("user_count_ui"),
             conditionalPanel(
               condition = "output.isAdmin",
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
  
  observe({
    confs <- names(global_data$conferences)
    updateSelectInput(session, "conf_id_select", choices = confs)
    updateSelectInput(session, "conf_id_select_guest", choices = confs)
  })
  
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
  
  observe({
    req(input$conf_id_select_guest)
    conf <- input$conf_id_select_guest
    if (is.null(global_data$user_counts[[conf]])) global_data$user_counts[[conf]] <- list()
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
  
  output$user_count_ui_guest <- renderUI({
    conf <- input$conf_id_select_guest
    if (is.null(global_data$user_counts[[conf]])) return(NULL)
    h5(paste0("Users online for ", conf, ": ", length(global_data$user_counts[[conf]])))
  })
  
  observeEvent(input$create_conf, {
    id <- trimws(input$new_conf_id)
    if (id == "" || id %in% names(global_data$conferences)) {
      showNotification("Invalid input or duplicate ID", type = "error")
      return()
    }
    token <- paste0(sample(c(LETTERS, letters, 0:9), 8, replace = TRUE), collapse = "")
    global_data$conferences[[id]] <- list()
    global_data$settings[[id]] <- list(max_votes = 5, allow_multiple = FALSE)
    global_data$user_counts[[id]] <- list()
    global_data$admin_tokens[[id]] <- token
    global_data$admin_logged_in(TRUE)
    updateTextInput(session, "new_conf_id", value = "")
    updateSelectInput(session, "conf_id_select", selected = id)
    showNotification(paste("Created Conference ID:", id, "Token:", token))
  })
  
  observeEvent(input$login_admin, {
    req(input$conf_id_select, input$admin_token_input)
    token <- global_data$admin_tokens[[input$conf_id_select]]
    if (!is.null(token) && input$admin_token_input == token) {
      global_data$admin_logged_in(TRUE)
      showNotification("Admin login successful")
    } else {
      showNotification("Invalid token", type = "error")
    }
  })
  
  output$isAdmin <- reactive({
    global_data$admin_logged_in()
  })
  outputOptions(output, "isAdmin", suspendWhenHidden = FALSE)
  
  observeEvent(input$add_question, {
    req(input$conf_id_select, input$new_question_text, global_data$admin_logged_in())
    conf <- input$conf_id_select
    question_id <- paste0("q", as.integer(Sys.time()))
    global_data$conferences[[conf]][[question_id]] <- list(
      text = input$new_question_text,
      responses = data.frame(ID = integer(), Answer = character(), Votes = integer(), stringsAsFactors = FALSE)
    )
    updateTextInput(session, "new_question_text", value = "")
    showNotification("Question added.")
  })
  
  observeEvent(input$delete_question, {
    req(input$conf_id_select, input$delete_question_id, global_data$admin_logged_in())
    global_data$conferences[[input$conf_id_select]][[input$delete_question_id]] <- NULL
    showNotification("Question deleted.", type = "warning")
  })
  
  observeEvent(input$restart_question, {
    req(input$conf_id_select, input$restart_question_id, global_data$admin_logged_in())
    conf <- input$conf_id_select
    qid <- input$restart_question_id
    if (!is.null(global_data$conferences[[conf]][[qid]])) {
      global_data$conferences[[conf]][[qid]]$responses <- data.frame(ID = integer(), Answer = character(), Votes = integer(), stringsAsFactors = FALSE)
      showNotification("Responses reset.")
    }
  })
  
  observeEvent(input$apply_settings, {
    req(input$conf_id_select, global_data$admin_logged_in())
    global_data$settings[[input$conf_id_select]] <- list(
      max_votes = input$max_votes,
      allow_multiple = input$allow_multiple
    )
  })
  
  output$question_ui_guest <- renderUI({
    req(input$conf_id_select_guest)
    conf <- input$conf_id_select_guest
    questions <- global_data$conferences[[conf]]
    if (length(questions) == 0) return(h5("No questions yet."))
    selectInput("question_id_guest", "Choose a question:", choices = setNames(names(questions), sapply(questions, `[[`, "text")))
  })
  
  output$question_ui_admin <- renderUI({
    req(input$conf_id_select, global_data$admin_logged_in())
    conf <- input$conf_id_select
    questions <- global_data$conferences[[conf]]
    if (length(questions) == 0) return(h5("No questions available."))
    selectInput("question_id_admin", "Select a question to view:", choices = setNames(names(questions), sapply(questions, `[[`, "text")))
  })
  
  observeEvent(input$submit_answer, {
    req(input$conf_id_select_guest, input$question_id_guest, input$answer)
    conf <- input$conf_id_select_guest
    qid <- input$question_id_guest
    responses <- global_data$conferences[[conf]][[qid]]$responses
    new_id <- if (nrow(responses) == 0) 1 else max(responses$ID) + 1
    new_entry <- data.frame(ID = new_id, Answer = input$answer, Votes = 0, stringsAsFactors = FALSE)
    responses <- rbind(responses, new_entry)
    global_data$conferences[[conf]][[qid]]$responses <- responses
    updateTextInput(session, "answer", value = "")
    updateSelectInput(session, "question_id_guest", selected = qid)
  })
  
  output$vote_table <- renderDT({
    req(input$conf_id_select_guest, input$question_id_guest)
    conf <- input$conf_id_select_guest
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
  
  observeEvent(input$vote_table_rows_selected, {
    req(input$conf_id_select_guest, input$question_id_guest)
    conf <- input$conf_id_select_guest
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
    
    # Preserve the selected question
    updateSelectInput(session, "question_id_guest", selected = qid)
  })
  
  output$admin_table <- renderDT({
    req(input$conf_id_select, input$question_id_admin, global_data$admin_logged_in())
    conf <- input$conf_id_select
    qid <- input$question_id_admin
    responses <- global_data$conferences[[conf]][[qid]]$responses
    if (nrow(responses) == 0) return(NULL)
    datatable(responses[order(-responses$Votes), -1], rownames = FALSE)
  })
}

shinyApp(ui, server)