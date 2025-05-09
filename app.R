library(shiny)
library(DT)

# Global storage
global_data <- reactiveValues(
  conferences = list(),
  settings = list(),
  user_counts = list(),
  tokens = list()
)

ui <- fluidPage(
  tags$div(
    style = "text-align: center;",
    h1("Polapp"),
    h4("Interactive and real-time polling system"),
    br()
  ),  
  fluidRow(
    column(12,
           wellPanel(
             p("Polapp is a live polling platform designed for conferences, events, workshops, meetings, and similar events. Admins can create unique rooms, design questions, and enable access for attendees via unique tokens. Attendees can suggest answers and vote in real-time. Admins can manage questions, voting settings, and get summary statistics."),
             selectInput("user_role", "What is your role in this poll?", choices = c("Guest", "Admin"), selected = "Guest")
           )
    ),
    br()
  ),
  
  conditionalPanel(
    condition = "input.user_role == 'Guest'",
    tabPanel("Guest Panel",
             conditionalPanel(
               condition = "!output.validGuestConf",
               wellPanel(
                 h5("As a guest, you can suggest answers and vote in real-time within a room."),
                 tags$ul(
                   tags$li("Enter the access token provided by the admin."),
                   tags$li("Once inside, choose a question to view, suggest answers, and vote."),
                   tags$li("You can vote on multiple answers unless limited by the admin.")
                 ),
                 textInput("guest_token_input", "Enter access token:"),
                 actionButton("guest_enter", "Enter conference")
               )
             ),
             uiOutput("user_count_ui_guest"),
             conditionalPanel(
               condition = "output.validGuestConf",
               uiOutput("question_ui_guest"),
               uiOutput("guest_question_controls")
             )
    )
  ),
  
  conditionalPanel(
    condition = "input.user_role == 'Admin'",
    tabPanel("Admin Panel",
             conditionalPanel(
               condition = "!output.isAdmin",
               wellPanel(
                 h5("Create a new room to get started."),
                 tags$ul(
                   tags$li("Each room generates a unique access token to share with participants."),
                   tags$li("Add, delete, or reset questions and control voting settings."),
                   tags$li("View results from your voting room.")
                 ),
                 textInput("new_conf_id", "Conference ID:"),
                 actionButton("create_conf", "Create conference")
               )
             ),
             uiOutput("admin_token_display"),
             uiOutput("user_count_ui"),
             conditionalPanel(
               condition = "output.isAdmin",
               fluidRow(
                 column(6,
                        wellPanel(
                          h4("Question management"),
                          textInput("new_question_text", "New question text:"),
                          actionButton("add_question", "Add Question"),
                          selectInput("delete_question_id", "Delete question:", choices = NULL),
                          actionButton("delete_question", "Delete"),
                          selectInput("restart_question_id", "Restart question responses:", choices = NULL),
                          actionButton("restart_question", "Restart")
                        )
                 ),
                 column(6,
                        wellPanel(
                          h4("General settings"),
                          numericInput("max_votes", "Max votes per user:", value = 5, min = 1),
                          checkboxInput("allow_multiple", "Allow multiple votes per option", value = TRUE),
                          actionButton("apply_settings", "Apply Settings")
                        )
                 )
               ),
               wellPanel(
                 h4("Live results"),
                 uiOutput("question_ui_admin"),
                 DTOutput("admin_table")
               )
             )
    )
  ),
  
  fluidRow(
    column(12,
           wellPanel(
             tags$div(
               style = "text-align: center;",
               p("Polapp was designed by ",
                 a("Dr. Cristian Román-Palacios (Data Diversity Lab, U of A)", href = "https://datadiversitylab.github.io/", target = "_blank"),
                 " • ", 
                 a("GitHub Repo", href = "https://github.com/datadiversitylab/polapp", target = "_blank")
               ),
               p("Initial concept by ", 
                 a("Dr. Heidi Steiner", href = "https://heidiesteiner.netlify.app/", target = "_blank")
                 )
               ,
               tags$small("Note: No data is stored permanently. All session data is lost when the admin session ends.")
             )
           )
    )
  )
)


server <- function(input, output, session) {
  session_id <- paste0("user_", as.integer(Sys.time()), "_", sample(10000, 1))
  
  user_votes <- reactiveVal(list())
  vote_message <- reactiveVal("")
  is_admin <- reactiveVal(FALSE)
  admin_conference_id <- reactiveVal(NULL)
  guest_conference_id <- reactiveVal(NULL)
  guest_selected_question <- reactiveVal(NULL)
  guest_typed_answer <- reactiveVal("") 
  
  output$isAdmin <- reactive({ is_admin() })
  outputOptions(output, "isAdmin", suspendWhenHidden = FALSE)
  
  output$validGuestConf <- reactive({ !is.null(guest_conference_id()) })
  outputOptions(output, "validGuestConf", suspendWhenHidden = FALSE)
  
  observeEvent(input$create_conf, {
    id <- trimws(input$new_conf_id)
    if (id == "" || id %in% names(global_data$conferences)) {
      showNotification("Invalid or duplicate ID", type = "error")
      return()
    }
    token <- paste0(sample(c(LETTERS, letters, 0:9), 4, replace = TRUE), collapse = "")
    global_data$conferences[[id]] <- list()
    global_data$settings[[id]] <- list(max_votes = 5, allow_multiple = FALSE)
    global_data$user_counts[[id]] <- list()
    global_data$tokens[[token]] <- id
    is_admin(TRUE)
    admin_conference_id(id)
    updateTextInput(session, "new_conf_id", value = "")
    showNotification(paste("Created Conference ID:", id))
  })
  
  output$admin_token_display <- renderUI({
    req(admin_conference_id())
    id <- admin_conference_id()
    token <- names(global_data$tokens)[sapply(global_data$tokens, function(x) x == id)]
    wellPanel(
      h5("Access token (share with users):"),
      verbatimTextOutput("admin_token_text")
    )
  })
  
  output$admin_token_text <- renderText({
    id <- admin_conference_id()
    names(global_data$tokens)[sapply(global_data$tokens, function(x) x == id)]
  })
  
  observeEvent(input$guest_enter, {
    token <- trimws(input$guest_token_input)
    if (token %in% names(global_data$tokens)) {
      guest_conference_id(global_data$tokens[[token]])
    } else {
      showNotification("Invalid token", type = "error")
    }
  })
  
  observe({
    req(admin_conference_id())
    conf <- admin_conference_id()
    questions <- global_data$conferences[[conf]]
    if (length(questions) == 0) return()
    question_ids <- names(questions)
    question_texts <- sapply(questions, function(q) q$text)
    names(question_ids) <- question_texts
    updateSelectInput(session, "delete_question_id", choices = question_ids)
    updateSelectInput(session, "restart_question_id", choices = question_ids)
  })
  
  observe({
    conf <- guest_conference_id()
    req(conf)
    if (is.null(global_data$user_counts[[conf]])) global_data$user_counts[[conf]] <- list()
    global_data$user_counts[[conf]][[session_id]] <- TRUE
    session$onSessionEnded(function() {
      global_data$user_counts[[conf]][[session_id]] <- NULL
    })
  })
  
  output$user_count_ui <- renderUI({
    conf <- admin_conference_id()
    if (is.null(conf)) return(NULL)
    users <- length(global_data$user_counts[[conf]])
    h5(paste("Users online in", conf, ":", users))
  })
  
  output$user_count_ui_guest <- renderUI({
    conf <- guest_conference_id()
    if (is.null(conf)) return(NULL)
    users <- length(global_data$user_counts[[conf]])
    h5(paste("Users online in", conf, ":", users))
  })
  
  observeEvent(input$add_question, {
    req(admin_conference_id(), input$new_question_text)
    conf <- admin_conference_id()
    question_id <- paste0("q", as.integer(Sys.time()))
    global_data$conferences[[conf]][[question_id]] <- list(
      text = input$new_question_text,
      responses = data.frame(ID = integer(), Answer = character(), Votes = integer(), stringsAsFactors = FALSE)
    )
    updateTextInput(session, "new_question_text", value = "")
    showNotification("Question added.")
  })
  
  observeEvent(input$delete_question, {
    req(admin_conference_id(), input$delete_question_id)
    global_data$conferences[[admin_conference_id()]][[input$delete_question_id]] <- NULL
    showNotification("Question deleted.", type = "warning")
  })
  
  observeEvent(input$restart_question, {
    req(admin_conference_id(), input$restart_question_id)
    conf <- admin_conference_id()
    qid <- input$restart_question_id
    global_data$conferences[[conf]][[qid]]$responses <- data.frame(ID = integer(), Answer = character(), Votes = integer(), stringsAsFactors = FALSE)
    showNotification("Responses reset.")
  })
  
  observeEvent(input$apply_settings, {
    req(admin_conference_id())
    global_data$settings[[admin_conference_id()]] <- list(
      max_votes = input$max_votes,
      allow_multiple = input$allow_multiple
    )
  })
  
  output$question_ui_guest <- renderUI({
    conf <- guest_conference_id()
    if (is.null(conf) || !(conf %in% names(global_data$conferences))) return(NULL)
    questions <- global_data$conferences[[conf]]
    if (length(questions) == 0) return(h5("Please request the admin to create a new question."))
    selectInput("question_id_guest", "Choose a question:",
                choices = setNames(names(questions), sapply(questions, `[[`, "text")),
                selected = guest_selected_question())
  })
  
  observeEvent(input$question_id_guest, {
    guest_selected_question(input$question_id_guest)
  })
  
  observeEvent(input$answer, {
    guest_typed_answer(input$answer)
  })
  
  output$question_ui_admin <- renderUI({
    conf <- admin_conference_id()
    questions <- global_data$conferences[[conf]]
    if (length(questions) == 0) return(h5("Please request the admin to create a new question."))
    selectInput("question_id_admin", "Select a question to view:",
                choices = setNames(names(questions), sapply(questions, `[[`, "text")))
  })
  
  output$guest_question_controls <- renderUI({
    conf <- guest_conference_id()
    if (is.null(conf) || !(conf %in% names(global_data$conferences))) return(NULL)
    questions <- global_data$conferences[[conf]]
    if (length(questions) == 0) return(NULL)
    sidebarLayout(
      sidebarPanel(
        textInput("answer", "Suggest an answer:", value = guest_typed_answer()),
        actionButton("submit_answer", "Submit")
      ),
      mainPanel(
        h4("Vote for an answer:"),
        DTOutput("vote_table"),
        textOutput("vote_warning")
      )
    )
  })
  
  observeEvent(input$submit_answer, {
    conf <- guest_conference_id()
    qid <- guest_selected_question()
    req(conf, qid, input$answer)
    responses <- global_data$conferences[[conf]][[qid]]$responses
    new_id <- if (nrow(responses) == 0) 1 else max(responses$ID) + 1
    new_entry <- data.frame(ID = new_id, Answer = input$answer, Votes = 0, stringsAsFactors = FALSE)
    responses <- rbind(responses, new_entry)
    global_data$conferences[[conf]][[qid]]$responses <- responses
    guest_typed_answer("")
  })
  
  output$vote_table <- renderDT({
    conf <- guest_conference_id()
    qid <- guest_selected_question()
    req(conf, qid)
    responses <- global_data$conferences[[conf]][[qid]]$responses
    if (nrow(responses) == 0) return(NULL)
    votes_by_user <- user_votes()[[qid]]
    if (is.null(votes_by_user)) votes_by_user <- integer()
    responses$YourVotes <- votes_by_user[as.character(responses$ID)]
    responses$YourVotes[is.na(responses$YourVotes)] <- 0
    datatable(responses[, c("Answer", "YourVotes")], selection = "single", rownames = FALSE, options = list(dom = 't'))
  })
  
  output$vote_warning <- renderText({ vote_message() })
  
  observeEvent(input$vote_table_rows_selected, {
    conf <- guest_conference_id()
    qid <- guest_selected_question()
    selected <- input$vote_table_rows_selected
    req(conf, qid, length(selected) > 0)
    settings <- global_data$settings[[conf]]
    responses <- global_data$conferences[[conf]][[qid]]$responses
    votes_all <- user_votes()
    votes <- votes_all[[qid]]
    if (is.null(votes)) votes <- integer()
    id_char <- as.character(responses$ID[selected])
    total_votes <- sum(votes, na.rm = TRUE)
    current_vote <- votes[id_char]
    if (is.na(current_vote)) current_vote <- 0
    max_votes <- settings$max_votes
    allow_multiple <- settings$allow_multiple
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
  
  output$admin_table <- renderDT({
    conf <- admin_conference_id()
    qid <- input$question_id_admin
    req(conf, qid)
    responses <- global_data$conferences[[conf]][[qid]]$responses
    if (nrow(responses) == 0) return(NULL)
    datatable(responses[order(-responses$Votes), -1], rownames = FALSE)
  })
}

shinyApp(ui, server)
