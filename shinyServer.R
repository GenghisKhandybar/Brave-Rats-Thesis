


#library(RSQLite)
library("data.table")
library(shiny)
library(shinyjs)
library(DT) # For interactive and styled tables


# Preprocess the file into a SQLite database
#db <- dbConnect(SQLite(), "data.db")
#lines <- fread("SolutionFiles/SimplexVsSimplex.txt", sep = ":", header = FALSE, col.names = c("key", "value"), quote = "", encoding = "UTF-8")
#dbWriteTable(db, "key_value_pairs", lines, overwrite = TRUE)
#dbDisconnect(db)


library(shiny)

getNextTurnString <- function(current, p1_card, p2_card){
  return("p1-01235-p2-01236-w-20-g-00-s-00-h-22")
  #return(c(current, p1_card, p2_card))
}

# Define UI
ui <- fluidPage(
  titlePanel("Search Key-Value Pairs"),
  
  # Big green "Starting Position" button
  div(
    style = "text-align: center; margin-bottom: 15px;",
    actionButton(
      "start_pos_btn",
      "New Game",
      style = "font-size: 24px; padding: 15px 30px; background-color: green; color: white; border: none; border-radius: 10px; cursor: pointer;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      textInput("search_key", "Enter key:", "p1-01234567-p2-01234567-w-00-g-00-s-00-h-00"),
      actionButton("search_btn", "Search")
    ),
    mainPanel(
      tags$style(HTML("
        .result-box {
          border: 2px solid #4CAF50;
          padding: 15px;
          background-color: #f9f9f9;
          border-radius: 10px;
          font-size: 18px;
          font-weight: bold;
          color: #333;
          text-align: center;
          box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.2);
        }
        table {
          width: 100%;
          border-collapse: collapse;
        }
        th, td {
          border: 1px solid #ddd;
          padding: 8px;
          text-align: center;
        }
        th {
          background-color: #f2f2f2;
        }
      ")),
      h3("Result:"),
      div(id = "game_value_box", class = "result-box", textOutput("game_value_output")),
      h4("Player 1 Optimal Probabilities"),
      tableOutput("player1_table"),
      h4("Player 2 Optimal Probabilities"),
      tableOutput("player2_table"),
      h4("Expected Value Matrix"),
      tableOutput("ev_matrix_table"),
      verbatimTextOutput("full_result_output")
    )
  )
)


# Define Server
server <- function(input, output, session) {
  state_info <- reactiveValues()
  perform_search <- function(search_key) {
    print(paste0("Searching for ", search_key))
    
    search_key <- search_key
    state_info$search_key <- search_key
    chunk_size <- 10000 # Number of lines to process at once
    
    if (nchar(search_key) > 0) {
      result <- NULL
      file_conn <- file("SolutionFiles/SimplexVsSimplex.txt", "r")
      
      repeat {
        chunk <- readLines(file_conn, n = chunk_size, warn = FALSE)
        if (length(chunk) == 0) break
        
        # Split lines and check for the key
        for (line in chunk) {
          parts <- strsplit(line, ":")[[1]]
          if (parts[1] == search_key) {
            result <- parts[2]
            break
          }
        }
        
        if (!is.null(result)) break
      }
      
      close(file_conn)
      
      if (!is.null(result)) {
        # Parse the result to extract necessary sections
        parsed_sections <- strsplit(result, "\\|")[[1]]
        game_value <- as.numeric(parsed_sections[2]) # Assuming the first number is in the second section
        
        # Parse probabilities for player 1 and player 2
        player1_probs <- as.numeric(unlist(strsplit(parsed_sections[4], ",")))
        player2_probs <- as.numeric(unlist(strsplit(parsed_sections[6], ",")))
        print(player1_probs)
        print(player2_probs)
        
        # Identify cards with non-NA probabilities
        player1_cards <- which(!is.na(player1_probs)) - 1 # Cards for player 1
        player2_cards <- which(!is.na(player2_probs)) - 1 # Cards for player 2
        
        state_info$player1_cards <- player1_cards
        state_info$player2_cards <- player2_cards
        
        # Create labels with card number and probability percentage
        player1_labels <- paste0(player1_cards, " (", round(player1_probs[player1_cards + 1] * 100, 1), "%)")
        player2_labels <- paste0(player2_cards, " (", round(player2_probs[player2_cards + 1] * 100, 1), "%)")
        
        # Parse the EV matrix
        ev_matrix_raw <- unlist(strsplit(parsed_sections[8], " \\$ "))
        ev_matrix_raw <- ev_matrix_raw[seq(3, length(ev_matrix_raw), by = 2)]
        ev_matrix <- do.call(rbind, lapply(ev_matrix_raw, function(row) {
          as.numeric(unlist(strsplit(row, ",")))
        }))
        
        # Set custom row and column labels for the EV matrix
        colnames(ev_matrix) <- player2_labels
        rownames(ev_matrix) <- player1_labels
        
        # Render the clickable EV matrix table
        output$ev_matrix_table <- renderUI({
          # Create the header row
          header_row <- tags$tr(
            tags$th(""), # Empty top-left corner
            lapply(player2_labels, function(label) tags$th(label))
          )
          
          # Create the rows with clickable buttons
          table_rows <- lapply(seq_along(player1_labels), function(i) {
            tags$tr(
              tags$th(player1_labels[i]), # Row label (Player 1's cards)
              lapply(seq_along(player2_labels), function(j) {
                tags$td(
                  actionButton(
                    inputId = paste0(search_key,"_cell_", i, "_", j),
                    label = round(ev_matrix[i, j], 2),
                    class = "action-button small-btn",
                    onclick = sprintf(
                      "Shiny.setInputValue('cell_click', {row: %d, col: %d}, {priority: 'event'})",
                      i, j
                    )
                  )
                )
              })
            )
          })
          
          # Combine the header row and table rows
          tags$table(
            class = "table table-striped table-hover",
            header_row,
            do.call(tags$tbody, table_rows)
          )
        })
        
        
        
        # Other outputs
        output$game_value_output <- renderText({
          paste("Game Value:", game_value)
        })
        
        output$player1_table <- renderTable({
          data.frame(Card = player1_labels, Probability = round(player1_probs[player1_cards + 1] * 100, 1))
        }, rownames = FALSE)
        
        output$player2_table <- renderTable({
          data.frame(Card = player2_labels, Probability = round(player2_probs[player2_cards + 1] * 100, 1))
        }, rownames = FALSE)
        
        output$full_result_output <- renderText({
          paste("Full Result String:", result)
        })
      } else {
        # Handle no results found
        output$game_value_output <- renderText({ "No match found." })
        output$player1_table <- renderTable({ NULL })
        output$player2_table <- renderTable({ NULL })
        output$ev_matrix_table <- renderUI({ NULL })
        output$full_result_output <- renderText({ "" })
      }
    } else {
      # Handle empty search key
      output$game_value_output <- renderText({ "Please enter a key." })
      output$player1_table <- renderTable({ NULL })
      output$player2_table <- renderTable({ NULL })
      output$ev_matrix_table <- renderUI({ NULL })
      output$full_result_output <- renderText({ "" })
    }
  }

  # Search when search button is clicked
  observeEvent(input$search_btn, {
    print("Using search button")
    perform_search(input$search_key)
  })
  
  # Search when "Starting Position" button is clicked
  observeEvent(input$start_pos_btn, {
    print("Start button pressed")
    #updateTextInput(session, "search_key", value = "p1-01234567-p2-01234567-w-00-g-00-s-00-h-00")
    perform_search("p1-01234567-p2-01234567-w-00-g-00-s-00-h-00")
  })
  
  # An observer for when a cell is clicked
  observeEvent(input$click, {
    print("Cell click")
    perform_search(input$cell_press)
  })
  
  # Observe cell clicks and call getNextTurnString
  observeEvent(input$cell_click, {
    row_index <- input$cell_click$row
    col_index <- input$cell_click$col
    
    # Extract card details from labels
    p1_card <- state_info$player1_cards[row_index]
    p2_card <- state_info$player2_cards[col_index]
    print(paste("Press:", p1_card, p2_card))
    # Call the function (implement getNextTurnString as needed)
    next_turn_string <- getNextTurnString(state_info$search_key, p1_card, p2_card)
    
    # Update the search input with the new key
    
    #updateTextInput(session, "search_key", value = next_turn_string)
    perform_search(next_turn_string)
  })
  
  # Automatically perform the initial search on app startup
  #observe({
  #  print("Starting up, searching for start position")
  #  updateTextInput(session, "search_key", value = "p1-01234567-p2-01234567-w-00-g-00-s-00-h-00")
  #  perform_search("p1-01234567-p2-01234567-w-00-g-00-s-00-h-00")
  #})
}

# Run the app
shinyApp(ui, server)

