


library(RSQLite)
library("data.table")
library(shiny)
library(DT) # For interactive and styled tables


# Preprocess the file into a SQLite database
#db <- dbConnect(SQLite(), "data.db")
#lines <- fread("SolutionFiles/SimplexVsSimplex.txt", sep = ":", header = FALSE, col.names = c("key", "value"), quote = "", encoding = "UTF-8")
#dbWriteTable(db, "key_value_pairs", lines, overwrite = TRUE)
#dbDisconnect(db)


library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Search Key-Value Pairs"),
  sidebarLayout(
    sidebarPanel(
      textInput("search_key", "Enter key:", ""),
      actionButton("search_btn", "Search")
    ),
    mainPanel(
      tags$style(HTML("
        .result-box {
          border: 2px solid #4CAF50; /* Green border */
          padding: 15px;
          background-color: #f9f9f9; /* Light grey background */
          border-radius: 10px; /* Rounded corners */
          font-size: 18px;
          font-weight: bold;
          color: #333; /* Dark text */
          text-align: center;
          box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.2); /* Subtle shadow */
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
      tableOutput("player1_table"), # Transposed grid for player 1
      h4("Player 2 Optimal Probabilities"),
      tableOutput("player2_table"), # Transposed grid for player 2
      h4("Expected Value Matrix"),
      tableOutput("ev_matrix_table"), # Table for the EV matrix
      verbatimTextOutput("full_result_output") # Optional full string for debugging
    )
  )
)

# Define Server
server <- function(input, output, session) {
  observeEvent(input$search_btn, {
    search_key <- input$search_key
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
        
        # Identify cards with non-NA probabilities
        player1_cards <- as.integer(which(!is.na(player1_probs))) - 1 # Cards for player 1
        player2_cards <- as.integer(which(!is.na(player2_probs))) - 1 # Cards for player 2
        
        # Create labels with card number and probability percentage
        player1_labels <- paste0(player1_cards, " (", round(player1_probs[player1_cards + 1] * 100, 1), "%)")
        player2_labels <- paste0(player2_cards, " (", round(player2_probs[player2_cards + 1] * 100, 1), "%)")
        
        # Transpose data for display
        player1_table <- data.frame(
          "Card" = player1_cards,
          "Probability" = player1_probs[player1_cards + 1]
        )
        
        player2_table <- data.frame(
          "Card" = player2_cards,
          "Probability" = player2_probs[player2_cards + 1]
        )
        player1_table_t <- t(player1_table)
        player2_table_t <- t(player2_table)
        
        # Parse the EV matrix
        ev_matrix_raw <- unlist(strsplit(parsed_sections[8], " \\$ "))
        ev_matrix_raw <- ev_matrix_raw[seq(3, length(ev_matrix_raw), by = 2)]
        ev_matrix <- do.call(rbind, lapply(ev_matrix_raw, function(row) {
          as.numeric(unlist(strsplit(row, ",")))
        }))
        
        # Set custom row and column labels for the EV matrix
        colnames(ev_matrix) <- player2_labels
        rownames(ev_matrix) <- player1_labels
        
        # Render outputs
        output$game_value_output <- renderText({
          paste("Game Value:", game_value)
        })
        
        output$player1_table <- renderTable({
          as.data.frame(player1_table_t, row.names = c("Card", "Probability"))
        }, rownames = TRUE, colnames = FALSE)
        
        output$player2_table <- renderTable({
          as.data.frame(player2_table_t, row.names = c("Card", "Probability"))
        }, rownames = TRUE, colnames = FALSE)
        
        output$ev_matrix_table <- renderTable({
          ev_matrix
        }, rownames = TRUE)
        
        output$full_result_output <- renderText({
          paste("Full Result String:", result)
        })
      } else {
        output$game_value_output <- renderText({ "No match found." })
        output$player1_table <- renderTable({ NULL })
        output$player2_table <- renderTable({ NULL })
        output$ev_matrix_table <- renderTable({ NULL })
        output$full_result_output <- renderText({ "" })
      }
    } else {
      output$game_value_output <- renderText({ "Please enter a key." })
      output$player1_table <- renderTable({ NULL })
      output$player2_table <- renderTable({ NULL })
      output$ev_matrix_table <- renderTable({ NULL })
      output$full_result_output <- renderText({ "" })
    }
  })
}

# Run the app
shinyApp(ui, server)

