


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

getNextTurnString <- function(current, state_info, P1, P2){
  
  P1_Strength = P1 + 2*state_info$generals[1]
  P2_Strength = P2 + 2*state_info$generals[2]
  print(paste0("P1 Strength:", P1_Strength))
  # Outcome of the round - who won?
  # There are several effects here. In order:
  # If either player used a wizard (5) we simply compare strength.
  
  # Ways p1 could win:
  # Greater strength, (IF: (Either player uses wizard) OR (Neither player uses Musician (0) or Wizard (3)))
  Musician_In_Play = (P1 == 0 | P2 == 0) & (P1 != 5 & P2 != 5)
  P1_Princess_Win = (P1 == 1 & P2 == 7)
  P2_Princess_Win = (P2 == 1 & P1 == 7)
  # We consider assassin's ability to be cancelled if either player uses Prince (7) because prince takes precedent
  Assassin_In_Play = (P1 == 3 | P2 == 3) & (P1 != 5 & P2 != 5) & (P1 != 7 & P2 != 7)
  # Ambassador will be factored in later
  # Wizard already factored into others
  # General affects next turn, not current turn
  # Prince ability factored into Assassin
  
  # All factors to determine who won the round
  Round_Winner_P1 = ifelse(Musician_In_Play, FALSE,
                           ifelse(P1_Princess_Win, TRUE,
                                  ifelse(P2_Princess_Win, FALSE,
                                         ifelse(Assassin_In_Play, P1_Strength < P2_Strength,
                                                P1_Strength > P2_Strength))))
  Round_Winner_P2 = ifelse(Musician_In_Play, FALSE,
                           ifelse(P2_Princess_Win, TRUE,
                                  ifelse(P1_Princess_Win, FALSE,
                                         ifelse(Assassin_In_Play, P2_Strength < P1_Strength,
                                                P2_Strength > P1_Strength))))
  Round_Tied = (!Round_Winner_P1) & (!Round_Winner_P2)
  
  # Now find round outcome
  # Determine how many rounds were on hold at the start of the round.
  
  # First we need to calculate how many wins are added to the hold for each player on each turn
  Rounds_Added_To_Hold_P1 = ifelse(!Round_Tied, -999, #If it's not a tie we'll record as -999
                                   ifelse(P1 == 4 & P2 != 5, 2, 1) # Add two to hold if ambassador was used and tied 
                                   # (Wizard (5) check is redundant but included for consistency)
  )
  
  Rounds_Added_To_Hold_P2 = ifelse(!Round_Tied, -999, #If it's not a tie we'll record as -999
                                   ifelse(P2 == 4 & P1 != 5, 2, 1) # Add two to hold if ambassador was used and tied 
                                   # (Wizard (5) check is redundant but included for consistency)
  )
  
  
  # Write new state
  player1_cards <- paste(state_info$player1_cards[state_info$player1_cards != P1], collapse='') # Remove Player 1's used card
  player2_cards <- paste(state_info$player2_cards[state_info$player2_cards != P2], collapse='') # Remove Player 2's used card
  
  p1_wins = max(min(state_info$wins[1] + ifelse(Round_Winner_P1, 1 + state_info$holds[1] + 3*P1_Princess_Win + 1*(P1 == 4 & P2 != 5), 0),4),0) #Add new wins, truncate to 0-4
  p2_wins = max(min(state_info$wins[2] + ifelse(Round_Winner_P2, 1 + state_info$holds[2] + 3*P2_Princess_Win + 1*(P2 == 4 & P1 != 5), 0),4),0)
  
  # Whether a player has had a spy used against them. They using a spy (2) or wizard (5) cancels this.
  p1_spy = (P1 == 2 & P2 != 2 & P2 != 5)
  p2_spy = (P2 == 2 & P1 != 2 & P1 != 5)
  # Whether a player has successfully used a general the previous term. For simplicity, if both players use general, this effect is cancelled.
  p1_gen = (P1 == 6 & P2 != 6 & P2 != 5)
  p2_gen = (P2 == 6 & P1 != 6 & P1 != 5)
  
  # Adding holds first as they'll be used to add wins
  p1_holds <- max(min(3, state_info$holds[1]+Rounds_Added_To_Hold_P1),0)
  p2_holds <- max(min(3, state_info$holds[2]+Rounds_Added_To_Hold_P2),0)
  
  print(print(paste0("Old p1 cards:", state_info$player1_cards)))
  print(paste0("New p1 cards:", player1_cards))
  
  new_state_str <- sprintf("p1-%s-p2-%s-w-%d%d-g-%d%d-s-%d%d-h-%d%d",
                           player1_cards, player2_cards,
                           p1_wins,p2_wins, p1_gen,p2_gen, p1_spy,p2_spy, p1_holds,p2_holds
                           )
  print(new_state_str)
  #print(state_info)
  return(new_state_str)
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
        # Parse the input's components
        state_sections <- strsplit(search_key, "\\-")[[1]]
        print(state_sections)
        state_info$wins <- as.numeric(unlist(strsplit(state_sections[6], "")))
        state_info$generals <- as.numeric(unlist(strsplit(state_sections[8], "")))
        state_info$spies <- as.numeric(unlist(strsplit(state_sections[10], "")))
        state_info$holds <- as.numeric(unlist(strsplit(state_sections[12], "")))
        
        # Parse the result to extract necessary sections
        parsed_sections <- strsplit(result, "\\|")[[1]]
        game_value <- as.numeric(parsed_sections[2]) # Assuming the first number is in the second section
        
        # Parse probabilities for player 1 and player 2
        player1_probs <- as.numeric(unlist(strsplit(parsed_sections[4], ",")))
        player2_probs <- as.numeric(unlist(strsplit(parsed_sections[6], ",")))
        print(player1_probs)
        print(player2_probs)
        
        # Identify cards with non-NA probabilities
        state_info$player1_cards <- which(!is.na(player1_probs)) - 1 # Cards for player 1
        state_info$player2_cards <- which(!is.na(player2_probs)) - 1 # Cards for player 2

        
        # Create labels with card number and probability percentage
        player1_labels <- paste0(state_info$player1_cards, " (", round(player1_probs[state_info$player1_cards + 1] * 100, 1), "%)")
        player2_labels <- paste0(state_info$player2_cards, " (", round(player2_probs[state_info$player2_cards + 1] * 100, 1), "%)")
        
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
          data.frame(Card = player1_labels, Probability = round(player1_probs[state_info$player1_cards + 1] * 100, 1))
        }, rownames = FALSE)
        
        output$player2_table <- renderTable({
          data.frame(Card = player2_labels, Probability = round(player2_probs[state_info$player2_cards + 1] * 100, 1))
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
    next_turn_string <- getNextTurnString(state_info$search_key, state_info, p1_card, p2_card)
    
    # Update the search input with the new key
    perform_search(next_turn_string)
  })
  
  # Automatically perform the initial search on app startup
  observe({
    isolate({
    print("Starting up, searching for start position")
    perform_search("p1-01234567-p2-01234567-w-00-g-00-s-00-h-00")
    })
  })
}

# Run the app
shinyApp(ui, server)

