
# Read in data, remove columns we're not interested in.
braverats <- read.csv("BraveRatsData.csv")

player_names <- braverats %>% 
  select(Game, Player.1, Player.2) %>% 
  rename(P1_name = Player.1, P2_name = Player.2)

braverats <- braverats %>% 
  select(-Player.1, -Player.2, -Outcome, -Notes, -Date) %>% 
  filter(!is.na(Winner))



# Turn the data to long form.

# We want every column "PX.Card.X" to become a new row to begin our analysis 

braverats_by_turn <- braverats %>% 
  pivot_longer(
    # So, we select all columns that start with "P"
    cols = starts_with("P"),  
    # There are two pieces of information we want from each column we're elongating: Player # and Turn #
    # So, we write a regular expression that describes those columns, putting parentheses around the two parts we want to extract.
    names_pattern = "P([1-2]).Card.([1-8])", 
    # We specify that the first group contains "Player", and the second contains the current turn.
    names_to = c("Player", "Turn"),
    # Lastly, we state that the value of each of the original "PX.Card.X" columns is "Card".
    values_to = "Card")

# Next, we want to widen this to just have one row per turn of each game. That row should contain both players' cards.

braverats_by_turn <- braverats_by_turn %>% 
  pivot_wider(id_cols = c("Game", "Winner", "Turn"),
              names_from = "Player",
              names_prefix = "P",
              values_from = Card) %>% 
  drop_na()

# Lastly, we will add in the special effects from the cards and evaluate basic events on each turn such as who won each throw.

braverats_by_turn <- braverats_by_turn %>% 
  group_by(Game) %>% 
  mutate(
    # Creates new columns for previous move, for simplicity. On the first turn these are set to -1.
    P1_Prev = lag(P1, default = -1),
    P2_Prev = lag(P2, default = -1),
    # Whether a player has had a spy used against them. They using a spy (2) or wizard (5) cancels this.
    P1_Spied = (P2_Prev == 2 & P1_Prev != 2 & P1_Prev != 5),
    P2_Spied = (P1_Prev == 2 & P2_Prev != 2 & P2_Prev != 5),
    # Whether a player has successfully used a general the previous term. For simplicity, if both players use general, this effect is cancelled.
    P1_Plus_Two = (P1_Prev == 6 & P2_Prev != 6 & P2_Prev != 5),
    P2_Plus_Two = (P2_Prev == 6 & P1_Prev != 6 & P1_Prev != 5),
    # Each player's strength after accounting for General buffs
    P1_Strength = P1 + 2*P1_Plus_Two,
    P2_Strength = P2 + 2*P2_Plus_Two,
    # Outcome of the round - who won?
    # There are several effects here. In order:
    # If either player used a wizard (5) we simply compare strength.
    
    # Ways p1 could win:
    # Greater strength, (IF: (Either player uses wizard) OR (Neither player uses Musician (0) or Wizard (3)))
    Musician_In_Play = (P1 == 0 | P2 == 0) & (P1 != 5 & P2 != 5),
    P1_Princess_Win = (P1 == 1 & P2 == 7),
    P2_Princess_Win = (P2 == 1 & P1 == 7),
    # We consider assassin's ability to be cancelled if either player uses Prince (7) because prince takes precedent
    Assassin_In_Play = xor(P1 == 3, P2 == 3) & (P1 != 5 & P2 != 5) & (P1 != 7 & P2 != 7),
    # Ambassador will be factored in later
    # Wizard already factored into others
    # General affects next turn, not current turn
    # Prince ability factored into Assassin
    
    # All factors to determine who won the round
    Round_Winner_P1 = ifelse(Musician_In_Play, FALSE,
                             ifelse(P1_Princess_Win, TRUE,
                                    ifelse(P2_Princess_Win, FALSE,
                                           ifelse(Assassin_In_Play, P1_Strength < P2_Strength,
                                                  P1_Strength > P2_Strength)))),
    Round_Winner_P2 = ifelse(Musician_In_Play, FALSE,
                             ifelse(P2_Princess_Win, TRUE,
                                    ifelse(P1_Princess_Win, FALSE,
                                           ifelse(Assassin_In_Play, P2_Strength < P1_Strength,
                                                  P2_Strength > P1_Strength)))),
    Round_Tied = (!Round_Winner_P1) & (!Round_Winner_P2),
    
    # Now find round outcome
    # Determine how many rounds were on hold at the start of the round.
    
    # First we need to calculate how many wins are added to the hold for each player on each turn
    Rounds_Added_To_Hold_P1 = ifelse(!Round_Tied, -999, #If it's not a tie we'll record as -999
                                     ifelse(P1 == 4 & P2 != 5, 2, 1) # Add two to hold if ambassador was used and tied 
                                     # (Wizard (5) check is redundant but included for consistency)
    ),
    
    Rounds_Added_To_Hold_P2 = ifelse(!Round_Tied, -999, #If it's not a tie we'll record as -999
                                     ifelse(P2 == 4 & P1 != 5, 2, 1) # Add two to hold if ambassador was used and tied 
                                     # (Wizard (5) check is redundant but included for consistency)
    ),
    
    # In order to calculate the number of held rounds we need to re-group based on when rounds tie.
    # This will let our "holds" counters reset everytime either player wins a round
    
    # Every time there's a the last round was not tied, we go to the next hold group. This represents a different sequence of tied plays.
    # Summing booleans like this is a good way to count how many of a particular event has occurred.
    Hold_Group = cumsum(!lag(Round_Tied, default = FALSE))
  ) %>% 
  ungroup() %>% 
  # Now we group by Game (as before) and Hold Group so that we can sum the held rounds on each turn.
  # If round is tied, we add up all the other holds from the group. Otherwise, 0 holds at end of turn ("EOT" = End Of Turn)
  group_by(Game, Hold_Group) %>% 
  mutate(Holds_EOT_P1 = ifelse(Round_Tied, cumsum(Rounds_Added_To_Hold_P1), 0),
         Holds_EOT_P2 = ifelse(Round_Tied, cumsum(Rounds_Added_To_Hold_P2), 0)) %>% 
  ungroup() %>% 
  group_by(Game) %>% 
  mutate(
    # Holds variable is for holds at start of turn
    Holds_P1 = lag(Holds_EOT_P1, default = 0),
    Holds_P2 = lag(Holds_EOT_P2, default = 0),
    Round_Wins_P1 = ifelse(Round_Winner_P1, 1 + Holds_P1 + 3*P1_Princess_Win + 1*(P1 == 4 & P2 != 5), 0),
    Round_Wins_P2 = ifelse(Round_Winner_P2, 1 + Holds_P2 + 3*P2_Princess_Win + 1*(P2 == 4 & P2 != 5), 0),
    Total_Wins_EOT_P1 = cumsum(Round_Wins_P1),
    Total_Wins_EOT_P2 = cumsum(Round_Wins_P2),
    Total_Wins_P1 = lag(Total_Wins_EOT_P1, default = 0),
    Total_Wins_P2 = lag(Total_Wins_EOT_P2, default = 0)
  ) %>% 
  select(-Hold_Group, -Rounds_Added_To_Hold_P1, -Rounds_Added_To_Hold_P2, 
         -Round_Tied, -Holds_EOT_P1, -Holds_EOT_P2, -Assassin_In_Play, 
         -P1_Princess_Win, -P2_Princess_Win, -Musician_In_Play) %>% 
  ungroup()


P1_Hand <- braverats_by_turn %>% 
  mutate(temp_1s = 1) %>% 
  # Create column for each of P1's cards
  pivot_wider(names_from = P1, values_from = temp_1s, values_fill = 0, names_prefix = "P1_C") %>% 
  # Transform each of the P1 Card columns into an indicator of whether P1 has that card at the start of that turn. 
  # Formula: 1 - the cumsum of the column's previous values.
  group_by(Game) %>% 
  mutate(across(starts_with("P1_C"), ~ 1 - lag(cumsum(.), default = 0))) %>% 
  ungroup() %>% 
  select(starts_with("P1_C"))

P2_Hand <- braverats_by_turn %>% 
  mutate(temp_1s = 1) %>% 
  # Create column for each of P2's cards
  pivot_wider(names_from = P2, values_from = temp_1s, values_fill = 0, names_prefix = "P2_C") %>%
  # Transform each of the P2 Card columns into an indicator of whether P2 has that card at the start of that turn. 
  # Formula: 1 - the cumsum of the column's previous values.
  group_by(Game) %>% 
  mutate(across(starts_with("P2_C"), ~ 1 - lag(cumsum(.), default = 0))) %>% 
  ungroup() %>% 
  select(starts_with("P2_C"))

# Add the current hand columns to braverats_by_turn

braverats_by_turn <- braverats_by_turn %>% 
  cbind(P1_Hand) %>% 
  cbind(P2_Hand)

# Function to urn the data to long form.

# We want every column "PX.Card.X" to become a new row to begin our analysis 
compute_results <- function(braverats) {
  braverats_by_turn <- braverats %>% 
    pivot_longer(
      # So, we select all columns that start with "P"
      cols = starts_with("P"),  
      # There are two pieces of information we want from each column we're elongating: Player # and Turn #
      # So, we write a regular expression that describes those columns, putting parentheses around the two parts we want to extract.
      names_pattern = "P([1-2]).Card.([1-8])", 
      # We specify that the first group contains "Player", and the second contains the current turn.
      names_to = c("Player", "Turn"),
      # Lastly, we state that the value of each of the original "PX.Card.X" columns is "Card".
      values_to = "Card")
  
  # Next, we want to widen this to just have one row per turn of each game. That row should contain both players' cards.
  
  braverats_by_turn <- braverats_by_turn %>% 
    pivot_wider(id_cols = c("Game", "Winner", "Turn"),
                names_from = "Player",
                names_prefix = "P",
                values_from = Card) %>% 
    drop_na()
  
  # Lastly, we will add in the special effects from the cards and evaluate basic events on each turn such as who won each throw.
  
  braverats_by_turn <- braverats_by_turn %>% 
    group_by(Game) %>% 
    mutate(
      # Creates new columns for previous move, for simplicity. On the first turn these are set to -1.
      P1_Prev = lag(P1, default = -1),
      P2_Prev = lag(P2, default = -1),
      # Whether a player has had a spy used against them. They using a spy (2) or wizard (5) cancels this.
      P1_Spied = (P2_Prev == 2 & P1_Prev != 2 & P1_Prev != 5),
      P2_Spied = (P1_Prev == 2 & P2_Prev != 2 & P2_Prev != 5),
      # Whether a player has successfully used a general the previous term. For simplicity, if both players use general, this effect is cancelled.
      P1_Plus_Two = (P1_Prev == 6 & P2_Prev != 6 & P2_Prev != 5),
      P2_Plus_Two = (P2_Prev == 6 & P1_Prev != 6 & P1_Prev != 5),
      # Each player's strength after accounting for General buffs
      P1_Strength = P1 + 2*P1_Plus_Two,
      P2_Strength = P2 + 2*P2_Plus_Two,
      # Outcome of the round - who won?
      # There are several effects here. In order:
      # If either player used a wizard (5) we simply compare strength.
      
      # Ways p1 could win:
      # Greater strength, (IF: (Either player uses wizard) OR (Neither player uses Musician (0) or Wizard (3)))
      Musician_In_Play = (P1 == 0 | P2 == 0) & (P1 != 5 & P2 != 5),
      P1_Princess_Win = (P1 == 1 & P2 == 7),
      P2_Princess_Win = (P2 == 1 & P1 == 7),
      # We consider assassin's ability to be cancelled if either player uses Prince (7) because prince takes precedent
      Assassin_In_Play = xor(P1 == 3, P2 == 3) & (P1 != 5 & P2 != 5) & (P1 != 7 & P2 != 7),
      # Ambassador will be factored in later
      # Wizard already factored into others
      # General affects next turn, not current turn
      # Prince ability factored into Assassin
      
      # All factors to determine who won the round
      Round_Winner_P1 = ifelse(Musician_In_Play, FALSE,
                               ifelse(P1_Princess_Win, TRUE,
                                      ifelse(P2_Princess_Win, FALSE,
                                             ifelse(Assassin_In_Play, P1_Strength < P2_Strength,
                                                    P1_Strength > P2_Strength)))),
      Round_Winner_P2 = ifelse(Musician_In_Play, FALSE,
                               ifelse(P2_Princess_Win, TRUE,
                                      ifelse(P1_Princess_Win, FALSE,
                                             ifelse(Assassin_In_Play, P2_Strength < P1_Strength,
                                                    P2_Strength > P1_Strength)))),
      Round_Tied = (!Round_Winner_P1) & (!Round_Winner_P2),
      
      # Now find round outcome
      # Determine how many rounds were on hold at the start of the round.
      
      # First we need to calculate how many wins are added to the hold for each player on each turn
      Rounds_Added_To_Hold_P1 = ifelse(!Round_Tied, -999, #If it's not a tie we'll record as -999
                                       ifelse(P1 == 4 & P2 != 5, 2, 1) # Add two to hold if ambassador was used and tied 
                                       # (Wizard (5) check is redundant but included for consistency)
      ),
      
      Rounds_Added_To_Hold_P2 = ifelse(!Round_Tied, -999, #If it's not a tie we'll record as -999
                                       ifelse(P2 == 4 & P1 != 5, 2, 1) # Add two to hold if ambassador was used and tied 
                                       # (Wizard (5) check is redundant but included for consistency)
      ),
      
      # In order to calculate the number of held rounds we need to re-group based on when rounds tie.
      # This will let our "holds" counters reset everytime either player wins a round
      
      # Every time there's a the last round was not tied, we go to the next hold group. This represents a different sequence of tied plays.
      # Summing booleans like this is a good way to count how many of a particular event has occurred.
      Hold_Group = cumsum(!lag(Round_Tied, default = FALSE))
    ) %>% 
    ungroup() %>% 
    # Now we group by Game (as before) and Hold Group so that we can sum the held rounds on each turn.
    # If round is tied, we add up all the other holds from the group. Otherwise, 0 holds at end of turn ("EOT" = End Of Turn)
    group_by(Game, Hold_Group) %>% 
    mutate(Holds_EOT_P1 = ifelse(Round_Tied, cumsum(Rounds_Added_To_Hold_P1), 0),
           Holds_EOT_P2 = ifelse(Round_Tied, cumsum(Rounds_Added_To_Hold_P2), 0)) %>% 
    ungroup() %>% 
    group_by(Game) %>% 
    mutate(
      # Holds variable is for holds at start of turn
      Holds_P1 = lag(Holds_EOT_P1, default = 0),
      Holds_P2 = lag(Holds_EOT_P2, default = 0),
      Round_Wins_P1 = ifelse(Round_Winner_P1, 1 + Holds_P1 + 3*P1_Princess_Win + 1*(P1 == 4 & P2 != 5), 0),
      Round_Wins_P2 = ifelse(Round_Winner_P2, 1 + Holds_P2 + 3*P2_Princess_Win + 1*(P2 == 4 & P2 != 5), 0),
      Total_Wins_EOT_P1 = cumsum(Round_Wins_P1),
      Total_Wins_EOT_P2 = cumsum(Round_Wins_P2),
      Total_Wins_P1 = lag(Total_Wins_EOT_P1, default = 0),
      Total_Wins_P2 = lag(Total_Wins_EOT_P2, default = 0)
    ) %>% 
    select(-Hold_Group, -Rounds_Added_To_Hold_P1, -Rounds_Added_To_Hold_P2, 
           -Round_Tied, -Holds_EOT_P1, -Holds_EOT_P2, -Assassin_In_Play, 
           -P1_Princess_Win, -P2_Princess_Win, -Musician_In_Play) %>% 
    ungroup()
  
  # Make hand variables
  
  P1_Hand <- braverats_by_turn %>% 
    mutate(temp_1s = 1) %>% 
    # Create column for each of P1's cards
    pivot_wider(names_from = P1, values_from = temp_1s, values_fill = 0, names_prefix = "P1_C") %>% 
    # Transform each of the P1 Card columns into an indicator of whether P1 has that card at the start of that turn. 
    # Formula: 1 - the cumsum of the column's previous values.
    group_by(Game) %>% 
    mutate(across(starts_with("P1_C"), ~ 1 - lag(cumsum(.), default = 0))) %>% 
    ungroup() %>% 
    select(starts_with("P1_C"))
  
  P2_Hand <- braverats_by_turn %>% 
    mutate(temp_1s = 1) %>% 
    # Create column for each of P2's cards
    pivot_wider(names_from = P2, values_from = temp_1s, values_fill = 0, names_prefix = "P2_C") %>%
    # Transform each of the P2 Card columns into an indicator of whether P2 has that card at the start of that turn. 
    # Formula: 1 - the cumsum of the column's previous values.
    group_by(Game) %>% 
    mutate(across(starts_with("P2_C"), ~ 1 - lag(cumsum(.), default = 0))) %>% 
    ungroup() %>% 
    select(starts_with("P2_C"))
  
  # Add the current hand columns to braverats_by_turn
  
  braverats_by_turn <- braverats_by_turn %>% 
    cbind(P1_Hand) %>% 
    cbind(P2_Hand)
  
  return(braverats_by_turn)
}

get_both_sides <- function(braverats_by_turn){
  # Finally, we need to pivot this table to its longest form, where each row is a single card played by a single player.
  # This will remove the "Player 1" vs "Player 2" distinction.
  # To do this, we simply make a replica dataset with P1 and P2 switched, and append it to our existing dataset.
  
  
  reversed_column_names <- sub('P1', 'P3', colnames(braverats_by_turn), fixed=TRUE) # Swap all 1's to 3's to avoid interferance
  reversed_column_names <- sub('P2', 'P1', reversed_column_names, fixed=TRUE) # Swap all 2's to 1's
  reversed_column_names <- sub('P3', 'P2', reversed_column_names, fixed=TRUE) # Swap all 3's to 2's (effectively swapping all 1's to 2's)
  
  braverats_by_turn_flipped <- braverats_by_turn
  colnames(braverats_by_turn_flipped) <- reversed_column_names
  
  # We also have to flip the Winner column
  braverats_by_turn_flipped <- braverats_by_turn_flipped %>% 
    mutate(Winner = 1 - Winner)
  
  braverats_by_turn_flipped$Flipped <- TRUE
  braverats_by_turn$Flipped <- FALSE
  
  braverats_complete <- rbind(braverats_by_turn, braverats_by_turn_flipped)
  braverats_complete <- braverats_complete %>% 
    mutate(Outcome = ifelse(Winner == 1, "Loss", ifelse(Winner == 0, "Win", "Tie")),
           Round_Winner = ifelse(Round_Winner_P1, "Win",
                                 ifelse(Round_Winner_P2, "Loss", "Tie")))
}

# Add player names back in
braverats_by_turn <- braverats_by_turn %>% 
  left_join(player_names, by = "Game")

# Finally, we need to pivot this table to its longest form, where each row is a single card played by a single player.
# This will remove the "Player 1" vs "Player 2" distinction.
# To do this, we simply make a replica dataset with P1 and P2 switched, and append it to our existing dataset.

reversed_column_names <- sub('P1', 'P3', colnames(braverats_by_turn), fixed=TRUE) # Swap all 1's to 3's to avoid interferance
reversed_column_names <- sub('P2', 'P1', reversed_column_names, fixed=TRUE) # Swap all 2's to 1's
reversed_column_names <- sub('P3', 'P2', reversed_column_names, fixed=TRUE) # Swap all 3's to 2's (effectively swapping all 1's to 2's)

braverats_by_turn_flipped <- braverats_by_turn
colnames(braverats_by_turn_flipped) <- reversed_column_names

# We also have to flip the Winner column
braverats_by_turn_flipped <- braverats_by_turn_flipped %>% 
  mutate(Winner = 1 - Winner)

braverats_by_turn_flipped$Flipped <- TRUE
braverats_by_turn$Flipped <- FALSE

braverats_complete <- rbind(braverats_by_turn, braverats_by_turn_flipped)
braverats_complete <- braverats_complete %>% 
  mutate(Outcome = ifelse(Winner == 1, "Loss", ifelse(Winner == 0, "Win", "Tie")),
         Round_Winner = ifelse(Round_Winner_P1, "Win",
                               ifelse(Round_Winner_P2, "Loss", "Tie")))

# Adjust to match optimal data
braverats_adjusted <- braverats_complete %>% 
  mutate(p1_spy = P2_Spied,
         p2_spy = P1_Spied
         ) %>% 
  rename(p1_holds = Holds_P1, p2_holds = Holds_P2,
         p1_wins = Total_Wins_P1, p2_wins = Total_Wins_P2,
         p1_general = P1_Plus_Two, p2_general = P2_Plus_Two,
         p1_name = P1_name, p2_name = P2_name
         ) %>% 
  mutate(across(matches("^P[12]_C"), ~as.logical(.))) %>% 
  select(-P1_Spied, -P2_Spied, -P1_Strength, -P2_Strength, -Round_Winner_P1, Round_Winner_P2)

colnames(braverats_adjusted) <- sub('P1_C', 'p1_c', colnames(braverats_adjusted), fixed=TRUE)
colnames(braverats_adjusted) <- sub('P2_C', 'p2_c', colnames(braverats_adjusted), fixed=TRUE)

colnames(braverats_adjusted)
colnames(allTurns)
