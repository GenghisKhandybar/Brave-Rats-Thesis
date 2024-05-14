# Reading in data from solution files

getAllTurns <- function(path){
    allTurns <- read.csv(path, header = FALSE, sep = "|", col.names = c("gamestate", "value", "s1_type","p1_optimal","s2_type", "p2_optimal", ".m", "value_matrix")) %>% 
      mutate(gamestate = substr(gamestate,1, nchar(gamestate)-2)) %>%
      mutate(gamestate_str = gamestate) %>% #Making a copy to be used as row labels
      separate(gamestate, c(".1", "p1_cards", ".2", "p2_cards", ".3", "wins", ".4", "generals", ".5", "spies", ".6", "holds"), sep = "-")
    
    rownames(allTurns) <- allTurns$gamestate_str
    allTurns <- allTurns %>% 
      separate(p1_optimal, paste("p1_o", 0:7, sep=''), sep=",", convert = TRUE) %>% 
      separate(p2_optimal, paste("p2_o", 0:7, sep=''), sep=",", convert = TRUE)
    
    allTurns <- allTurns %>% select(-starts_with(".")) # get rid of temp columns starting with "."
    allTurns <- allTurns %>% mutate(cards_left = nchar(p1_cards),
                                    turn = 9-cards_left) %>% 
      mutate(p1_c0 = grepl("0",p1_cards),
             p1_c1 = grepl("1",p1_cards),
             p1_c2 = grepl("2",p1_cards),
             p1_c3 = grepl("3",p1_cards),
             p1_c4 = grepl("4",p1_cards),
             p1_c5 = grepl("5",p1_cards),
             p1_c6 = grepl("6",p1_cards),
             p1_c7 = grepl("7",p1_cards),
             p2_c0 = grepl("0",p2_cards),
             p2_c1 = grepl("1",p2_cards),
             p2_c2 = grepl("2",p2_cards),
             p2_c3 = grepl("3",p2_cards),
             p2_c4 = grepl("4",p2_cards),
             p2_c5 = grepl("5",p2_cards),
             p2_c6 = grepl("6",p2_cards),
             p2_c7 = grepl("7",p2_cards),
             p1_wins = as.numeric(substring(wins, 1,1)),
             p2_wins = as.numeric(substring(wins, 2,2)),
             p1_general = substring(generals, 1,1)==1,
             p2_general = substring(generals, 2,2)==1,
             p1_spy = substring(spies, 1,1)==1,
             p2_spy = substring(spies,2,2)==1,
             p1_holds = as.numeric(substring(holds,1,1)),
             p2_holds = as.numeric(substring(holds,2,2))
      )
  allTurns
}


getAllStrategies <- function(allTurns){
  # For spy turns, expands the responder's strategy into N rows, one for each possible card it's responding to
  
  # For response turns, we have a separate distribution of (equal) probabilities for each opponent's choice
  response_turns <- allTurns %>% 
    filter(s1_type == "s1r") %>% 
    # Each "optimal" here is whether 
    pivot_longer(cols = c("p1_o0","p1_o1","p1_o2","p1_o3","p1_o4","p1_o5","p1_o6","p1_o7"), names_to = "responding_to_card", values_to = "optimal_responses") %>% 
    mutate(responding_to_card = as.numeric(substring(responding_to_card,5,6))) %>% 
    filter(optimal_responses!="") %>% # Cards opponent doesn't have have "" optimal strings
    mutate(num_possibilities = str_length(optimal_responses),
           p1_o0 = ifelse(p1_c0, ifelse(grepl("0",optimal_responses), 1/num_possibilities, 0), NA),
           p1_o1 = ifelse(p1_c1, ifelse(grepl("1",optimal_responses), 1/num_possibilities, 0), NA),
           p1_o2 = ifelse(p1_c2, ifelse(grepl("2",optimal_responses), 1/num_possibilities, 0), NA),
           p1_o3 = ifelse(p1_c3, ifelse(grepl("3",optimal_responses), 1/num_possibilities, 0), NA),
           p1_o4 = ifelse(p1_c4, ifelse(grepl("4",optimal_responses), 1/num_possibilities, 0), NA),
           p1_o5 = ifelse(p1_c5, ifelse(grepl("5",optimal_responses), 1/num_possibilities, 0), NA),
           p1_o6 = ifelse(p1_c6, ifelse(grepl("6",optimal_responses), 1/num_possibilities, 0), NA),
           p1_o7 = ifelse(p1_c7, ifelse(grepl("7",optimal_responses), 1/num_possibilities, 0), NA)
    ) %>% 
    select(-num_possibilities, -optimal_responses)
  
  normal_turns <- allTurns %>% 
    filter(s1_type != "s1r") %>% 
    mutate(responding_to_card = NA) # This indicates that this entry is not a response to an opponent's spied card
  
  
  
  allOptimalStrategies <- rbind(normal_turns, response_turns) %>% 
    mutate(across(matches("^P[12]_o"), ~as.numeric(.))) # Turn all "optimal probability" to numeric
}


# Lists of optimal strategy columns

# A dataframe of just the optimal probabilities (0 for non-possibilities)
# Used for clustering later

allTurns <- getAllTurns("SolutionFiles/updatedOptimalSolution.txt")

allOptimalStrategies <- getAllStrategies(allTurns)

p1_optimal_turns <- allOptimalStrategies %>% 
  #filter(turn <=5) %>% 
  filter(p1_spy == 0 & p2_spy == 0) %>% 
  select("p1_o0","p1_o1","p1_o2","p1_o3","p1_o4","p1_o5","p1_o6","p1_o7") %>% 
  mutate_all(~replace(., is.na(.), 0))


