

import nashpy as nash
import numpy as np
np.seterr(divide='ignore', invalid='ignore')

import helperFunctions

# First, the 'default' way some models solve sequential turns
def optimal_first_spy_strat(game, reducedMatrix, player, margin = 0.00001, row_sum_margin = 0.005):
    if player == 1:
        reducedMatrix = 1 - np.transpose(reducedMatrix)

    myCards = list(game.cardsAvailable[player])

    value = -np.inf
    best_row_sum = -np.inf
    chosenCards = []
    row_i = 0
    for row in reducedMatrix:
        row_min = min(row)
        row_sum = sum(row) # row_sum is secondary, not as important. It represents how good the card is on average
        # We give it a much more lenient margin for diverse play, as it doesn't even effect optimal play values
        if abs(value - row_min) < margin and abs(row_sum - best_row_sum) < row_sum_margin: # If this card is the same as others, we will also use it
            chosenCards.append(myCards[row_i])
            best_row_sum = max(best_row_sum, best_row_sum)
        elif row_min > value or (abs(value - row_min) < margin and row_sum > best_row_sum): 
            # If this card is better, reset the list and use it
            # It can be either:
            # - Better value
            # - Equal value and better row sum
            value = row_min
            best_row_sum = row_sum
            chosenCards = [myCards[row_i]]
        
        row_i += 1
        
    ans = np.zeros(8)
    ans[chosenCards] = 1/len(chosenCards)

    if abs(sum(ans) - 1) > margin:
        print(f"ERROR in first turn solve - invalid response: {ans}")

    return ans

def optimal_second_spy_strat(game, reducedMatrix, player, margin = 0.00001):
    # opponent_choice = None if you are the player who has to choose first
    # opponent_choice = opponent's card chosen otherwise
    if player == 1:
        reducedMatrix = 1 - np.transpose(reducedMatrix)
    other_player = abs(1-player)

    myCards = list(game.cardsAvailable[player])
    

    # Going second, we know what opponent chose
    
    # If we're player 2, transpose and reverse the values of the matrix

    # For each choice the opponent could make, we have a list of cards we could play
    ans = [] # Placeholder list
    opp_cards = list(game.cardsAvailable[other_player])

    
    reduced_i = 0 
    for i in range(game.game_size):
        if i in opp_cards:
            #opponent_column = int(opp_cards.index(opponent_choice))
            possibleValues = reducedMatrix[:,reduced_i]
            bestVal = np.max(possibleValues)
            bestIndexes = list(np.flatnonzero(abs(possibleValues-bestVal) < margin))
            bestCards = [myCards[x] for x in bestIndexes]  # A list of all valid cards to play (ex. "[4,6,7]" if each of those cards is equally viable)
            ans.append(bestCards)
            reduced_i += 1
        else:
            ans.append([])
    
    return ans

# Agents (models)

class naive:
    # This is a modifier to another model.
    # Specify a model you'd like to modify and two value matrices you'd like them to use

    def __init__(self, model, path_normal, path_general_turn):
        # Initialize the model with naive value matrices from 2 existing solution files
        # The first is from any typical solution file
        # The second is for general turns, coming from a modified matrix where P1 starts with a general's +2 ability
        # We also take the inverse transpose of the second matrix for turns when opponent usese general
        self.normal_matrix = helperFunctions.read_known_solutions(path_normal)['p1-01234567-p2-01234567-w-00-g-00-s-00-h-00'][2]
        gen_matrix = helperFunctions.read_known_solutions(path_general_turn)['p1-01234567-p2-01234567-w-00-g-10-s-00-h-00'][2]
        self.general_matrix = gen_matrix
        self.p2_general_matrix = 1-np.transpose(self.general_matrix)
        self.model = model

    def get_naive_reduced_matrix(self, game):
        # Return a 
        if max(game.generals) == 0:
            m = self.normal_matrix
        elif game.generals[0] == 1:
            m =  self.general_matrix
        else:
            m = self.p2_general_matrix

        reducedMatrix = m[list(game.cardsAvailable[0])]
        reducedMatrix = reducedMatrix[:, list(game.cardsAvailable[1])]
        return reducedMatrix

    def get_strategy(self, game, reducedMatrix, player):
        return self.model.get_strategy(game, self.get_naive_reduced_matrix(game), player)
            
    
    def get_first_spy_strat(self, game, reducedMatrix, player):
        return self.model.get_first_spy_strat(game, self.get_naive_reduced_matrix(game), player)
    
    def get_second_spy_strat(self, game, reducedMatrix, player):
        return self.model.get_second_spy_strat(game, self.get_naive_reduced_matrix(game), player)

class randomNonSpy:
    # Plays randomly, except for spy turns, which it plays optimally
    def get_strategy(self, game, reducedMatrix, player):
        card_count = len(game.cardsAvailable[0])

        return [1/card_count if i in game.cardsAvailable[player] else 0 for i in range(8)]
    
    def get_first_spy_strat(self, game, reducedMatrix, player, margin = 0.00001, row_sum_margin = 0.005):
        return optimal_first_spy_strat(game, reducedMatrix, player, margin = margin, row_sum_margin = row_sum_margin)

    def get_second_spy_strat(self, game, reducedMatrix, player, margin = 0.00001):
        return optimal_second_spy_strat(game, reducedMatrix, player, margin = margin)

class intuitiveDistribution:
    def get_strategy(self, game, reducedMatrix, player, margin = 0.000001):
        # For each card our opponent can pick, we'll find the best card/cards to counter it.
        card_count = len(game.cardsAvailable[0])
        if player == 1:
            reducedMatrix = 1 - np.transpose(reducedMatrix)
        reduced_strat = np.zeros(card_count)

        max_values = np.max(reducedMatrix, axis=0)
        indices_within_margin = [
            np.abs(column - max_val) < margin for column, max_val in zip(np.transpose(reducedMatrix), max_values)
        ]
        for valid_responses in indices_within_margin:
            valid_responses = np.array(valid_responses).astype(int) # Converts booleans to ints
            valid_responses = valid_responses/sum(valid_responses) # Scales to sum to 1
            reduced_strat += valid_responses
        
        reduced_strat = reduced_strat/card_count # Scales to sum to 1

        return helperFunctions.expandProbabilities(reduced_strat, game.cardsAvailable[player])
    
    def get_first_spy_strat(self, game, reducedMatrix, player, margin = 0.00001, row_sum_margin = 0.005):
        return optimal_first_spy_strat(game, reducedMatrix, player, margin = margin, row_sum_margin = row_sum_margin)

    def get_second_spy_strat(self, game, reducedMatrix, player, margin = 0.00001):
        return optimal_second_spy_strat(game, reducedMatrix, player, margin = margin)

# Best Response Model
class defeatStrategy:
    # This AI knows what strategy its opponent is playing and counters it perfectly
    # This is similar to how the Optimal AI decides which cards to play from an opponent's spy play
    # This model is defined using another strategy profile opponentAI as a parameter
    def __init__(self, opponentAI):
        self.opponent = opponentAI

    def get_strategy(self, game, reducedMatrix, player, margin = 0.00001):
        oppStrat = self.opponent.get_strategy(game=game, reducedMatrix=reducedMatrix, player=abs(1-player))
        oppStrat = helperFunctions.reduceProbabilities(oppStrat, game.cardsAvailable[abs(1-player)])

        if player == 1:
            reducedMatrix = 1 - np.transpose(reducedMatrix)

        myCards = list(game.cardsAvailable[player])

        cardValues = np.matmul(np.array(oppStrat), np.transpose(reducedMatrix))
        
        best_value = -np.inf
        chosenCards = []
        row_i = 0
        for i, cardVal in enumerate(cardValues):
            if abs(best_value - cardVal) < margin: # If this card is the same as others, we will also use it
                chosenCards.append(myCards[i])
            elif cardVal > best_value: 
                # If this card is better, reset the list and use it
                best_value = cardVal
                chosenCards = [myCards[i]]
            
            row_i += 1
            
        ans = np.zeros(8)
        ans[chosenCards] = 1/len(chosenCards)

        if abs(sum(ans) - 1) > margin:
            print(f"ERROR in first turn solve - invalid response: {ans}")

        return ans
    
    def get_first_spy_strat(self, game, reducedMatrix, player, margin = 0.00001):
        oppStrat = self.opponent.get_second_spy_strat(game, reducedMatrix, abs(1-player))
        oppStrat = [oppStrat[i] for i in list(game.cardsAvailable[player])] # Remove the 'None' strategies for cards you don't have

        if player == 1:
            reducedMatrix = 1 - np.transpose(reducedMatrix)

        best_value = -np.inf
        chosenCards = []
        for i, valueRow in enumerate(reducedMatrix):
            oppPossibilities = helperFunctions.reduceIndexes(oppStrat[i], game.cardsAvailable[abs(1-player)]) # List of cards opponent would play against this choice
            possibleValues = valueRow[oppPossibilities]
            avgValue = np.mean(possibleValues)
            if abs(avgValue - best_value) < margin:
                chosenCards.append(i)
            elif avgValue > best_value:
                chosenCards = [i]
                best_value = avgValue

        chosenCards = helperFunctions.expandIndexes(chosenCards, game.cardsAvailable[player]) # convert to 8-long
        ans = np.zeros(8)
        ans[chosenCards] = 1/len(chosenCards)

        return ans  
    
    def get_second_spy_strat(self, game, reducedMatrix, player, margin = 0.00001):
        return optimal_second_spy_strat(game, reducedMatrix, player, margin = margin)

# Uniform model
class fullRandom:
    def get_strategy(self, game, reducedMatrix, player):
        card_count = len(game.cardsAvailable[0])

        return [1/card_count if i in game.cardsAvailable[player] else 0 for i in range(8)]
    
    def get_first_spy_strat(self, game, reducedMatrix, player):
        card_count = len(game.cardsAvailable[0])

        return [1/card_count if i in game.cardsAvailable[player] else 0 for i in range(8)]
    
    def get_second_spy_strat(self, game, reducedMatrix, player):
        base_strat = list(game.cardsAvailable[player])
        return [base_strat if i in game.cardsAvailable[abs(1-player)] else None for i in range(8)] 

# This model is used to play models 
class savedModel:
    # This plays an existing strategy profile. It does not solve the game.
    def __init__(self, path, use_player_index = 0):
        # use_player_index indicates which player in the game file we would like to play from
        # Usually this is the first player (0), but in some cases we may want to use index 1.
        self.use_player_index = use_player_index
        try:
            # Read solutions from file (5-10 seconds)
            print("Reading solutions...")
            knownSolutions = helperFunctions.read_known_solutions(path)
            print("Done reading solutions")
        except Exception as e:
            print(f"ERROR - NO SOLUTION FILE FOUND - {str(e)}")
        self.knownSolutions = knownSolutions

    def get_strategy(self, game, reducedMatrix, player):
        # Get the strategy for the appropriate player
        # Reverse the game string if we need to get the strategy that player 2 used
        p1_game_str = game.get_game_str() if player == self.use_player_index else helperFunctions.reverseGameState(game.get_game_str())
        return self.knownSolutions[p1_game_str][1][self.use_player_index]
    
    def get_first_spy_strat(self, game, reducedMatrix, player):
        # Get the strategy for the appropriate player
        return self.get_strategy(game, reducedMatrix, player)

    def get_second_spy_strat(self, game, reducedMatrix, player):
        # Get the strategy for the appropriate player (it will be in the correct format)
        return self.get_strategy(game, reducedMatrix, player)
    
class simplexSolver:
    def aggregate_solution(self, game, reducedMatrix, player):
        # Takes all availiable strategies and averages them for a final result
        # Ocasionally, these methods yield different strategies that yield the same value
        # However, some of these strategies will be strictly better against sub-optimal play
        # Therefore, this selection of optimal strategy will give better results vs humans

        # Rounding matrix to 5 decimals to prevent floating point decimal error issue
        reducedMatrix = np.round(reducedMatrix,5)

        reduced_game_size = len(reducedMatrix)
        nash_subgame = nash.Game(reducedMatrix)
        strategies = []

        # First, support enumeration
        if len(strategies) == 0:
            #print("Unusual case - no equilibira found by Vertex Enumeration or Lemke Howson. Trying Support Enumeration.")
            try:
                strat = next(nash_subgame.support_enumeration())
                if not np.isnan(strat[0][0]):
                    strategies.append(strat[player])
            except Exception:
                pass

        # First, vertex enumeration
        if len(strategies) == 0:
            #print("Support enumeration failed")
            #print(reducedMatrix)
            equilibria = nash_subgame.vertex_enumeration()
            try:
                for eq in equilibria:
                    strategies.append(eq[player])
            except Exception:
                pass   

        # Next, lemke howson if that didn't work
        if len(strategies) == 0:
            print("Unusual case - no equilibira found by vertex enumeration or support enumeration. Trying lemke howson.")
            print(reducedMatrix)
            for i in range(reduced_game_size):
                try:
                    strat = nash_subgame.lemke_howson(initial_dropped_label=i)
                    if not np.isnan(strat[0][0]):
                        strategies.append(strat[player])
                except Exception:
                    pass        
        
        best_val = -1 # Impossibly low to start
        best_strat = None

        # Creating a matrix to calculate "total value of strategy"
        # For each card, multiply p(play that card) * sum(possible values given that card played)
        card_total_vals = np.sum(np.array(reducedMatrix), axis=0) if player==0 else (np.sum(1-np.array(reducedMatrix), axis=1))
        for s in strategies:
            strat_val_sum = np.matmul(card_total_vals,np.transpose(s))
            if strat_val_sum > best_val:
                best_val = strat_val_sum
                best_strat = s

        #aggregated_optimal = np.sum(np.array(strategies), axis = 0) / len(strategies)

        if best_strat is None:
            print("\nERROR in aggregate_solution: No strategies found.\n")

        if abs(sum(best_strat)-1) > 0.0001:
            print(f"INVALID STRATEGY: \n{best_strat}")

        return list(best_strat)

    def get_strategy(self, game, reducedMatrix, player):
        # Trivial solution for only 1 card
        if len(game.cardsAvailable[0]) == 1:
            strat = np.zeros(shape = 8)
            strat[list(game.cardsAvailable[player])[0]] = 1
            return strat

        optimal_strat = self.aggregate_solution(game, reducedMatrix, player)

        # Convert reduced-from strategy into long-form strategy
        strat = helperFunctions.expandProbabilities(optimal_strat, game.cardsAvailable[player])

        return strat
    
    def get_first_spy_strat(self, game, reducedMatrix, player, margin = 0.00001, row_sum_margin = 0.005):
        return optimal_first_spy_strat(game, reducedMatrix, player, margin = margin, row_sum_margin = row_sum_margin)

    def get_second_spy_strat(self, game, reducedMatrix, player, margin = 0.00001):
        return optimal_second_spy_strat(game, reducedMatrix, player, margin = margin)


