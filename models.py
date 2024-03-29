

import nashpy as nash
import numpy as np
np.seterr(divide='ignore', invalid='ignore')

import helperFunctions

class fullRandom:
    def get_strategy(self, game, reducedMatrix, player):
        card_count = len(game.cardsAvailable[0])
        return [1/card_count]*card_count
    
class savedSimplexOptimal:
    # This plays by the Simplex optimal solution. It does not solve the game.
    def __init__(self, path):
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
        strat = self.knownSolutions[game.get_game_str()][1][player]

        # Next, subset the list of probabilities to only those for available cards
        strat = strat[list(game.cardsAvailable[player])] 
        return strat
    
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
        strat = np.zeros(shape = 8)
        reduced_index = 0 # index in reduced-grid strategy
        for i in range(8):
            if i in game.cardsAvailable[player]:
                strat[i] = optimal_strat[reduced_index]
                reduced_index += 1

        return strat
    
    def get_first_spy_strat(self, game, reducedMatrix, player, margin = 0.00001, row_sum_margin = 0.005):
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

    def get_second_spy_strat(self, game, reducedMatrix, player, margin = 0.00001):
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
