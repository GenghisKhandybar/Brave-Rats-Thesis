

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
    def aggregate_solution(self, reducedMatrix, game_size, player):
        # Takes all availiable strategies and averages them for a final result
        # Ocasionally, these methods yield different strategies that yield the same value
        # However, some of these strategies will be strictly better against sub-optimal play
        # Therefore, this selection of optimal strategy will give better results vs humans

        # Rounding matrix to 6 decimals to prevent floating point decimal error issue
        reducedMatrix = np.round(reducedMatrix,5)

        game_size = len(reducedMatrix)
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
            for i in range(game_size):
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

        """
        strat = np.zeros(shape = game_size)
        for s in strategies:
            strat += np.array(s[0])/len(strategies)
        """

        if abs(sum(best_strat)-1) > 0.0001:
            print(f"INVALID STRATEGY: \n{best_strat}")

        return list(best_strat)

    def get_strategy(self, game, reducedMatrix, player):

        if len(game.cardsAvailable[0]) == 1:
            return [1]

        optimal_strat = self.aggregate_solution(reducedMatrix, game.game_size, player)

        return optimal_strat
    
    def get_spy_strat(self, game, reducedMatrix, player, opponent_choice, margin = 0.00001):
        # opponent_choice = None if you are the player who has to choose first
        # opponent_choice = opponent's card chosen otherwise
        if player == 1:
            reducedMatrix = 1 - np.transpose(reducedMatrix)

        myCards = list(game.cardsAvailable[player])

        if opponent_choice is None:

            value = -np.inf
            chosenCards = []
            row_i = 0
            for row in reducedMatrix:
                row_min = min(row)
                if abs(value - row_min) < margin: # If this card is the same as others, we will also use it
                    chosenCards.append(myCards[row_i])
                elif row_min > value: # If this card is better, reset the list and use it
                    value = row_min
                    chosenCards = [myCards[row_i]]
                
                row_i += 1
               
            ans = np.zeros(game.game_size)
            ans[chosenCards] = 1/len(chosenCards)

            return ans

        else:
            # Going second, we know what opponent chose
            
            # If we're player 2, transpose and reverse the values of the matrix

            # UPDATE THIS TO ADD 1 STRATEGY PER OPPONENT'S POSSIBLE STRATEGIES
            other_player = abs(1-player) 
            opp_cards = list(game.cardsAvailable[other_player])
            opponent_column = int(opp_cards.index(opponent_choice))
            possibleValues = reducedMatrix[:,opponent_column]
            bestIndex = np.argmax(possibleValues)

            chosenCard = list(game.cardsAvailable[player])[bestIndex]
                
            ans = np.zeros(game.game_size)
            ans[chosenCard] = 1

            return ans
        
