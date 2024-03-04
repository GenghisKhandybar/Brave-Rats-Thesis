

import nashpy as nash
import numpy as np
np.seterr(divide='ignore', invalid='ignore')

import helperFunctions

class fullRandom:
    def get_strategy(self, gameState, reducedMatrix, player):
        card_count = len(gameState.cardsAvailiable[0])
        return [1/card_count]*card_count
    
class simplexOptimal:
    # This plays by the Simplex optimal solution. It does not solve the game.
    def __init__(self, path):
        try:
            # Read solutions from file (5-10 seconds)
            print("Reading solutions...")
            (knownValues, knownSolutions) = helperFunctions.read_known_solutions(path)
            print("Done reading solutions")
        except Exception as e:
            print("ERROR - NO SOLUTION FILE FOUND")
        self.knownSolutions = knownSolutions

    def get_strategy(self, gameState, reducedMatrix, player):
        strats = self.knownSolutions[gameState.get_game_str()]
        return strats[1][player]
    
class simplexSolver:
    def aggregate_solution(self, reducedMatrix, game_size, player):
        # Takes all availiable strategies and averages them for a final result
        # Ocasionally, these methods yield different strategies that yield the same value
        # However, some of these strategies will be strictly better against sub-optimal play
        # Therefore, this selection of optimal strategy will give better results vs humans

        # Rounding matrix to 6 decimals to prevent floating point decimal error issue
        reducedMatrix = np.round(reducedMatrix,5)
        #print(reducedMatrix)

        

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
            print("Support enumeration failed")
            print(reducedMatrix)
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

    def safe_lemke_howson(self, game, game_size, initial_dropped_label):
        if initial_dropped_label >= game_size:
            strategies = game.support_enumeration()
            first_strat = next(strategies)
            return first_strat
        try:
            strat_gen = game.lemke_howson(initial_dropped_label=initial_dropped_label)
            first_strat = list(strat_gen)
            if min(min(first_strat[0]), min(first_strat[1])) < 0 or np.isnan(first_strat[0][0]): # Somehow this happened once, a sub-zero probability
                return(self.safe_lemke_howson(game, game_size, initial_dropped_label+1))

            return(strat_gen)
        except ValueError:
            print("Error - trying again with higher initial drop label")

            return(self.safe_lemke_howson(game, game_size, initial_dropped_label+1))


    def get_strategy(self, gameState, reducedMatrix, player):

        if len(gameState.cardsAvailiable[0]) == 1:
            return [1]

        
        optimal_strat = self.aggregate_solution(reducedMatrix, gameState.game_size, player)
        """

        strategies = self.safe_lemke_howson(nash_subgame, len(gameState.cardsAvailiable[0]), 0)
        # For some reason, inital_dropped_label = 0 gives an error on rare occasion

        #first_strat = next(strategies)
        # There can be multiple viable strategies but we just take the first
        first_strat = strategies

        if np.isnan(first_strat[0][0]): # Sometimes lemke howson method errors, so we use support enumeration
            # The error in question: nashpy\linalg\tableau.py:318: RuntimeWarning: invalid value encountered in 
            # true_divide return strategy / sum(strategy)
            strategies = nash_subgame.support_enumeration()
            first_strat = next(strategies)
        """

        return optimal_strat
    


test_matrix = [[1.,0.9599359,  0.,0.,0.],
    [0.97916667, 0.,1.,0.,1.],
    [0.89285714, 1.,0.84583333, 0.,0.],
    [1.,1.,1.,0.84681373, 0.],
    [1.,1.,1.,1.,0.89583333]]


solver = simplexSolver()
print(solver.aggregate_solution(test_matrix,3,1))
