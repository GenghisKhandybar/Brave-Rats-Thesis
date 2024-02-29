

import nashpy as nash
import numpy as np

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
    def aggregate_solution(self, nash_subgame, game_size):
        # Takes all availiable strategies and averages them for a final result
        # Ocasionally, some strategies will miss opportunities to punish sub-optimal play
        # Therefore, this average will give better results vs humans
        strategies = []
        for i in range(game_size):
            try:
                strat = nash_subgame.lemke_howson(initial_dropped_label=i)
                if not np.isnan(strat[0][0]):
                    strategies.append(strat)
            except Exception:
                pass
        try:
            strat = next(nash_subgame.support_enumeration())
            strategies.append(strat)
        except Exception:
            pass

        strat1 = np.zeros(shape = game_size)
        strat2 = np.zeros(shape = game_size)
        for strat in strategies:
            strat1 += np.array(strat[0])/len(strategies)
            strat2 += np.array(strat[1])/len(strategies)

        if abs(sum(strat1)-1) > 0.0001 or abs(sum(strat2)-1) > 0.0001:
            print(f"INVALID STRATEGIES: \n{strat1}\n{strat2}")

        return (list(strat1), list(strat2))

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

        nash_subgame = nash.Game(reducedMatrix)

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

        return first_strat[player]
