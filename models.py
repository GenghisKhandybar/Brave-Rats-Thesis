

#import solutionConsole
import helperFunctions

class fullRandom:
    def get_strategy(gameState, player):
        card_count = len(gameState.cardsAvailiable[0])
        return [1/card_count]*card_count
    
class simplexOptimal:
    def __init__(self, path):
        try:
            # Read solutions from file (5-10 seconds)
            print("Reading solutions...")
            (knownValues, knownSolutions) = helperFunctions.read_known_solutions(path)
            print("Done reading solutions")
        except Exception as e:
            print("ERROR - NO SOLUTION FILE FOUND")
        self.knownSolutions = knownSolutions

    def get_strategy(self, gameState, player):
        strats = self.knownSolutions[gameState.get_game_str()]
        return strats[1][player]
