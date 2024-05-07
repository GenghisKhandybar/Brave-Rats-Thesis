
from solutionConsole import *

# %% ratGame
# Functions for recording the probabilities of every possible state
def add_resulting_probabilities(game, knownSolutions, stateProbs):
    # Updates a dictionary of this state and all sub-games
    # with the probability of reaching each gamestate (given a solution file)
    # Returns a set of all sub-game states reached from current state.
    # This is for weighting each state, for training simplified models
    gameStr = game.get_game_str()
    current_turn_prob = stateProbs[gameStr]
    player_strategies = knownSolutions[gameStr][1]
    strat_types = knownSolutions[gameStr][4]
    next_turn_states = set() # All non-zero-probability states in the next turn
    
    for p1_card in game.cardsAvailable[0]:
        for p2_card in game.cardsAvailable[1]:
            subGame = game.copy()
            subGame.advanceGameState([p1_card, p2_card])
            subGameStr = subGame.get_game_str()
            
            # Conditional prob = probability of playing cards p1_card and p2_card on this turn 
            if strat_types[0] == "s": # Simultaneous turn
                conditionalProb = player_strategies[0][p1_card] * player_strategies[2][p2_card] 
            elif strat_types[0] == "f": # Turns where p1 goes first
                conditionalProb = player_strategies[0][p1_card] * \
                    (1/len(player_strategies[1][p1_card]) if p2_card in player_strategies[1][p1_card] else 0)
            else: # turns where p2 goes first
                conditionalProb = player_strategies[1][p2_card] * \
                    (1/len(player_strategies[0][p2_card]) if p1_card in player_strategies[0][p2_card] else 0)
                
            if conditionalProb > 0: # Only traverse and record if p > 0
                if subGame.gameWinner is None: # Don't traverse further on end-states
                    next_turn_states.add(subGameStr)

                if subGameStr in stateProbs:
                    stateProbs[subGameStr] += current_turn_prob * conditionalProb
                else:
                    stateProbs[subGameStr] = current_turn_prob * conditionalProb

def getAllStateProbabilities(knownSolutions, writePath, initialGameState = 'p1-01234567-p2-01234567-w-00-g-00-s-00-h-00',):
    statesToTraverse = [initialGameState]
    stateProbs = {initialGameState:1}
    nextStates = set()
    while len(statesToTraverse) > 0:
        for state in statesToTraverse:
            # Add probabilities from this state
            additionalStates = add_resulting_probabilities(ratGame(state), knownSolutions, stateProbs)
            # Add the next turn states from this state to our list
            nextStates = nextStates.union(additionalStates)
        statesToTraverse = list(nextStates)
        nextStates = set()

    with open(writePath, 'w') as f:
        for key, value in stateProbs.items():
            f.write(f"{key}|{value}")

