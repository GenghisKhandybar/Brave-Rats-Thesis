# %% [markdown]
# # Braverats Solver

# %%
#Packages
import nashpy as nash
import numpy as np
import json

# other models
import models
import helperFunctions

# %%
def aggregate_solution(nash_subgame, game_size):
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

def safe_lemke_howson(game, game_size, initial_dropped_label):
    if initial_dropped_label >= game_size:
        strategies = game.support_enumeration()
        first_strat = next(strategies)
        return first_strat
    try:
        strat_gen = game.lemke_howson(initial_dropped_label=initial_dropped_label)
        first_strat = list(strat_gen)
        if min(min(first_strat[0]), min(first_strat[1])) < 0: # Somehow this happened once, a sub-zero probability
            return(safe_lemke_howson(game, game_size, initial_dropped_label+1))

        return(strat_gen)
    except ValueError:
        print("Error - trying again with higher initial drop label")

        return(safe_lemke_howson(game, game_size, initial_dropped_label+1))

def write_known_solutions(knownSolutions, path, write_type = "original"):
    with open(path, 'w') as f:
        for key, value in knownSolutions.items():
            if write_type == "original":
                f.write('%s:%s\n' % (key, helperFunctions.get_solution_save_string(value)))
            else:
                game = ratGame(game_str = key)
                if game.gameWinner is None and len(game.wins) == 2: #for 2-digit wins issue
                    #print(key)
                    f.write('%s:%s\n' % (key, helperFunctions.get_solution_save_string_2(game, value)))

# %% ratGame class
class ratGame:
    def __init__(self, game_str = None):
        
        self.game_size = 8

        if game_str is None:
            self.cardsAvailiable =[set(range(self.game_size)),set(range(self.game_size))]
            self.wins = [0,0]
            self.generals = [0,0]
            self.spies = [0,0]
            self.holds = [0,0]

            self.gameWinner = None
            
        else:
            game_str = game_str.split("-")
            self.cardsAvailiable =[set(map(int, list(game_str[1]))), set(map(int, list(game_str[3])))]
            self.wins = list(map(int, list(game_str[5]))) # KNOWN ISSUE: 2-digit win counts
            self.generals = list(map(int, list(game_str[7])))
            self.spies = list(map(int, list(game_str[9])))
            self.holds = list(map(int, list(game_str[11])))

            self.gameWinner = None
            if self.wins[0] >= 4:
                self.gameWinner = 0
            elif self.wins[1] >= 4:
                self.gameWinner = 1
            elif len(self.cardsAvailiable[0]) == 0:
                self.gameWinner = 0.5

    def get_game_str(self, swap_positions = False):
        
        if swap_positions:
            (p1, p2) = 1, 0
        else:
            (p1, p2) = 0, 1

        # Capping wins at 4 and holds at 3, as anything beyond those is irrelevant
        game_str = "p1-" + ''.join(map(str, self.cardsAvailiable[p1])) + \
            "-p2-" + ''.join(map(str, self.cardsAvailiable[p2])) + \
            "-w-" + str(min(4,self.wins[p1])) + str(min(4,self.wins[p2])) + \
            "-g-" + str(self.generals[p1]) + str(self.generals[p2]) + \
            "-s-" + str(self.spies[p1]) + str(self.spies[p2]) + \
            "-h-" + str(min(3,self.holds[p1])) + str(min(3,self.holds[p2]))
         
        return(game_str)

    def innerMatchup(self, values,effects): # Helper function is used for matchup to compare cards
        if(values[0] == values[1] or effects[0] == 0 or effects[1] == 0):
            return 0
        if(effects[0] == 7):
            if(effects[1] == 1):
                return -4
            else:
                return 1
        if(effects[0] == 3 or effects[1] == 3):
            values = (values[1],values[0])
        if(values[0] > values[1]):
            return (1)
        else:
            return (-1)

    def advanceGameState(self, cards): # Imported from braveRatsGame.py
        if cards[0] not in self.cardsAvailiable[0] or cards[1] not in self.cardsAvailiable[1]:
            print("ILLEGAL PLAY" + self.get_game_str() + str(cards))

        values = ((cards[0]+self.generals[0]*2), (cards[1]+self.generals[1]*2)) #Card Values 
        effects = (cards[0], cards[1])

        if(effects[0] == 5 or effects[1] == 5): #WIZARD
            effects = (8, 8) #irrelevant numbers
        if(values[0] < values[1]): #For the inner function, v0 > v1
            a = -self.innerMatchup([values[1],values[0]],[effects[1],effects[0]])
        else:
            a = self.innerMatchup(values,effects)
            
        for i in range(2):
            if(effects[i] == 4): #Ambassador : Gives you +1 hold
                self.holds[i] += 1 

        if(a>0):
            self.wins[0] += a + self.holds[0]
            self.holds = [0,0]
        elif(a<0):
            self.wins[1] += -a + self.holds[1]
            self.holds = [0,0]
        else:
            self.holds[0] += 1
            self.holds[1] += 1

        self.spies = [0,0]
        self.generals = [0,0]
        for i in range(2):
            if(effects[i] == 6 and effects[1-i] != 6):
                self.generals[i] = 1
            if(effects[i] == 2 and effects[1-i] != 2):
                self.spies[i] = 1

            self.cardsAvailiable[i].remove(cards[i])

        if self.wins[0] >= 4:
            self.gameWinner = 0
        elif self.wins[1] >= 4:
            self.gameWinner = 1

    def suboptimal_simultaneous_solve(self, reducedMatrix, models):
        strat_results = [0,0] # 0's are placeholders for distributions

        # Grab the optimal strategies if necessary for either player
        
        for i in range(2):
            strat_results[i] = models[i].get_strategy(self, reducedMatrix = reducedMatrix, player = i)
            # Validate the strategy
            if abs(sum(strat_results[i])-1) > 0.00001:
                print(f"WARNING: Invalid strategy on turn {self.get_game_str()}: {strat_results[i]}")

        # Calculate the value of the game given these models
        p2_card_vals = np.matmul(np.array(strat_results[0]), reducedMatrix)
        value = np.matmul(np.array(strat_results[1]), p2_card_vals)

        state_info = (value, [list(strat_results[0]), list(strat_results[1])], ["Non-optimal"])

        return (state_info, value)

    def getValue(self, knownValues, knownSolutions, models):
        # Returns the value of the current gamestate
        # Strategy can either be "Optimal" or an object with property get_strategy that takes a gamestate (and player #)
        # and returns a list of probabilities for each of that player's possible cards.

        #First,  check if the game is over (win/loss or tie)
        if self.gameWinner is not None:
            knownSolutions[self.get_game_str()] = (1 - self.gameWinner, [[0],[0]], "")
            return 1 - self.gameWinner
        elif len(self.cardsAvailiable[0]) == 0:
            knownSolutions[self.get_game_str()] = (0.5, [[0],[0]], "")
            return 0.5

        # If the game is not over, we must make a matrix of all sub-games, and calculate value from it.
        currentRoundMatrix = np.zeros(shape=(self.game_size,self.game_size))
        # Start with a k by k matrix, and get the value for each corresponding sub-state.
        for p1_next in self.cardsAvailiable[0]:
            for p2_next in self.cardsAvailiable[1]:
                subGame = ratGame() #ratGame(self.get_game_str())
                # Copying manually should be faster than using the gamestring
                subGame.cardsAvailiable = [self.cardsAvailiable[0].copy(), self.cardsAvailiable[1].copy()]
                subGame.wins = self.wins[:]
                subGame.generals = self.generals[:]
                subGame.spies = self.spies[:]
                subGame.holds = self.holds[:]

                subGame.advanceGameState([p1_next, p2_next])

                subGameStr = subGame.get_game_str()
                
                if subGameStr in knownValues:
                    # We may already know this state's value
                    currentRoundMatrix[p1_next][p2_next] = knownValues[subGameStr]
                    
                else: 
                    # Otherwise, we have to solve the sub-game before solving this game
                    #print("Playing [" + str(p1_next) + ", " + str(p2_next) + "], Generating value for sub-game: " + subGameStr)

                    subGameValue = subGame.getValue(knownValues, knownSolutions, models)
                    
                    if subGameValue < 0 or subGameValue > 1:
                        print(f"Invalid value in position {self.get_game_str()}: {subGameValue}")

                    currentRoundMatrix[p1_next][p2_next] = subGameValue

                    #print("Got value: " + str(round(subGameValue,4)))

                    #print("Generated new value: " + str(subGameValue))

        game_str = self.get_game_str()
        reducedMatrix = currentRoundMatrix[list(self.cardsAvailiable[0])]
        reducedMatrix = reducedMatrix[:, list(self.cardsAvailiable[1])]

        if max(self.spies) == 0:
            # Non-spy round - solve via simultaneous move (simplex method)

            gameResult = self.suboptimal_simultaneous_solve(reducedMatrix, models)
        else:
            # STILL DOING SPY ROUNDS OPTIMALLY FOR ALL PLAYER TYPES FOR NOW
            # Spy round - sequential solve (minmax)
            gameResult = self.sequential_sovle(reducedMatrix, first_player = self.spies[0]) # 0 = p1 first, 1 = p2 first

            
        knownSolutions[game_str] = gameResult[0]
        knownValues[game_str] = float(gameResult[1])

        if len(self.cardsAvailiable[0]) >= 7: # Status progress report on high-level turns
            #print("Finished solving gamestate " + self.get_game_str())
            print("Finished solving state" + game_str + " with value " + str(gameResult[1]))

        return gameResult[1]
    
    def simultaneous_solve(self, reducedMatrix):

        if len(self.cardsAvailiable[0]) == 1:
            value = reducedMatrix[0][0]
            state_info = (value, [[1], [1]], reducedMatrix)
            return (state_info, value)

        nash_subgame = nash.Game(reducedMatrix)

        strategies = safe_lemke_howson(nash_subgame, len(self.cardsAvailiable[0]), 0)
        # For some reason, inital_dropped_label = 0 gives an error on rare occasion

        #first_strat = next(strategies)
        # There can be multiple viable strategies but we just take the first
        first_strat = strategies

        if np.isnan(first_strat[0][0]): # Sometimes lemke howson method errors, so we use support enumeration
            # The error in question: nashpy\linalg\tableau.py:318: RuntimeWarning: invalid value encountered in 
            # true_divide return strategy / sum(strategy)
            strategies = nash_subgame.support_enumeration()
            print(reducedMatrix)
            first_strat = next(strategies)
        
        #first_strat = aggregate_solution(nash_subgame, len(self.cardsAvailiable[0]))
        
        value = nash_subgame[list(first_strat[0]), list(first_strat[1])][0]

        state_info = (value, [list(first_strat[0]), list(first_strat[1])], reducedMatrix)
        
        return (state_info, value)
    
    def sequential_sovle(self, reducedMatrix, first_player):
        # For spy turns, we will simply use minmax.
        game_size = len(self.cardsAvailiable[0])

        if first_player == 1:
            # If P2 is going first, we transpose and take the negative to figure out which is best.
            updatedMatrix = - np.transpose(reducedMatrix)
        else:
            updatedMatrix = reducedMatrix
        
        value = -np.inf
        best_row = 0
        row_i = 0
        for row in updatedMatrix:
            row_min = min(row)
            if row_min > value:
                value = row_min
                best_row = row_i
            row_i += 1
        
        best_col = np.where(updatedMatrix[best_row] == value)
        
        strategies = [np.zeros(game_size), np.zeros(game_size)]
        strategies[0][best_row] = 1
        strategies[1][best_col[0][0]] = 1
        
        if first_player == 1:
            (strategies[0], strategies[1]) = (strategies[1], strategies[0])
            value = -value

        state_info = (value, strategies, reducedMatrix)

        return (state_info, value)

# %% functions for command line interface
def print_solution(statename, knownSolutions):
    print(create_solution_str(ratGame(statename),knownSolutions[statename]))

def standardize_decimal(num):
    return str(round(num, 4)).ljust(6, " ")

def create_solution_str(game, tup):
    if not (game.gameWinner is None):
        return '!!!!!!!! WINNER: PLAYER %d !!!!!!!!'%(game.gameWinner + 1)
    
    ans = "State id: %s"%(game.get_game_str())
    ans += '\n\nWins: %s    Holds: %s'%(", ".join(map(str,game.wins)), ", ".join(map(str,game.holds)))
    ans += ("\nGenerals used: " + ", ".join(map(str,game.generals)) + "    Spies used: "  + ", ".join(map(str,game.spies)))
    ans += ("\nP1 Win Prob: " + str(round(100*tup[0],5)) + "%")
    ans += ("\nP1 Cards:            " + ",      ".join(map(str,game.cardsAvailiable[0])))
    ans += ("\nP1 Optimal Strategy: " + ", ".join(map(standardize_decimal,np.round(tup[1][0],4))))
    ans += ("\nP2 Cards:            " + ",      ".join(map(str,game.cardsAvailiable[1])))
    ans += ("\nP2 Optimal Strategy: " + ", ".join(map(standardize_decimal,np.round(tup[1][1],4))))
    ans += "\n\n                          P2"
    
    ans += "\nP1      "# + ",       ".join(map(str,game.cardsAvailiable[1])))
    p1_cards_list = list(game.cardsAvailiable[0])
    p2_cards_list = list(game.cardsAvailiable[1])
    for i in range(len(p2_cards_list)):
        ans += '%s (%s%%)  '%(str(p2_cards_list[i]), str(round(100*tup[1][1][i])).rjust(2))

    for i in range(len(p1_cards_list)):
        ans += '\n%s (%s%%) %s' % (str(p1_cards_list[i]), str(round(100*tup[1][0][i])).rjust(2), " | ".join(map(standardize_decimal,tup[2][i])))
        #ans += "\n" + str(p1_cards_list[i]) + " (" +  + " | ".join(map(standardize_decimal,tup[2][i]))

    #ans += ("\nCurrent Game Matrix: \n" + str(np.round(tup[2], 5)))
    return ans
        
def play_game(game, knownSolutions, players):
    state_str = game.get_game_str()
    solution = knownSolutions[state_str]
    game_size = len(game.cardsAvailiable[0])

    plays = [None, None]
    for player in range(2):
        p_type = players[player]
        if p_type == "Human":
            choices = game.cardsAvailiable[player]
            print('Choose card for player %d:'%(player + 1))
            
            while True:
                response = input()
                if response.isdigit():
                    response = int(response)
                    if response in choices:
                        plays[player] = response
                        break
                elif response == "a":
                    p_type = "AI"
                    break
                elif response == "r":
                    p_type = "Random"
                    break
                elif response == "x":
                    return
                print(f"Choose again. 'a' for AI, 'r' for random 'x' to exit, or one of the following:\n{str(choices)}")

        if p_type == "AI":
            probs = solution[1][player]
            plays[player] = list(game.cardsAvailiable[player])[np.random.choice(game_size, 1, p = probs)[0]]
            print(f"AI Choice: {plays[player]}")

        if p_type == "Random":
            plays[player] = list(game.cardsAvailiable[player])[np.random.choice(game_size, 1)[0]]
            print(f"Random Choice: {plays[player]}")
    
    print("\nPLAYING CARDS %d AND %d -----------------------------------------------\n"%(plays[0], plays[1]))

    game.advanceGameState(plays)
    #print(knownSolutions[game.get_game_str()])
    print_solution(game.get_game_str(), knownSolutions)

    if game.gameWinner == None:
        print("Next Round?")
        _ = input()
        play_game(game, knownSolutions, players)

# %%

def game_loop():
    while(True):
        print("Select starting position string. Hit enter to start a new game.")
        response = input("Press enter to start a new game. Paste a gamestate string to start from a specific state.")

        if len(response) == 0:
            game = ratGame()
        else:
            try:
                game = ratGame(response)
            except Exception as e:
                print(e)
                print("Invalid state string. Try again.")
                continue

        print_solution(game.get_game_str(), knownSolutions)

        play_game(game, knownSolutions, players = ["Human", "Human"])

        response = input("Play again? (n = no)")

        if(response == "n"):
            break

def default_console_start(path):
        # Try to read the file "knownSolutions.txt"
    # If we can't, create the solutions from scratch
    try:
        # Read solutions from file (5-10 seconds)

        print("Reading solutions...")

        
        (knownValues, knownSolutions) = helperFunctions.read_known_solutions(path)

        print("Done reading solutions")
    except Exception as e:
        # If file couldn't be read, we'll make a new one

        

        print(e)
        print("Solution file not found. Solving manually. (Should take 6-10 minutes?)")

        testGame = ratGame('p1-01234567-p2-01234567-w-00-g-00-s-00-h-00')
        # This will solve the game
        knownValues = {}
        knownSolutions = {}

        testGame.getValue(knownValues, knownSolutions, models= [models.simplexSolver, models.simplexSolver])
        write_known_solutions(knownSolutions, path)
            
    write_known_solutions(knownSolutions, "knownSolutions2.txt", write_type="new")

    game_loop()
# %%

path = "knownSolutions.txt"

if __name__ == "__main__":
    #default_console_start(path)
    testGame = ratGame('p1-01234567-p2-01234567-w-11-g-00-s-00-h-00')
    # This will solve the game
    knownValues = {}
    knownSolutions = {}

    #[models.fullRandom(), models.fullRandom()]
    testGame.getValue(knownValues, knownSolutions, models= [models.simplexSolver(), models.fullRandom()])
    print(knownSolutions)
    write_known_solutions(knownSolutions, "knownSolutionsNew")
        
    write_known_solutions(knownSolutions, "playableSolutionsNew.txt", write_type="new")
