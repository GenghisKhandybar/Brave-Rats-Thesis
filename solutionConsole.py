# %% [markdown]
# # Braverats Solver

# %%
#Packages
import nashpy as nash
import numpy as np

# Timing
from time import perf_counter as pc


# Local Files
import models
import helperFunctions

# %%
# Global reporting variables
overall_startTime = pc()
startTime = pc()
saveStartTime = pc()
computed_states = 0

# %% ratGame class
class ratGame:
    def __init__(self, game_str = None):
        
        self.game_size = 8

        if game_str is None:
            self.cardsAvailable =[set(range(self.game_size)),set(range(self.game_size))]
            self.wins = [0,0]
            self.generals = [0,0]
            self.spies = [0,0]
            self.holds = [0,0]

            self.gameWinner = None
            
        else:
            game_str = game_str.split("-")
            self.cardsAvailable =[set(map(int, list(game_str[1]))), set(map(int, list(game_str[3])))]
            self.wins = list(map(int, list(game_str[5])))
            self.generals = list(map(int, list(game_str[7])))
            self.spies = list(map(int, list(game_str[9])))
            self.holds = list(map(int, list(game_str[11])))

            self.gameWinner = None
            if self.wins[0] >= 4:
                self.gameWinner = 0
            elif self.wins[1] >= 4:
                self.gameWinner = 1
            elif len(self.cardsAvailable[0]) == 0:
                self.gameWinner = 0.5

    def get_game_str(self, swap_positions = False):
        
        if swap_positions:
            (p1, p2) = 1, 0
        else:
            (p1, p2) = 0, 1

        # Capping wins at 4 and holds at 3, as anything beyond those is irrelevant
        game_str = "p1-" + ''.join(map(str, self.cardsAvailable[p1])) + \
            "-p2-" + ''.join(map(str, self.cardsAvailable[p2])) + \
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
        if cards[0] not in self.cardsAvailable[0] or cards[1] not in self.cardsAvailable[1]:
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

            self.cardsAvailable[i].remove(cards[i])

        if self.wins[0] >= 4:
            self.gameWinner = 0
        elif self.wins[1] >= 4:
            self.gameWinner = 1

    def get_strats_and_value(self, currentRoundMatrix, reducedMatrix, models):
        strat_results = [0,0] # 0's are placeholders for distributions

        # Grab the optimal strategies if necessary for either player
        
        for i in range(2):
            strat_results[i] = models[i].get_strategy(self, reducedMatrix, player = i)
            # Validate the strategy
            if abs(sum(strat_results[i])-1) > 0.00001:
                print(f"WARNING: Invalid strategy on turn {self.get_game_str()}: {strat_results[i]}")

        # Calculate the value of the game given these models
        p2_card_vals = np.matmul(np.array(strat_results[0]), currentRoundMatrix)
        value = np.matmul(np.array(strat_results[1]), p2_card_vals)

        # value, strategies, reducedmatrix, strategy types
        # Both strategies are type "s" for simultaneous - one probability per card
        state_info = (value, [list(strat_results[0]), list(strat_results[1])], reducedMatrix, ["s", "s"])

        return state_info

    def get_sequential_strats_and_value(self, currentRoundMatrix, reducedMatrix, models, first_player, margin = 0.00001):
        strat_results = [0,0] # 0's are placeholders for first player's distribution and second player's response lists

        # Grab the optimal strategies if necessary for either player
        second_player = abs(1-first_player)
        
        # We use indexes 0 and 1 to indicate turn order. These will be flipped at the end if first_player == 1
        strat_results[0] = models[first_player].get_first_spy_strat(self, reducedMatrix, first_player)
        strat_results[1] = models[second_player].get_second_spy_strat(self, reducedMatrix, second_player)

        if first_player == 1:
           currentRoundMatrix = np.transpose(currentRoundMatrix)

        # Calculate the value of the game given these models
        value = 0
        total_prob = 0 # To check that all adds upp correctly
        for first_card in self.cardsAvailable[first_player]:
            prob_of_first_card = strat_results[0][first_card]
            chosen_responses = strat_results[1][first_card]
            prob_of_response = 1/len(chosen_responses)
            for response_card in chosen_responses:
                total_prob += prob_of_response * prob_of_first_card # For error checking
                value += prob_of_first_card*prob_of_response*currentRoundMatrix[first_card,response_card] 

        # We have 3 types of strategies: "f" for first, and "r" for response. 
        strat_types = ["f", "f"]
        strat_types[second_player] = "r"
        state_info = (value, [list(strat_results[first_player]), list(strat_results[second_player])], reducedMatrix, strat_types)

        # Error checking
        if abs(total_prob - 1) > margin:
            print(f"ERROR - INVALID TOTAL PROBABILITY ON SEQUENTIAL TURN: {total_prob}. State info: {state_info}")
        if value > 1+margin or value < 0-margin:
            print(f"ERROR - VALUE ON SEQUENTIAL TURN: {value}. State info: {state_info}")

        return state_info

    def getValue(self, knownSolutions, models, mirror_solutions = False, savePath = "temp_solution.txt", save_interval = 600, report_interval = 60, report_margin = 0.000001):
        # Returns the value of the current gamestate
        # Strategy can either be "Optimal" or an object with property get_strategy that takes a gamestate (and player #)
        # and returns a list of probabilities for each of that player's possible cards.

        #First,  check if the game is over (win/loss or tie)
        if self.gameWinner is not None:
            knownSolutions[self.get_game_str()] = (1 - self.gameWinner, "Endstate", "", ["N", "N"])
            return 1 - self.gameWinner
        elif len(self.cardsAvailable[0]) == 0:
            knownSolutions[self.get_game_str()] = (0.5, "Endstate", "", ["N", "N"])
            return 0.5

        # If the game is not over, we must make a matrix of all sub-games, and calculate value from it.
        currentRoundMatrix = np.zeros(shape=(self.game_size,self.game_size))
        # Start with a k by k matrix, and get the value for each corresponding sub-state.
        for p1_next in self.cardsAvailable[0]:
            for p2_next in self.cardsAvailable[1]:
                subGame = ratGame() #ratGame(self.get_game_str())
                # Copying manually should be faster than using the gamestring
                subGame.cardsAvailable = [self.cardsAvailable[0].copy(), self.cardsAvailable[1].copy()]
                subGame.wins = self.wins[:]
                subGame.generals = self.generals[:]
                subGame.spies = self.spies[:]
                subGame.holds = self.holds[:]

                subGame.advanceGameState([p1_next, p2_next])
                subGameStr = subGame.get_game_str()
                
                if subGameStr in knownSolutions:
                    # We may already know this state's value
                    currentRoundMatrix[p1_next][p2_next] = knownSolutions[subGameStr][0]
                    
                else: 
                    # Otherwise, we have to solve the sub-game before solving this game
                    subGameValue = subGame.getValue(knownSolutions, models, savePath=savePath, save_interval=save_interval, report_interval=report_interval)
                    
                    if subGameValue+report_margin < 0 or subGameValue-report_margin > 1:
                        print(f"Invalid value in position {self.get_game_str()}: {subGameValue}")

                    currentRoundMatrix[p1_next][p2_next] = subGameValue

        game_str = self.get_game_str()

        # Check if we've already computed this turn.
        # Note: It's still necessary that we calculated the up-stream values above, to traverse subsequent turns.
        reverse_str = helperFunctions.reverseGameState(game_str)
        if mirror_solutions and reverse_str in knownSolutions:
            reverse_solution = knownSolutions[reverse_str]
            val = 1 - reverse_solution[0]
            strats = [reverse_solution[1][1], reverse_solution[1][0]]
            val_matrix = 1 - np.transpose(reverse_solution[2])
            strat_types = [reverse_solution[3][1], reverse_solution[3][0]]
            gameResult = (val, strats, val_matrix, strat_types)
            knownSolutions[game_str] = gameResult
        else:
            reducedMatrix = currentRoundMatrix[list(self.cardsAvailable[0])]
            reducedMatrix = reducedMatrix[:, list(self.cardsAvailable[1])]

            if max(self.spies) == 0: # Check if a spy effect is active
                # Non-spy round - solve via simultaneous move (simplex method)

                gameResult = self.get_strats_and_value(currentRoundMatrix, reducedMatrix, models)

            else:
                # Spy round - sequential solve (minmax)
                gameResult = self.get_sequential_strats_and_value(currentRoundMatrix, reducedMatrix, models=models, first_player = self.spies[0]) # 0 = p1 first, 1 = p2 first

            val = gameResult[0]
            knownSolutions[game_str] = gameResult

        if len(self.cardsAvailable[0]) >= 7: # Status progress report on high-level turns
            print("Progress: finished solving turn 1 or 2 state " + game_str + " with value " + str(gameResult[0]))

        # Time-based progress reports and saving.
        global startTime, saveStartTime, overall_startTime, computed_states
        t = pc()
        if t - saveStartTime > save_interval:
            print(f"Auto-saving at {savePath}")
            helperFunctions.write_known_solutions(knownSolutions, savePath)
            saveStartTime = t
            
        if t - startTime > report_interval:
            states = len(knownSolutions)
            print(f"Computed {states - computed_states} states in {round(t-startTime)} seconds. Total: {states} states so far in {round(t-overall_startTime)} seconds.")
            startTime = t
            computed_states = states

        return val

# %% functions for command line interface
def print_solution(statename, knownSolutions):
    game = ratGame(statename)
    if not (game.gameWinner is None): # If there's a winner, print this instead
        print('!!!!!!!! WINNER: PLAYER %d !!!!!!!!'%(game.gameWinner + 1))
    else:
        print(create_solution_str(game ,knownSolutions[statename]))

def standardize_decimal(num):
    return str(round(float(num), 4)).ljust(6, " ")

def create_solution_str(game, tup):
    p1_cards_list = list(game.cardsAvailable[0])
    p2_cards_list = list(game.cardsAvailable[1])

    if tup[3][0] != "r": # Not a spy response
        p1_probs = [tup[1][0][i] for i in p1_cards_list] # reduced form
    else:
        p1_probs = [tup[1][0][i] for i in p2_cards_list]
    if tup[3][1] != "r": # Not a spy response
        p2_probs = [tup[1][1][i] for i in p2_cards_list] # reduced form
    else:
        p2_probs = [tup[1][1][i] for i in p1_cards_list]

    ans = "State id: %s"%(game.get_game_str())
    ans += '\n\nWins: %s    Holds: %s'%(", ".join(map(str,game.wins)), ", ".join(map(str,game.holds)))
    ans += ("\nGenerals used: " + ", ".join(map(str,game.generals)) + "    Spies used: "  + ", ".join(map(str,game.spies)))
    ans += ("\nP1 Win Prob: " + str(round(100*tup[0],5)) + "%")
    ans += ("\nP1 Cards:            " + ",      ".join(map(str,game.cardsAvailable[0])))
    if tup[3][0] != "r": # Not a spy response
        ans += ("\nP1 Optimal Strategy: " + ", ".join(map(standardize_decimal, p1_probs))) # ,np.round(tup[1][0],4))))
    if tup[3][1] == "r":
        ans += f"\nP2 Response Cards:   {', '.join(['/'.join([str(y) for y in x]).ljust(6, ' ') for x in p2_probs])}\n"
    ans += ("\nP2 Cards:            " + ",      ".join(map(str,game.cardsAvailable[1])))
    if tup[3][1] != "r": # Not a spy response
        ans += ("\nP2 Optimal Strategy: " + ', '.join(map(standardize_decimal, p2_probs))) 
    if tup[3][0] == "r":
        ans += f"\nP1 Response Cards:   {', '.join(['/'.join([str(y) for y in x]).ljust(6, ' ') for x in p1_probs])}\n"
    ans += "\n\n                          P2"
    
    ans += "\nP1       "# + ",       ".join(map(str,game.cardsAvailable[1])))
    

    if tup[3][0] != "r": # Not a spy response
        p1_prob_strings = ["(" + str(round(100*x)).rjust(3) + "%) " for x in p1_probs]
    else:
        p1_prob_strings = ["       "]*8
    if tup[3][1] != "r": # Not a spy response
        p2_prob_strings = ["(" + str(round(100*x)).rjust(3) + "%) " for x in p2_probs]
    else:
        p2_prob_strings = ["       "]*8

    for i in range(len(p2_cards_list)):
        ans += f" {p2_cards_list[i]} {p2_prob_strings[i]}"
        #ans += '%s (%s%%)  '%(str(p2_cards_list[i]), str(round(100*tup[1][1][i])).rjust(2))

    for i in range(len(p1_cards_list)):
        ans += f"\n{p1_cards_list[i]} {p1_prob_strings[i]} {'  | '.join(map(standardize_decimal,tup[2][i]))}"
        #ans += '\n%s (%s%%) %s' % (str(p1_cards_list[i]), str(round(100*tup[1][0][i])).rjust(2), " | ".join(map(standardize_decimal,tup[2][i])))

    #ans += ("\nCurrent Game Matrix: \n" + str(np.round(tup[2], 5)))
    return ans
        
def play_game(game, knownSolutions, players, models):
    state_str = game.get_game_str()
    solution = knownSolutions[state_str]
    game_size = len(game.cardsAvailable[0])

    plays = [None, None]
    play_order = [0,1]
    if game.spies[0]==1:
        play_order = [1,0]

    for player in play_order:
        p_type = players[player]
        if p_type == "Human":
            choices = game.cardsAvailable[player]
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
            if solution[3][player] == 'r': # Response, based on opponent's card
                opp_play = plays[play_order[0]]
                possibilities = solution[1][player][opp_play]
                print(f"Choosing randomly from equally valid cards {', '.join([str(x) for x in possibilities])}")
                plays[player] = np.random.choice(possibilities)
            else: # Either simultaneous-move or first move
                probs = np.array([solution[1][player][i] for i in range(8) if i in game.cardsAvailable[player]])
                probs = probs / sum(probs) # To fix rounding errors
                plays[player] = list(game.cardsAvailable[player])[np.random.choice(len(probs), 1, p = probs)[0]]
            print(f"AI Choice: {plays[player]}")

        if p_type == "Random":
            plays[player] = list(game.cardsAvailable[player])[np.random.choice(game_size, 1)[0]]
            print(f"Random Choice: {plays[player]}")
    
    print("\nPLAYING CARDS %d AND %d -----------------------------------------------\n"%(plays[0], plays[1]))
    game.advanceGameState(plays)
    if game.gameWinner == None:
        print("Press enter to continue, or enter 'n' to quit.")
        _ = input()
        if _ == "n":
            return
    else:
        print('!!!!!!!! WINNER: PLAYER %d !!!!!!!!'%(game.gameWinner + 1))
        return

    print_solution(game.get_game_str(), knownSolutions)
    play_game(game, knownSolutions, players)

    

# %%

def game_loop(knownSolutions, start_gamestr = 'p1-01234567-p2-01234567-w-00-g-00-s-00-h-00'):
    while(True):
        print("Select starting position string. Hit enter to start a new game.")
        response = input("Press enter to start a new game. Paste a gamestate string to start from a specific state.")

        if len(response) == 0:
            game = ratGame(start_gamestr)
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

def solve_game(savePath, start_gamestr = 'p1-01234567-p2-01234567-w-00-g-00-s-00-h-00', mirror_solutions = False,
               tempSavePath = "temp_solution.txt", loadFromTempSave = None, models= [models.simplexSolver(), models.simplexSolver()], save_interval= 600, report_interval = 60):
    # This will solve the game
    # temp_solution will save as a txt file periodically in case the solution process is interrupted
    # you may input already-solved turns such as temp_solution.txt with loadFromTempSave to resume progress.
    
    print(f"Solving the game with models: {type(models[0])} and {type(models[1])}. Solve times may vary. Reporting progress every {round(report_interval/60,1)} minutes, saving every {round(save_interval/60,1)} minutes to {tempSavePath}.")

    if loadFromTempSave is not None:
        knownSolutions = helperFunctions.read_known_solutions(loadFromTempSave)
        print(f"Loaded {len(knownSolutions)} solutions from {loadFromTempSave}")
    else:
        knownSolutions = {}

    testGame = ratGame(start_gamestr)
    testGame.getValue(knownSolutions, mirror_solutions=mirror_solutions, savePath = tempSavePath, models= models, save_interval= save_interval, report_interval = report_interval)

    helperFunctions.write_known_solutions(knownSolutions, savePath)

def default_console_start(path, start_gamestr = 'p1-01234567-p2-01234567-w-00-g-00-s-00-h-00'):
    # Try to read the file "knownSolutions.txt"
    # If we can't, create the solutions from scratch
    try:
        # Read solutions from file (5-10 seconds)
        print("Reading solutions...")
        knownSolutions = helperFunctions.read_known_solutions(path)
        print("Done reading solutions")

    except Exception as e:
        # If file couldn't be read, we'll make a new one
        print(e)
        print(f"Could not find solution. Solving game instead, will save to {path}.")
        ans = input("Enter 'y' to solve game.")
        if ans.lower() == 'y':
            solve_game(path, mirror_solutions = True,tempSavePath = "temp_solution.txt", loadFromTempSave = None, models= [models.simplexSolver(), models.simplexSolver()], save_interval= 600, report_interval = 60)
        else:
            return

    game_loop(knownSolutions, start_gamestr= start_gamestr)
# %%

path = "SolutionFiles/updatedOptimalSolution.txt"

if __name__ == "__main__":
    # These are the commands to solve game for each pair of models

    #solve_game(savePath = "SolutionFiles/updatedOptimalSolution.txt", save_interval = 300, loadFromTempSave="temp_solution.txt")
    #solve_game(savePath = "SolutionFiles/SimplexVsRandom.txt", models=[models.savedModel("SolutionFiles/updatedOptimalSolution.txt"), models.fullRandom()])
    #solve_game(savePath = "SolutionFiles/DefeatVsRandom.txt", models=[models.defeatStrategy(models.fullRandom()), models.fullRandom()])
    #solve_game(savePath = "SolutionFiles/SimplexVsDefeat.txt", models=[models.savedModel("SolutionFiles/updatedOptimalSolution.txt"), models.defeatStrategy(models.savedModel("SolutionFiles/updatedOptimalSolution.txt"))])
    #solve_game(savePath = "SolutionFiles/IntuitiveVsOptimal.txt", models=[models.intuitiveDistribution(), models.savedModel("SolutionFiles/updatedOptimalSolution.txt")])
    #solve_game(savePath = "SolutionFiles/IntuitiveVsDefeater.txt", models=[models.savedModel("SolutionFiles/IntuitiveVsOptimal.txt"), models.defeatStrategy(models.savedModel("SolutionFiles/IntuitiveVsOptimal.txt"))])
    #solve_game(savePath = "SolutionFiles/IntuitiveVsRandom.txt", models=[models.savedModel("SolutionFiles/IntuitiveVsOptimal.txt"), models.fullRandom()])
    #solve_game(savePath = "SolutionFiles/RandomNonSpyVsSimplex.txt", models=[models.randomNonSpy(), models.savedModel("SolutionFiles/updatedOptimalSolution.txt")])
    #solve_game(savePath = "SolutionFiles/RandomNonSpyVsDefeater.txt", models=[models.savedModel("SolutionFiles/RandomNonSpyVsSimplex.txt"), models.defeatStrategy(models.savedModel("SolutionFiles/RandomNonSpyVsSimplex.txt"))])
    #solve_game(savePath = "SolutionFiles/RandomNonSpyVsRandom.txt", models=[models.savedModel("SolutionFiles/RandomNonSpyVsSimplex.txt"), models.fullRandom()])
    #solve_game(savePath = "SolutionFiles/RandomNonSpyVsIntuitive.txt", models=[models.savedModel("SolutionFiles/RandomNonSpyVsSimplex.txt"), models.savedModel("SolutionFiles/IntuitiveVsOptimal.txt")])

    # Naive simplex
    #solve_game(savePath = "SolutionFiles/hypotheticalGeneralStart.txt", start_gamestr='p1-01234567-p2-01234567-w-00-g-10-s-00-h-00')
    #solve_game(savePath = "SolutionFiles/NaiveSimplexVsSimplex.txt", 
    #           models=[models.naive(models.simplexSolver(), "SolutionFiles/updatedOptimalSolution.txt", "SolutionFiles/hypotheticalGeneralStart.txt"), 
    #                   models.savedModel("SolutionFiles/updatedOptimalSolution.txt")])
    #solve_game(savePath = "SolutionFiles/NaiveSimplexVsDefeat.txt",
    #            models=[models.savedModel("SolutionFiles/NaiveSimplexVsSimplex.txt"), 
    #            models.defeatStrategy(models.savedModel("SolutionFiles/NaiveSimplexVsSimplex.txt"))])
    #solve_game(savePath = "SolutionFiles/NaiveSimplexVsRandom.txt", models=[models.savedModel("SolutionFiles/NaiveSimplexVsSimplex.txt"), models.fullRandom()])
    #solve_game(savePath = "SolutionFiles/NaiveSimplexVsIntuitive.txt", models=[models.savedModel("SolutionFiles/NaiveSimplexVsSimplex.txt"), models.savedModel("SolutionFiles/IntuitiveVsOptimal.txt")])
    #solve_game(savePath = "SolutionFiles/NaiveSimplexVsRandomNonSpy.txt", models=[models.savedModel("SolutionFiles/NaiveSimplexVsSimplex.txt"), models.savedModel("SolutionFiles/RandomNonSpyVsSimplex.txt")])

    # Naive Simplex
    #solve_game(savePath = "SolutionFiles/NaiveIntuitiveVsSimplex.txt", models=[
    #    models.naive(models.intuitiveDistribution(), "SolutionFiles/updatedOptimalSolution.txt", "SolutionFiles/hypotheticalGeneralStart.txt"), 
    #    models.savedModel("SolutionFiles/updatedOptimalSolution.txt")])
    #solve_game(savePath = "SolutionFiles/NaiveIntuitiveVsDefeat.txt",
    #            models=[models.savedModel("SolutionFiles/NaiveIntuitiveVsSimplex.txt"), 
    #            models.defeatStrategy(models.savedModel("SolutionFiles/NaiveIntuitiveVsSimplex.txt"))])
    #solve_game(savePath = "SolutionFiles/NaiveIntuitiveVsRandom.txt", models=[models.savedModel("SolutionFiles/NaiveIntuitiveVsSimplex.txt"), models.fullRandom()])
    #solve_game(savePath = "SolutionFiles/NaiveIntuitiveVsIntuitive.txt", models=[models.savedModel("SolutionFiles/NaiveIntuitiveVsSimplex.txt"), models.savedModel("SolutionFiles/IntuitiveVsOptimal.txt")])
    #solve_game(savePath = "SolutionFiles/NaiveIntuitiveVsRandomNonSpy.txt", models=[models.savedModel("SolutionFiles/NaiveIntuitiveVsSimplex.txt"), models.savedModel("SolutionFiles/RandomNonSpyVsSimplex.txt")])
    #solve_game(savePath = "SolutionFiles/NaiveIntuitiveVsNaiveSimplex.txt", models=[models.savedModel("SolutionFiles/NaiveIntuitiveVsSimplex.txt"), models.savedModel("SolutionFiles/NaiveSimplexVsSimplex.txt")])

    default_console_start(path)