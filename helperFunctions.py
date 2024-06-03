import numpy as np

# Helper functions necessary for both models.py and solutionConsole.py
# Primarily string manipulation and reading/writing strategy profile files.

def reverseGameState(gameState):
    game_str = gameState.split("-")
    return "p1-" + game_str[3] + "-p2-" + game_str[1] + \
        "-w-" + game_str[5][1] + game_str[5][0] + \
        "-g-" + game_str[7][1] + game_str[7][0] + \
        "-s-" + game_str[9][1] + game_str[9][0] + \
        "-h-" + game_str[11][1] + game_str[11][0]

def get_solution_save_string(cardsAvailiable, solution, precision = 6):
    # solution is a tuple with 4 items: value, strategies, matrix, strategy types
    
    ans = "v|" + str(round(solution[0], precision))

    # Convert solutions from reduced form (only probabilities for active cards)
    # to long form (1 )
    long_solutions = []
    for p in range(2):
        s = ""
        for i in range(8):
            if solution[3][p] != "r":
                if i in cardsAvailiable[p]:
                    s += str(round(solution[1][p][i], precision))
            else: # for "r" (response) turns
                if i in cardsAvailiable[abs(1-p)]:
                    s += "/".join([str(x) for x in solution[1][p][i]]) 
                    # Instead of probabilities, this is a list of viable cards for any possible opponent's play, separated by "/".
            if i != 7:
                s += ","
        long_solutions.append(s)

    ans += "|s1" + solution[3][0] + "|" + long_solutions[0]
    ans += "|s2" + solution[3][1] + "|" + long_solutions[1]
    ans += "|m|"
    for line_i in range(len(solution[2])):
        ans += " $ " + str(line_i) + " $ "
        ans += ",".join(map(lambda x: str(round(x, precision)),solution[2][line_i]))
    return ans

def get_solution_from_string(solution_string):
    sections = solution_string.split("|")
    value = float(sections[1]) # Get position value
    strat_types = [sections[2][2], sections[4][2]] # 3rd character of the strategy space is the strategy type. Ex. "s1s" = standard
    raw_strats = [sections[3], sections[5]]
    strats = []
    for player in range(2):
        if strat_types[player] != "r":
            # Each item is a single float for a probability
            # 0 if not an option
            s = np.array([float(x) if x != "" else 0 for x in raw_strats[player].split(",") ]) 
        else: # Each item is a list of integers for cards delimited by "/"
            s = []
            for x in raw_strats[player].split(","):
                if x != "":
                    s.append([int(y) for y in x.split("/")]) 
                else:
                    s.append([])
        strats.append(s)

    # Re-construct values matrix
    m_strings = sections[7].split(" $ ")
    m = []
    for i in range(2, len(m_strings), 2):
        m.append(m_strings[i].split(","))
    return (value, strats, np.array(m, dtype = float), strat_types)

def read_known_solutions(path):
    # Returns both known solutions and known values from knownSolutions.txt
    knownSolutions = {}

    with open(path, 'r') as f:

        for line in f:
            if "None" in line: # Temporary - removing wrong terms
                continue
            key_val = line.split(":")
            key = key_val[0]

            val = get_solution_from_string(key_val[1])
            knownSolutions[key] = val

    return knownSolutions

def write_known_solutions(knownSolutions, path):
    with open(path, 'w') as f:
        for key, value in knownSolutions.items():
            if value[1] != "Endstate":
                game_str = key.split("-")
                cardsAvailiable =[set(map(int, list(game_str[1]))), set(map(int, list(game_str[3])))]
                f.write('%s:%s\n' % (key, get_solution_save_string(cardsAvailiable, value)))

def reduceProbabilities(probs, cards):
    # Changes a list of probabilities from 8 long to n-cards long
    return np.array([probs[i] for i in range(8) if i in cards])

def expandProbabilities(probs, cards):
    strat = np.zeros(shape = 8)
    reduced_index = 0 # index in reduced-grid strategy
    for i in range(8):
        if i in cards:
            strat[i] = probs[reduced_index]
            reduced_index += 1
    return strat

def reduceIndexes(card_list, cardsAvailable):
    # Turns a list of cards into a list of indexes (8 long -> n cards long)
    cardsAvailable = list(cardsAvailable)
    return [cardsAvailable.index(x) for x in card_list]

def expandIndexes(index_list, cardsAvailable):
    # Turns a list of indexes into a list of cards (n cards long -> 8 long)
    cardsAvailable = list(cardsAvailable)
    return [cardsAvailable[x] for x in index_list]
