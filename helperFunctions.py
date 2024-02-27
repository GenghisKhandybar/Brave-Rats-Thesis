import numpy as np

# Helper functions necessary for both models and solutionConsole
# Primarily string manipulation

def reverseGameState(gameState):
    game_str = gameState.split("-")
    return "p1-" + game_str[3] + "-p2-" + game_str[1] + \
        "-w-" + game_str[5][1] + game_str[5][0] + \
        "-g-" + game_str[7][1] + game_str[7][0] + \
        "-s-" + game_str[9][1] + game_str[9][0] + \
        "-h-" + game_str[11][1] + game_str[11][0]

def get_solution_save_string(solution):
    ans = ""
    ans += "v|" + str(solution[0])
    ans += "|s1|" + ",".join(map(str,solution[1][0]))
    ans += "|s2|" + ",".join(map(str,solution[1][1]))
    ans += "|m|"
    for line_i in range(len(solution[2])):
        ans += " $ " + str(line_i) + " $ "
        ans += ",".join(map(str,solution[2][line_i]))
    return ans

def get_solution_save_string_2(game, solution):
    # This version has probabilities for all 8 cards 
    
    ans = ""
    ans += "v|" + str(solution[0])
    long_solutions = []
    for p in range(2):
        s = ""
        sol_index = 0
        for i in range(8):
            if i in game.cardsAvailiable[p]:
                s += str(solution[1][p][sol_index])
                sol_index += 1
            if i != 7:
                s += ","
        long_solutions.append(s)

    ans += "|s1|" + long_solutions[0]
    ans += "|s2|" + long_solutions[1]
    ans += "|m|"
    for line_i in range(len(solution[2])):
        ans += " $ " + str(line_i) + " $ "
        ans += ",".join(map(str,solution[2][line_i]))
    return ans

def get_solution_from_string(solution_string):
    sections = solution_string.split("|")
    value = float(sections[1])
    s1 = np.array(sections[3].split(","), dtype = float)
    s2 = np.array(sections[5].split(","), dtype = float)
    m_strings = sections[7].split(" $ ")
    m = []
    for i in range(2, len(m_strings), 2):
        m.append(m_strings[i].split(","))
    return (value, [s1,s2], np.array(m, dtype = float))



"""
def write_known_values(knownValues, path):
    with open(path, 'w') as convert_file:
        convert_file.write(json.dumps(knownValues))
"""

def read_known_solutions(path):
    # Returns both known solutions and known values from knownSolutions.txt
    knownSolutions = {}
    knownValues = {}

    with open(path, 'r') as f:

        for line in f:
            key_val = line.split(":")
            key = key_val[0]

            val = get_solution_from_string(key_val[1])
            game_val = val[0] # Just game value (win probability)
            knownSolutions[key] = val
            knownValues[key] = game_val 
    return(knownValues, knownSolutions)