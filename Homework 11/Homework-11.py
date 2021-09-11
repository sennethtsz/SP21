"""
In the videos, we saw the "diet problem". (The diet problem is one of the first large-scale optimization problems to be
studied in practice. Back in the 1930's and 40's, the Army wanted to meet the nutritional requirements of its soldiers
while minimizing the cost.) In this homework you get to solve a diet problem with real data. The data is given in the
file diet.xls.

1.	Formulate an optimization model (a linear program) to find the cheapest diet that satisfies the maximum and minimum
daily nutrition constraints, and solve it using PuLP.  Turn in your code and the solution. (The optimal solution should
be a diet of air-popped popcorn, poached eggs, oranges, raw iceberg lettuce, raw celery, and frozen broccoli. UGH!)
"""
from pulp import *
import pandas as pd

data = pd.read_excel("C:/Users/Admin/Desktop/MM/Homework 11/diet.xls",
                     sheet_name="Sheet1")
print(data.tail())
data = data[0:64]  # exclude bottom data

# intake restrictions
min_intake = [1500, 30, 20, 800, 130, 125, 60, 1000, 400, 700, 10]
max_intake = [2500, 240, 70, 2000, 450, 250, 100, 10000, 5000, 1500, 40]

# create foods dictionary
data = data.values.tolist()
foods = [x[0] for x in data]
food_dict = []
for i in range(3, 14):
    food_dict.append(dict([(x[0], float(x[i])) for x in data]))

# cost
cost = dict([(x[0], float(x[1])) for x in data])

# initiate optimization problem
prob = LpProblem('myProblem', LpMinimize)

# create variables
var_food = LpVariable.dicts("foods", foods, 0)
var_chosen = LpVariable.dicts("chosen", foods, 0, 1, LpBinary)
amount = LpVariable.dicts("amount", foods, 0)

# objective function - linear sum of costs (cost * food)
prob += lpSum([cost[food] * amount[food] for food in foods])

# constraints
for i in range(0, 10):  # nutrient constraint
    nutrients = pulp.lpSum([food_dict[i][food] * amount[food] for food in foods])
    prob += max_intake[i] >= nutrients
    prob += min_intake[i] <= nutrients

# optimize
prob.solve()
print('Solution:')
for var in prob.variables():
    if var.varValue > 0:  # if solution for food value is more than 0
        if str(var).find('chosen'):  # if food is chosen for solution
            print(str(var.varValue) + " units of " + str(var))
print("Total cost of food = $%.2f" % value(prob.objective))


"""
2.	Please add to your model the following constraints (which might require adding more variables) and solve the new 
model:
a.	If a food is selected, then a minimum of 1/10 serving must be chosen. (Hint: now you will need two variables for 
each food i: whether it is chosen, and how much is part of the diet. You'll also need to write a constraint to link 
them.)
"""
# initiate optimization for problem 1
prob1 = LpProblem('myProblem1', LpMinimize)

# objective function
prob1 += lpSum([cost[food] * amount[food] for food in foods])

# constraints
for i in range(0, 10):  # nutrient constraint
    nutrients = pulp.lpSum([food_dict[i][food] * amount[food] for food in foods])
    prob1 += max_intake[i] >= nutrients
    prob1 += min_intake[i] <= nutrients

for food in foods:  # minimum food unit constraint
    prob1 += var_food[food] >= 0.1 * var_chosen[food]

# optimize
prob1.solve()
print('Solution for Problem 1:')
for var in prob1.variables():
    if var.varValue > 0:  # if solution for food value is more than 0
        if str(var).find('chosen'):  # if food is chosen for solution
            print(str(var.varValue) + " units of " + str(var))
print("Total cost of food = $%.2f" % value(prob1.objective))


"""
b.	Many people dislike celery and frozen broccoli. So at most one, but not both, can be selected.
"""
# initiate optimization for problem 2
prob2 = LpProblem('myProblem2', LpMinimize)

# objective function
prob2 += lpSum([cost[food] * amount[food] for food in foods])

# constraints
for i in range(0, 10):  # nutrient constraint
    nutrients = pulp.lpSum([food_dict[i][food] * amount[food] for food in foods])
    prob2 += max_intake[i] >= nutrients
    prob2 += min_intake[i] <= nutrients

for food in foods:  # minimum food unit constraint
    prob2 += var_food[food] >= 0.1 * var_chosen[food]

prob2 += var_chosen['Frozen Broccoli'] + var_chosen['Celery, Raw'] <= 1  # at most one of the items constraint

# optimize
prob2.solve()
print('Solution for Problem 2:')
for var in prob2.variables():
    if var.varValue > 0:  # if solution for food value is more than 0
        if str(var).find('chosen'):  # if food is chosen for solution
            print(str(var.varValue) + " units of " + str(var))
print("Total cost of food = $%.2f" % value(prob2.objective))


"""
c.	To get day-to-day variety in protein, at least 3 kinds of meat/poultry/fish/eggs must be selected. [If something is 
ambiguous (e.g., should bean-and-bacon soup be considered meat?), just call it whatever you think is appropriate – I 
want you to learn how to write this type of constraint, but I don't really care whether we agree on how to classify 
foods!]
"""
# initiate optimization for problem 2
prob3 = LpProblem('myProblem3', LpMinimize)

# objective function
prob3 += lpSum([cost[food] * amount[food] for food in foods])

# constraints
for i in range(0, 10):  # nutrient constraint
    nutrients = pulp.lpSum([food_dict[i][food] * amount[food] for food in foods])
    prob3 += max_intake[i] >= nutrients
    prob3 += min_intake[i] <= nutrients

for food in foods:  # minimum food unit constraint
    prob3 += var_food[food] >= 0.1 * var_chosen[food]

prob3 += var_chosen['Frozen Broccoli'] + var_chosen['Celery, Raw'] <= 1  # at most one of the items constraint

prob3 += var_chosen['Ham,Sliced,Extralean'] + var_chosen['Frankfurter, Beef'] + var_chosen['Hamburger W/Toppings'] \
        + var_chosen['Hotdog, Plain'] + var_chosen['Scrambled Eggs'] + var_chosen['Kielbasa,Prk'] \
        + var_chosen['Poached Eggs'] + var_chosen['Pork'] + var_chosen['Roasted Chicken'] \
        + var_chosen['Sardines in Oil'] + var_chosen['White Tuna in Water'] \
        >= 3  # at least 3 kinds of meat/poultry/fish/eggs constraint

# optimize
prob3.solve()
print('Solution for Problem 3:')
for var in prob3.variables():
    if var.varValue > 0:  # if solution for food value is more than 0
        if str(var).find('chosen'):  # if food is chosen for solution
            print(str(var.varValue) + " units of " + str(var))
print("Total cost of food = $%.2f" % value(prob3.objective))