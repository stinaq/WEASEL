import random


#returns either the same letter, or a random other, depending on mutation rate
def random_mutation(character, mutation_rate):
  if random.randint(0,mutation_rate) == mutation_rate:
    return random.choice('ABCDEFGHIJKLMNOPQRSTUVWXYZ ')
  return character

#take a parent string and make a child from it. How similar it is is dependent on mutation rate
def make_child(parent, mutation_rate):
  child = ""
  for character in parent:
    child = child + random_mutation(character, mutation_rate)
  return child


#decides how 'fit' the child is, how similar it is to the goal
def fitness(current, goal):
  i = 0
  points = 0
  for s in current:
    if s == goal[i]:
      points = points + 1
    i = i + 1
  return points

#takes a parent and makes a number of childen, returns the fittest one
def make_best_child(parent, goal, mutation_rate, number_of_children):
  best_fitness = -1
  best_child = ""
  for child in range(0, number_of_children):
    current_child = make_child(parent, mutation_rate)
    current_fitness = fitness(current_child, goal)
    if best_fitness < current_fitness:
      best_child = current_child
      best_fitness = current_fitness
  return best_child

#sets the goal string, creates random start string, makes generations of children to find one that looks like the goal
def methinks(mutation_rate, number_of_children):
  goal = "METHINKS IT IS LIKE A WEASEL"
  goal_fitness = fitness(goal, goal)

  current = ""
  for i in range(0, len(goal)):
    current = current + random.choice('ABCDEFGHIJKLMNOPQRSTUVWXYZ ')
  current_fitness = fitness(current, goal)
  print current

  generation_count = 1
  while(goal_fitness != current_fitness):
    current_fitness = fitness(current, goal)
    current = make_best_child(current, goal, mutation_rate, number_of_children)
    generation_count = generation_count + 1
    print current
  print "finished at generation"
  print generation_count



def main():
  mutation_rate = 100
  number_of_children = 400

  methinks(mutation_rate, number_of_children)

if __name__ == '__main__':
  main()


  
  
