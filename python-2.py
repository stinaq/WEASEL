import random, sys

def mutate(character, mutation_rate, alphabet):
    if mutation_rate >= random.random():
        return random.choice(alphabet)
    return character

def child(ancestor, mutation_rate, alphabet):
    return ''.join([mutate(character, mutation_rate, alphabet) for character in ancestor])

def children(ancestor, mutation_rate, generation_size, alphabet):
    return [child(ancestor, mutation_rate, alphabet) for i in range(generation_size)]

def fitness(child, target_string):
    return sum([1 for c in zip(child,target_string) if c[0] == c[1]])

def fittest(list_of_children, target_string):
    winner = list_of_children[0]
    for c in list_of_children:
        if fitness(c, target_string) > fitness(winner, target_string):
            winner = c
    return winner

def main():
    alphabet = 'abcdefghijklmnopqrstuvwxyz '
    random_start_string = ''.join([random.choice(alphabet) for i in range(28)])
    target_string = 'methinks it is like a weasel'
    mutation_rate = 0.05
    generation_size = 400

    current = random_start_string
    rounds = 0

    while(True):
        cs = children(current, mutation_rate, generation_size, alphabet)
        current = fittest(cs, target_string)

        print "Current string: %s" % current
        rounds += 1

        if current == target_string:
            print "Finished after %d rounds!" % rounds
            sys.exit(0)

if __name__ == '__main__':
    main()