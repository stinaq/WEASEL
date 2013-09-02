MUTATION_RATE = 0.04
LITTER_SIZE = 100
GENE_POOL = "abcdefghijklmnopqrstuvwxyz ".split ''
UBER_WEASEL = "methinks it is like a weasel"

randomGene = () -> GENE_POOL[Math.floor(Math.random() * GENE_POOL.length)]

class Weasel
  constructor: (@genes) ->
    @fitness = 0

  calculateFitness: () ->
    @fitness = (@genes.filter (e, i) -> UBER_WEASEL[i] is e).length

  mutate: () ->
    @genes = for letter in @genes
      if (Math.random() < MUTATION_RATE) then randomGene()
      else letter
    @calculateFitness();
    return this;

  procreate: () ->
    num = 0
    while num++ < LITTER_SIZE
      new Weasel(@genes).mutate()

evolution = (nextWeasel, count) ->
  if nextWeasel.fitness is UBER_WEASEL.length
    console.log "Uber weasel found"
  else
    litter = nextWeasel.procreate()
      .sort((w1, w2) ->
        if w1.fitness < w2.fitness then 1
        else if w1.fitness > w2.fitness then -1
        else 0)
    bestWeasel = litter[0]
    console.log(bestWeasel.fitness, bestWeasel.genes.join(''), count)
    evolution bestWeasel, ++count

main = () ->
  startWeasel = new Weasel(for letter in UBER_WEASEL
    randomGene())
  evolution(startWeasel, 0)

main()