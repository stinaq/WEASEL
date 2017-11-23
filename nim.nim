import math, times, random

const
  UltraWeasel = "methinks it is like a weasel"
  GenePool = "abcdefghijklmnopqrstuvwxyz "
  LitterSize = 100
  #MutationFactor = 0.04 # floating point randomness isnt supported on Windows
  MutationFactor = 4

proc randomGene(): char =
  return GenePool[random.random(GenePool.len)]

type
  TWeasel = object of TObject
    genes: string
    score: int

method calculateScore(w: var TWeasel) =
  w.score = 0
  for i, g in w.genes:
    if g == UltraWeasel[i]:
      w.score += 1

method createChild(w: TWeasel): TWeasel =
  result = TWeasel(genes: w.genes)
  for i, g in result.genes:
    if random.random(100) < MutationFactor:
      result.genes[i] = randomGene()
  result.calculateScore()

method createLitter(w: TWeasel): seq[TWeasel] =
  result = @[]
  for i in 1..LitterSize:
    result.add(w.createChild())

proc newWeasel(): TWeasel =
  var newGenes = ""
  for i in 1..UltraWeasel.len:
    newGenes.add($randomGene())
  result = TWeasel(genes: newGenes)
  result.calculateScore()

proc main(w: TWeasel, counter: int) =
  echo(counter, "\t", w.score, "\t", w.genes)
  if w.score == UltraWeasel.len:
    return
  let litter = w.createLitter()
  var bestOfLitter = litter[0]
  for i in 1..litter.len - 1:
    if litter[i].score > bestOfLitter.score:
      bestOfLitter = litter[i]

  main(bestOfLitter, counter + 1)

random.randomize(cast[int](times.epochTime()))
echo("Gen\tScore\tGenes")
echo("===\t=====\t=====")
main(newWeasel(), 1)
