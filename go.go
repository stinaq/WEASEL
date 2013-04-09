package main

import (
	"fmt"
	"math/rand"
	"os"
	"time"
)

const (
	GOAL        = "METHINKS IT IS LIKE A WEASEL"
	LITTER_SIZE = 100
	RATE        = 0.04
)

var (
	genePool = []Gene("ABCDEFGHIJKLMNOPQRSTUVWXYZ ")
)

type Gene byte

func Mutate(g Gene) Gene {
	if rand.Float64() < RATE {
		return RandomGene()
	}
	return g
}

func RandomGene() Gene {
	return Gene(genePool[rand.Intn(len(genePool))])
}

func RandomGenes(length int) []Gene {
	result := make([]Gene, length)
	for i := range result {
		result[i] = RandomGene()
	}
	return result
}

type Weasel struct {
	fitness int
	genes   []Gene
}

func NewWeasel() *Weasel {
	return &Weasel{
		fitness: 0,
		genes:   RandomGenes(len(GOAL)),
	}
}

func (w *Weasel) CreateOffspring() *Weasel {
	ng := make([]Gene, len(w.genes))
	copy(ng, w.genes)
	for i := range ng {
		ng[i] = Mutate(ng[i])
	}
	return &Weasel{
		fitness: 0,
		genes:   ng,
	}
}

func (w *Weasel) CreateGeneration() Litter {
	weasels := make([]*Weasel, LITTER_SIZE)
	for i := range weasels {
		weasels[i] = w.CreateOffspring()
	}
	return Litter(weasels)
}

func (w *Weasel) CompareToGoal() {
	counter := 0
	goalGenes := []Gene(GOAL)
	for i := range goalGenes {
		if goalGenes[i] == w.genes[i] {
			counter++
		}
	}
	w.fitness = counter
}

type Litter []*Weasel

func (l Litter) BestMatch() *Weasel {
	bw := &Weasel{fitness: 0}
	for i := range l {
		if l[i].fitness > bw.fitness {
			bw = l[i]
		}
	}
	return bw
}

func (l Litter) FindFittest() *Weasel {
	for i := range l {
		l[i].CompareToGoal()
	}
	return l.BestMatch()
}

func evolution(fittest *Weasel, iters int) {
	nextFittest := fittest.CreateGeneration().FindFittest()

	// Convert back to bytes, so it can be cast to a string.
	bs := make([]byte, len(nextFittest.genes))
	for i := range bs {
		bs[i] = byte(nextFittest.genes[i])
	}

	curr := string(bs)
	fmt.Fprintf(os.Stdout, "%d\t%d\t%s\n", iters, nextFittest.fitness, curr)
	if curr == GOAL {
		return
	}
	evolution(nextFittest, (iters + 1))
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())
	evolution(NewWeasel(), 0)
}
