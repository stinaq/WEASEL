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
	goal        = []byte(GOAL)
	goallen     = len(GOAL)
	genepool    = []byte("ABCDEFGHIJKLMNOPQRSTUVWXYZ ")
	genepoollen = len(genepool)
)

type weasel struct {
	fitness int
	genes   []byte
}

type litter []*weasel

func randomgene() byte {
	return genepool[rand.Intn(genepoollen)]
}

func randomgenes(length int) []byte {
	result := make([]byte, length, length)
	for i := range result {
		result[i] = randomgene()
	}
	return result
}

func mutategene(c byte) byte {
	if rand.Float64() < RATE {
		return randomgene()
	}
	return c
}

func newWeasel() *weasel {
	return &weasel{
		fitness: 0,
		genes:   randomgenes(goallen),
	}
}

func (w *weasel) procreate() *weasel {
	gl := len(w.genes)
	ng := make([]byte, gl, gl)
	for i := range w.genes {
		ng[i] = mutategene(w.genes[i])
	}
	return &weasel{
		fitness: 0,
		genes:   ng,
	}
}

func (p *weasel) creategeneration() litter {
	weasels := make([]*weasel, LITTER_SIZE, LITTER_SIZE)
	for i := range weasels {
		weasels[i] = p.procreate()
	}
	return litter(weasels)
}

func (w *weasel) comparetogoal() {
	counter := 0
	for i := range goal {
		if goal[i] == w.genes[i] {
			counter++
		}
	}
	w.fitness = counter
}

func (l litter) bestmatch() *weasel {
	bw := &weasel{fitness: 0}
	for i := range l {
		if l[i].fitness > bw.fitness {
			bw = l[i]
		}
	}
	return bw
}

func calculatefittest(l litter) {
	for i := range l {
		l[i].comparetogoal()
	}
}

func (weasels litter) findfittest() *weasel {
	calculatefittest(weasels)
	bm := weasels.bestmatch()
	return bm
}

func evolution(fittest *weasel, iters int) {
	nextFittest := fittest.creategeneration().findfittest()

	curr := string(nextFittest.genes)
	fmt.Fprintf(os.Stdout, "%d\t%d\t%s\n", iters, nextFittest.fitness, curr)
	if curr == GOAL {
		return
	}
	evolution(nextFittest, (iters + 1))
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())
	parent := newWeasel()

	evolution(parent, 0)
}
