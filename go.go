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
	goal     = []byte(GOAL)
	goallen  = len(GOAL)
	genepool = []byte("ABCDEFGHIJKLMNOPQRSTUVWXYZ ")
	parent   = newWeasel()
)

func randomgene() byte {
	return genepool[rand.Intn(len(genepool))]
}

// TODO: Use channels?
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

type weasel struct {
	genes []byte
}

func newWeasel() *weasel {
	return &weasel{
		genes: randomgenes(goallen),
	}
}

func (w *weasel) procreate() *weasel {
	gl := len(w.genes)
	ng := make([]byte, gl, gl)
	for i := range w.genes {
		ng[i] = mutategene(w.genes[i])
	}
	return &weasel{
		genes: ng,
	}
}

func creategeneration(p *weasel) []*weasel {
	weasels := make([]*weasel, LITTER_SIZE, LITTER_SIZE)
	for i := range weasels {
		weasels[i] = p.procreate()
	}
	return weasels
}

func (w *weasel) comparetogoal() int {
	counter := 0
	for i := range goal {
		if goal[i] == w.genes[i] {
			counter++
		}
	}
	return counter
}

func bestmatch(m map[int]*weasel) int {
	bwk := 0
	for i, _ := range m {
		if i > bwk {
			bwk = i
		}
	}
	return bwk
}

func findfittest(weasels []*weasel) *weasel {
	fitmap := make(map[int]*weasel)
	for i := range weasels {
		fitmap[weasels[i].comparetogoal()] = weasels[i]
	}
	bm := bestmatch(fitmap)
	fmt.Fprintf(os.Stdout, "%d\t", bm)
	return fitmap[bm]
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())

	cont := true
	for cont {
		parent = findfittest(creategeneration(parent))
		curr := string(parent.genes)
		fmt.Fprintf(os.Stdout, "%s\n", curr)
		if curr == GOAL {
			cont = false
		}
	}
}
