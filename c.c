// For strdup to work
#define _POSIX_C_SOURCE 200809L

/**
   Weasel implementaion
   to compile:
   [clang|gcc] -pipe -m64 -ansi -fPIC -g -O3 -fno-exceptions -fstack-protector \
     -fvisibility=hidden -W -Wall -Wno-unused-parameter -Wno-unused-function \
     -Wno-unused-label -Wpointer-arith -Wformat -Wreturn-type -Wsign-compare \
     -Wmultichar -Wformat-nonliteral -Winit-self -Wuninitialized \
     -Wno-deprecated -Wformat-security -Werror -std=c11 -c c.c

**/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>


const char* GENES = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";
const char* PERFECT_GENES = "METHINKS IT IS LIKE A WEASEL";
const int MUTATION_FACTOR = 4; // percent
const int LITTER_SIZE = 100;


typedef struct {
  char* genes;
} Weasel;


int
random_l(int limit) {
  /**
     return a random number between 0 and limit inclusive.
     No skew version
  **/
  int divisor = RAND_MAX/(limit+1);
  int result;

  do {
    result = rand() / divisor;
  } while (result > limit);

  return result;
}


char
random_gene() {
  return GENES[random_l(strlen(GENES) - 1)];
}


int
init_weasel(Weasel* w, char* parent_genes) {
  const int GENE_COUNT = strlen(PERFECT_GENES);
  char gs[GENE_COUNT + 1];
  if (parent_genes != NULL) {
    for (int i = 0; i < GENE_COUNT; i++) {
      if (random_l(100) < MUTATION_FACTOR) {
        gs[i] = random_gene();
      } else {
        gs[i] = parent_genes[i];
      }
    }
  } else {
    for (int i = 0; i < GENE_COUNT; i++) {
      gs[i] = random_gene();
    }
  }

  gs[GENE_COUNT] = '\0';
  w->genes = strdup(gs);
  return 0;
}


int
weasel_procreate(Weasel* parent, Weasel* children) {
  for (int i = 0; i < LITTER_SIZE; i++) {
    init_weasel(&children[i], parent->genes);
  }

  return 0;
}


unsigned long
get_score(Weasel* w) {
  const int GENE_COUNT = strlen(PERFECT_GENES);
  int score = 0;
  for (int i = 0; i < GENE_COUNT; i++) {
    if (w->genes[i] == PERFECT_GENES[i]) { score++; }
  }

  return (unsigned long)score;
}


int
cmp_weasels(const void* a, const void* b) {
  /**
     finds out which weasel is the better suited.
     negative result means weasel A was better
     0 means they are equally good
     positive means weasel B was better

     parameters are (void *)s since the qsort
     requires them to be such.
  **/
  Weasel* wa = (Weasel*)a; Weasel* wb = (Weasel*)b;
  int a_score = get_score(wa);
  int b_score = get_score(wb);

  return b_score - a_score;
}


int
main(void) {
  // seed the random
  srand(time(NULL));
  Weasel* w = (Weasel*) malloc(sizeof(Weasel));
  Weasel* children = malloc(sizeof(Weasel) * LITTER_SIZE);
  init_weasel(w, NULL);

  int counter = 0;
  while (get_score(w) != strlen(PERFECT_GENES)) {
    printf("Genes: [%s], Score: %lu, Iteration: %i\n", w->genes, get_score(w), ++counter);
    weasel_procreate(w, children);
    qsort(children, LITTER_SIZE, sizeof(Weasel), cmp_weasels);
    *w = children[0];

    // free the memory used by the inferior weasels.
    for (int i = 1; i < LITTER_SIZE; i++) {
      free(children[i].genes);
    }
  }

  printf("Winner: %s, after %i generations!\n", w->genes, counter);

  return 0;
}
