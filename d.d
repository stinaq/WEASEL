import std.stdio;
import std.random;
import std.algorithm;

const string uberGenes = "METHINKS IT IS LIKE A WEASEL";
const string genePool = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";
const float factor = 0.04;
const int litterSize = 100;

class Weasel {
  string genes;
  int f = -1;

  this (string genes) {
    this.genes = genes;
  }

  this (Weasel parent) {
    auto ng = new char[parent.genes.length];
    for(int i = 0; i < parent.genes.length; i++) {
      if (uniform(0.0f, 1.0f) < factor) ng[i] = randGene();
      else ng[i] = parent.genes[i];
    }
    this.genes = cast(string)ng;
  }

  public int fitness() {
    if (f != -1) return f;
    f = 0;
    for (int i = 0; i < genes.length; i++)
      if (genes[i] == uberGenes[i]) f++;
    return f;
  }

  public Weasel[] createOffspring() {
    auto children = new Weasel[litterSize];
    for (int i = 0; i < litterSize; i++) {
      children[i] = new Weasel(this);
    }
    return children;
  }

  public static Weasel findFittest(Weasel[] litter) {
    auto fittest = litter[0];
    foreach(w; litter) {
      if (w.fitness > fittest.fitness) {
        fittest = w;
      }
    }
    return fittest;
  }
}

static char randGene () {
  return genePool[uniform(0, genePool.length)];
}

static string randomGenes (int len) {
  char[] result = new char[len];
  for (int i = 0; i < len; i++) {
    result[i] = randGene();
  }
  return cast(string)result;
}

void evolution(Weasel w, int iters) {
  writefln("%d\t%d\t%s", iters, w.fitness, w.genes);
  auto m = w.createOffspring();
  auto fw = Weasel.findFittest(m);
  if (fw.fitness == uberGenes.length){
    writefln("%d\t%d\t%s", iters, fw.fitness, fw.genes);
    return;
  }
  evolution(fw, ++iters);
}

int main () {
  evolution(new Weasel(randomGenes(uberGenes.length)), 0);
  return 0;
}
