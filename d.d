import std.stdio;
import std.random;
import std.algorithm;

version(unittest) {
  import std.regex;
}

int main () {
  Genetics.evolution();
  return 0;
}

class Genetics {
private:
  const string genePool = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";

  static void evolution(Weasel w, int iters) {
    writefln("%d\t%d\t%s", iters, w.fitness, w.genes);
    auto m = w.createOffspring();
    auto fw = Weasel.findFittest(m);
    if (fw.fitness == uberGenes.length){
      writefln("%d\t%d\t%s", iters, fw.fitness, fw.genes);
      return;
    }
    evolution(fw, ++iters);
  }

public:
  const string uberGenes = "METHINKS IT IS LIKE A WEASEL";
  const float factor = 0.04;
  const int litterSize = 100;

  static char randomGene () {
    return genePool[uniform(0, genePool.length)];
  }

  static string randomGenes (int len) {
    char[] result = new char[len];
    for (int i = 0; i < len; i++) {
      result[i] = randomGene();
    }
    return cast(string)result;
  }

  static evolution() {
    auto w = new Weasel(randomGenes(uberGenes.length));
    evolution(w, 0);
  }

  unittest {
    enum n = 100;
    enum pg = r"[A-Z]*";
    auto g = randomGenes(n);
    assert(!match(g, pg).empty);
    assert(g.length == n);
  }
}

class Weasel {
private:
  string genes;
  int f = -1;

  this (Weasel parent) {
    auto ng = new char[parent.genes.length];
    for(int i = 0; i < parent.genes.length; i++) {
      if (uniform(0.0f, 1.0f) < Genetics.factor) ng[i] = Genetics.randomGene();
      else ng[i] = parent.genes[i];
    }
    this.genes = cast(string)ng;
  }

public:
  this (string genes) {
    this.genes = genes;
  }

  int fitness() {
    if (f != -1) return f;
    f = 0;
    for (int i = 0; i < genes.length; i++)
      if (genes[i] == Genetics.uberGenes[i]) f++;
    return f;
  }

  Weasel[] createOffspring() {
    auto children = new Weasel[Genetics.litterSize];
    for (int i = 0; i < Genetics.litterSize; i++) {
      children[i] = new Weasel(this);
    }
    return children;
  }

  static Weasel findFittest(Weasel[] litter) {
    auto fittest = litter[0];
    foreach(w; litter) {
      if (w.fitness > fittest.fitness) {
        fittest = w;
      }
    }
    return fittest;
  }

  unittest {
    auto g = Genetics.randomGenes(Genetics.uberGenes.length);
    auto w = new Weasel(g);
    assert(w.genes == g);
    assert(w.f == -1);
    assert(w.fitness != -1);
    assert(w.f != -1);
    auto wl = w.createOffspring();
    assert(wl.length == Genetics.litterSize);
  }
}
