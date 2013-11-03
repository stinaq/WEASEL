use std::rand;
use std::rand::Rng;
use std::str;
use std::vec;

static MUTATION_RATE: float = 0.04;
static GENERATION_SIZE: int = 100;
static TARGET: &'static [u8] = bytes!("methinks it is like a weasel");
static ALPHABET: &'static [u8] = bytes!("abcdefghijklmnopqrstuvwxyz ");

fn random_char<R: Rng> (rng: &mut R) -> u8 {
  rng.choose(ALPHABET)
}

fn random_string<R: Rng> (rng: &mut R) -> ~[u8] {
  vec::from_fn(TARGET.len(), |_| { random_char(rng) })
}

fn fitness (child: &[u8]) -> uint {
  child.iter().zip(TARGET.iter()).count(|(a,b)| { a == b })
}

fn mutate<R: Rng> (ind: &[u8], rng: &mut R) -> ~[u8] {
  ind.iter().map(|c| {
    let mutate: float = rng.gen();
    if mutate <= MUTATION_RATE { random_char(rng) }
    else { *c }
  }).to_owned_vec()
}

fn procreate<R: Rng> (progenitor: &[u8], rng: &mut R) -> ~[u8] {
  let mut winner = mutate(progenitor, rng);
  let mut winner_fitness = fitness(winner);
   
  for _ in range(1, GENERATION_SIZE) {
    let current = mutate(progenitor, rng);
    let current_fitness = fitness(current);
     
    if current_fitness > winner_fitness {
      winner = current;
      winner_fitness = current_fitness;
    }
  }
  winner
}

fn main () {
  println("Welcome to Dawkins' Weasel, Rust version.");
  let mut rng = rand::rng();
  let mut current = random_string(&mut rng);
  let mut counter = 0;
  loop {
    printfln!("Round %d: %s.", counter, str::from_utf8_slice(current));

    if TARGET == current {
      break;
    }

    counter = counter + 1;
    current = procreate(current, &mut rng);
  }

  printfln!("%d generations to reach target.", counter);
}
