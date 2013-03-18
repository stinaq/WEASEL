"use strict";

var goal = "METHINKS IT IS LIKE A WEASEL",
    size = 100,
    rate = 0.04,
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";

var randomCharacter = function () {
  return alphabet.charAt(Math.floor(Math.random() * alphabet.length));
};

var randomString = function () {
  var result = "";
  for (var i = 0; i < goal.length; i++) {
    result += randomCharacter();
  }
  return result;
};

var mutate = function (current) {
  var mutated = "";
  for (var i = 0; i < current.length; i++) {
    if (Math.random() < rate) {
      mutated += randomCharacter();
    } else {
      mutated += current[i];
    }
  }
  return mutated;
};

var procreate = function (current) {
  var children = [];
  for (var i = 0; i < size; i++) {
    children.push(mutate(current));
  }
  return children;
};

var fitness = function (ind) {
  var count = 0;
  for (var i = 0; i < ind.length; i++) {
    if (goal[i] === ind[i]) {
      count++;
    }
  }
  return count;
};

var findFittest = function (children) {
  var fittest = children[0];
  for (var i = 0; i < children.length; i++) {
    if (fitness(children[i]) > fitness(fittest)) {
      fittest = children[i];
    }
  }
  return fittest;
};

var main = function () {
  var current = randomString(),
      children,
      counter = 0;
  while (current !== goal) {
    children = procreate(current);
    current = findFittest(children);

    console.log("Current string: " + current);
    counter++;
  }
  console.log("It took " + counter + " generations.")
};

if (require.main === module) {
  main();
}