/**
 * To run:
 * `node javascript_Y-combinator.js`
 */
(function () {
  "use strict";

  var goal = "METHINKS IT IS LIKE A WEASEL";
  var size = 100;
  var rate = 0.04;
  var alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";

  // creates an array with `count` elements (empty strings) in a non-optimized way.
  var range = function (count) {
    return new Array(count).join(' ').split(' ');
  };

  // get a randomly chosen gene (character from the alphabet)
  var randGene = function () {
    return alphabet[Math.floor(Math.random() * alphabet.length)];
  };

  // calculate the score of a given weasel string.
  var getScore = function (weasel) {
    return weasel.split('').reduce(function (out, next, i) {
      return out + Number(next === goal[i]);
    }, 0);
  };

  var createChild = function (parent) {
    return parent.split('').map(function (c) {
      return Math.random() < rate ? randGene() : c;
    }).join('');
  };

  // Y combinator
  // see http://matt.might.net/articles/implementation-of-recursive-fixed-point-y-combinator-in-javascript-for-memoization/
  // use it so that we can recur without naming our functions.
  var Y = function (F) {
    return (function (x) {
      return F(function (y) {
        return (x(x))(y);
      });
    })(function (x) {
      return F(function (y) {
        return (x(x))(y);
      });
    });
  };

  var algo = Y(function (step) {
    return function (acc) {
      // `acc` is the accumulator.
      // it contains the whole history of the evolution.
      // so the last item is the latest state
      var curState = acc.slice(-1)[0];

      // this is our termination condition
      if (curState.bestWeasel === goal) {
        return acc;
      }

      // create a new litter from the current weasel.
      var litter = range(size).map(function () {
        return createChild(curState.bestWeasel);
      });

      // sort by the score, desc.
      var sorted = litter.sort(function (a, b) {
        return getScore(b) - getScore(a);
      });

      // the winner is the one at the start of the array
      var newBestWeasel = sorted[0];

      // do it again, but build upon the accumulator.
      return step(acc.concat({
        bestWeasel: newBestWeasel,
        score: getScore(newBestWeasel),
        generation: curState.generation + 1
      }));
    };
  });

  var initWeasel = range(goal.length).map(randGene).join('');
  var initialState = [{
    bestWeasel: initWeasel,
    score: getScore(initWeasel),
    generation: 0
  }];

  var result = algo(initialState);

  // print the whole evolution.
  result.forEach(function (state) {
    console.log(state.generation, state.bestWeasel, state.score);
  });

})();
