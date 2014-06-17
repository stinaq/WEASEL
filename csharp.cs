namespace Weasel
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    class Individual
    {
        public string Lookslike { get; private set; }

        public Individual(string lookslike)
        {
            Lookslike = lookslike;
        }
        public IEnumerable<Individual> ReproduceChildren(double mutationRate, int numberOfChildren, string alphabet)
        {
            return Enumerable.Range(0, numberOfChildren).Select(i => ReproduceChild(mutationRate, alphabet));
        }

        private Individual ReproduceChild(double mutationRate, string alphabet)
        {
            var newLookslike = Lookslike.Select(c => WeaselWorld.MutateCharacter(c, mutationRate, alphabet));
            return new Individual(new string(newLookslike.ToArray()));
        }
    }

    class WeaselWorld
    {
        private static readonly Random Random = new Random();
        private readonly string _alphabet;
        private readonly string _goal;

        public WeaselWorld(string alphabet, string goal)
        {
            _alphabet = alphabet;
            _goal = goal;
        }

        private Individual CreateFirstSire()
        {
            var firstSireLookslike =
                Enumerable.Range(0, _goal.Length)
                .Select(i => _alphabet[Random.Next(_alphabet.Length)]);
            return new Individual(new string(firstSireLookslike.ToArray()));
        }

        private int CalculateAlikeness(string child)
        {
            //Read more about Zip in LINQ
            return child
                .Zip(_goal, (childCharacter, goalCharacter) => childCharacter == goalCharacter)
                .Count(c => c);
        }

        internal static char MutateCharacter(char character, double mutationRate, string alphabet)
        {
            return Random.NextDouble() < mutationRate ? alphabet[Random.Next(alphabet.Length)] : character;
        }

        private Individual GetFittest(IEnumerable<Individual> children)
        {
            return children.OrderByDescending(c => CalculateAlikeness(c.Lookslike)).First();
        }

        public void CreateWeasel(double mutationRate, int numberOfChildren)
        {
            var sire = CreateFirstSire();
            Console.WriteLine(sire.Lookslike);

            var generations = 1;

            while (!sire.Lookslike.Equals(_goal))
            {
                var children = sire.ReproduceChildren(mutationRate, numberOfChildren, _alphabet);               
                sire = GetFittest(children);

                Console.WriteLine(generations++);
                Console.WriteLine(sire.Lookslike);
            }
            Console.WriteLine("Finished at generation " + generations);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var weaselWorld = new WeaselWorld("ABCDEFGHIJKLMNOPQRSTUVWXYZ ", "METHINKS IT IS LIKE A WEASEL");
            weaselWorld.CreateWeasel(0.04d, 100);
            Console.ReadKey();
        }
    }
}
