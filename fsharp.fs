open System

let goal = List.ofArray("methinks it is like a weasel".ToCharArray())
let genepool = "abcdefghijklmnopqrstuvwxyz "
let factor = 0.04
let litterSize = 100
let rng = new Random()

/// Get a random 'gene, as in a random character from the 'genepool'.
let randomGene = (fun _ -> genepool.[rng.Next(genepool.Length)])

/// Mutates a 'gene' if the magic powers of pseudo-randomness allows it to be so.
let mutateGene c =
    if rng.NextDouble() < factor then randomGene()
    else c

/// Takes a 'weasel' and generates <litterSize> new 'weasels'
/// and perhaps mutating some genes in the offspring in the processs
let createLitter fromWeasel =
    [for a in [0 .. litterSize] do
        yield List.map mutateGene fromWeasel]

/// Associates a score to each 'weasel' in this generation via a comparison to the goal.
let naturalSelection =
    let compareToGoal (cl:char list) =
        let c = List.zip cl goal
                |> List.map (fun (x, y) -> x = y)
                |> List.filter (fun x -> x)
        c.Length
    fun litter ->
        [for w in litter do
            yield (compareToGoal(w), w)]

/// Finds the fittest of the 'weasels' and returns it.
let getFittest selection =
    List.sort selection
    |> (fun x -> List.nth x (x.Length - 1)) // get the last element, since it's sorted ascending

let main() =
    // Create the starting weasel. S/he has all random genes.
    let startWeasel = [for a in [1 .. goal.Length] do
                          yield randomGene()]

    // Recursively apply the process of evolution
    let rec evolution curr iter =
        let fittestOfProgeny = curr
                               |> createLitter
                               |> naturalSelection
                               |> getFittest

        printf "%d\t%d\t" iter (fst(fittestOfProgeny))
        printf "%s\n" (String.Concat(Array.ofList (snd(fittestOfProgeny))))

        match snd(fittestOfProgeny) with
        | a when a = goal -> () // if the fittest of the litter is the goal, then return
        | _ -> evolution (snd(fittestOfProgeny)) (iter + 1) // otherwise, recur

    // begin the evolution
    evolution startWeasel 0

main()
