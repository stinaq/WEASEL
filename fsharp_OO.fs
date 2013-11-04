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

type Weasel private (genes:char list) =

    member this.Genes = genes

    member this.Score =
        (List.zip genes goal
        |> List.map (fun (x, y) -> x = y)
        |> List.filter (fun x -> x)).Length

    new (parent:Weasel) = Weasel(List.map mutateGene parent.Genes)
    
    new () = Weasel([for a in [1 .. goal.Length] do
                     yield randomGene()])

    member this.GiveBirth =
        [for a in [1 .. litterSize] do
            yield Weasel(List.map mutateGene this.Genes)]

    static member FindFittest (lst:Weasel list) =
        let sorted = List.sortBy (fun (w:Weasel) -> -w.Score) lst
        List.nth sorted 0


let main() =
    // Recursively apply the process of evolution
    let rec evolution (curr:Weasel, iter:int) =
        printf "%d\t%d\t" iter curr.Score
        printf "%s\n" (String.Concat(curr.Genes))

        let fittest = Weasel.FindFittest(curr.GiveBirth)

        match fittest with
        | f when f.Score = goal.Length -> (fittest, (iter + 1))
        | _ -> evolution (fittest, (iter + 1))

    // begin the evolution
    evolution (new Weasel(), 0)

let winner = main()
printf "Winner: %s\nIt took %d generations" (String.Concat((fst(winner)).Genes)) (snd(winner))
Console.ReadKey() |> ignore
