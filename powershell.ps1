<#
.Synopsis
   Short description
.DESCRIPTION
   All the calculations are done with string arrays. Only for outputing they are joined
.EXAMPLE
   Example of how to use this cmdlet
.EXAMPLE
   Another example of how to use this cmdlet
#>
$goal = "METHINKS IT IS LIKE A WEASEL"
$alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ".ToCharArray()
$mutationRate = 0.04d
$generations = 0
$mutationRandom = new-object Random
$numberOfChildren = 100
 
function New-RandomCharacter {
    $alphabet[$mutationRandom.Next($alphabet.Length)]
}
 
function New-Sire {
    (1..$goal.Length | % { New-RandomCharacter }) -join ""
}
 
function New-Mutation {
    [CmdletBinding()]
    Param ([Parameter(Mandatory=$true)]$Character)
    if ($mutationRandom.NextDouble() -lt $mutationRate) { New-RandomCharacter } else { $Character }
}
 
function New-Child {
    [CmdletBinding()]
    Param([Parameter(Mandatory=$true)][string]$Parent)
    #write-host "new-child parent: $parent"
    ($Parent[0..$Parent.Length] | % { New-Mutation -Character $_ }) -join ""
}
 
function New-Children {
    [CmdletBinding()]
    Param([Parameter(Mandatory=$true)][string]$Parent)
    1..$numberOfChildren | % { New-Child -Parent $Parent }
}
 
function Get-Alikeness {
    [CmdletBinding()]
    Param([Parameter(Mandatory=$true)]$Child)
    (0..($goal.Length-1) | ? { $goal[$_] -eq $Child[$_] }).Length
}
 
function Get-Fittest {
    [CmdletBinding()]
    Param([Parameter(Mandatory=$true)]$Children)
    ($children | Sort -Property @{Expression={ Get-Alikeness -Child $_ }} | Select -Last 1) -join ""
}
 
function New-EvolutionGeneration {
    [CmdletBinding()]
    Param([string]$Parent=$(New-Sire))
 
    $generations++
    
    $children = New-Children -Parent $Parent
    $fittestChild = Get-Fittest -Children $children
    Write "Generations: $generations`t`tWeasel: $fittestChild"
    if ($fittestChild -ne $goal) {
        New-EvolutionGeneration -Parent $fittestChild
    }
}
New-EvolutionGeneration