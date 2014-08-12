Option Explicit

Private Const MUTATE_CHANCE = 0.04
Private Const NUM_CHILDREN = 100
Private Const TARGET = "METHINKS IT IS LIKE A WEASEL"
Private Const CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "

Public Sub WeaselProgram()

    Randomize

    'Declare variables
    Dim current As String
    Dim children(NUM_CHILDREN) As String
    Dim tries As Long
    
    Dim score As Integer
    Dim best_score As Integer
    Dim best_child As Integer
    
    Dim i As Integer
    Dim c As Integer

    'initialize current generation to completely random string
    current = ""
    For i = 1 To Len(TARGET)
        current = current & GetRandomChar()
    Next
    Debug.Print "0: " & current

    ' main loop
    tries = 0
    While current <> TARGET
        
        'reproduce: clone current into children and mutate their characters
        For c = LBound(children) To UBound(children)
            children(c) = current
            For i = 1 To Len(children(c))
                If Rnd < MUTATE_CHANCE Then
                    children(c) = Left(children(c), i - 1) & GetRandomChar() & Mid(children(c), i + 1)
                End If
            Next
        Next
                
        'get the closes match and promote to current
        best_score = -1
        For c = LBound(children) To UBound(children)
            score = CompareWeasels(children(c), TARGET)
            If score > best_score Then
                best_score = score
                best_child = c
            End If
        Next
        current = children(best_child)
        
        'print current status
        tries = tries + 1
        Debug.Print tries & ": " & current
        DoEvents
    
    Wend
    
    Debug.Print "Completed in " & tries & " generations!"

End Sub

Private Function GetRandomChar()
    'Returns a random character from the allowed list characters

    Dim c As Integer
    c = Int((Len(CHARS) - 1 + 1) * Rnd + 1)     'Get a random number in an interval: Int((upperbound - lowerbound + 1) * Rnd + lowerbound)
    GetRandomChar = Mid(CHARS, c, 1)

End Function

Private Function CompareWeasels(s1 As String, s2 As String) As Integer
    'Compares two strings and returns the number of matching characters
    
    CompareWeasels = 0
    
    Dim i As Integer
    For i = 1 To Len(s1)
        If Mid(s1, i, 1) = Mid(s2, i, 1) Then
            CompareWeasels = CompareWeasels + 1
        End If
    Next
    
End Function

