# VBA CODES

******************************************************************************

Sub GetString()
'Updateby Extendoffice
    Dim xStr As String
    Dim yStr As String
    Dim FRow As Long
    Dim xScreen As Boolean
    xScreen = Application.ScreenUpdating
    Application.ScreenUpdating = False
    xStr = Application.InputBox("Enter text to permute:", "Kutools for Excel", , , , , , 2)
    yStr = Application.InputBox("Enter target value:", "Kutools for Excel", , , , , , 2)
    If Len(xStr) < 2 Then Exit Sub
    If Len(xStr) >= 8 Then
        MsgBox "Too many permutations!", vbInformation, "Kutools for Excel"
        Exit Sub
    Else
        ActiveSheet.Columns(1).Clear
        FRow = 1
        Call GetPermutation("", xStr, FRow, yStr)
    End If
    Application.ScreenUpdating = xScreen
End Sub
Sub GetPermutation(Str1 As String, Str2 As String, ByRef xRow As Long, yStr As String)
    Dim i As Integer, xLen As Integer
    xLen = Len(Str2)
    If xLen < 2 Then
        Range("A" & xRow) = Str1 & Str2
        Range("B" & xRow) = CInt(Left(Str1, 3))
        Range("C" & xRow) = CInt(Right(Str1, 2) & Str2)
        Range("D" & xRow) = CInt(Left(Str1, 3)) - CInt(Right(Str1, 2) & Str2)
      
        If (CInt(Left(Str1, 3)) - CInt(Right(Str1, 2) & Str2)) = CInt(yStr) Then
            Range("E" & xRow) = "TRUE"
            Range("F" & 1) = Str1 & Str2
            
        Else
            Range("E" & xRow) = "FALSE"
        
        
        End If
        xRow = xRow + 1
        
    Else
        For i = 1 To xLen
            Call GetPermutation(Str1 + Mid(Str2, i, 1), Left(Str2, i - 1) + Right(Str2, xLen - i), xRow, yStr)
        Next
    End If
End Sub


******************************************************************************

Sub Smallest()
'Updateby Extendoffice
    Dim xStr As String
    Dim yStr As String
    Dim FRow As Long
    Dim xScreen As Boolean
    Dim min As Integer
    min = 999
    xScreen = Application.ScreenUpdating
    Application.ScreenUpdating = False
    xStr = Application.InputBox("Enter text to permute:", "Kutools for Excel", , , , , , 2)
    yStr = Application.InputBox("Enter target value:", "Kutools for Excel", , , , , , 2)
    If Len(xStr) < 2 Then Exit Sub
    If Len(xStr) >= 8 Then
        MsgBox "Too many permutations!", vbInformation, "Kutools for Excel"
        Exit Sub
    Else
        ActiveSheet.Columns(1).Clear
        ActiveSheet.Columns(2).Clear
        ActiveSheet.Columns(3).Clear
        ActiveSheet.Columns(4).Clear
        ActiveSheet.Columns(5).Clear
        ActiveSheet.Columns(6).Clear
        ActiveSheet.Columns(7).Clear
        ActiveSheet.Columns(8).Clear
        ActiveSheet.Columns(9).Clear
        
        FRow = 1
        Call GetPermutation("", xStr, FRow, yStr, min)
    End If
    Application.ScreenUpdating = xScreen
End Sub
Sub GetPermutation(Str1 As String, Str2 As String, ByRef xRow As Long, yStr As String, min As Integer)
    Dim i As Integer, xLen As Integer
    
    Dim min_comb As String
    xLen = Len(Str2)
    If xLen < 2 Then
        Range("A" & xRow) = Str1 & Str2
        Range("B" & xRow) = CInt(Left(Str1, 3))
        Range("C" & xRow) = CInt(Right(Str1, 2) & Str2)
        Range("D" & xRow) = CInt(Left(Str1, 3)) - CInt(Right(Str1, 2) & Str2)
      
        If (CInt(Left(Str1, 3)) - CInt(Right(Str1, 2) & Str2)) = CInt(yStr) Then
            Range("E" & xRow) = "TRUE"
            Range("F" & 1) = Str1 & Str2
            
        Else
            Range("E" & xRow) = "FALSE"
        
        
        
        End If
        
        If Abs(yStr - Abs(CInt(Left(Str1, 3)) - CInt(Right(Str1, 2) & Str2))) < min Then
        
        min = Abs(yStr - Abs(CInt(Left(Str1, 3)) - CInt(Right(Str1, 2) & Str2)))
        min_comb = Str1 & Str2
        Range("H" & 1) = min
        Range("I" & 1) = min_comb
        
        End If
        
        
        

        xRow = xRow + 1
        
    Else
        
        For i = 1 To xLen
            Call GetPermutation(Str1 + Mid(Str2, i, 1), Left(Str2, i - 1) + Right(Str2, xLen - i), xRow, yStr, min)
        Next
    End If
    
   
End Sub


******************************************************************************

Sub Biggest()
'Updateby Extendoffice
    Dim xStr As String
    Dim yStr As String
    Dim FRow As Long
    Dim xScreen As Boolean
    Dim max As Integer
    max = 0
    xScreen = Application.ScreenUpdating
    Application.ScreenUpdating = False
    xStr = Application.InputBox("Enter text to permute:", "Kutools for Excel", , , , , , 2)
    yStr = Application.InputBox("Enter target value:", "Kutools for Excel", , , , , , 2)
    If Len(xStr) < 2 Then Exit Sub
    If Len(xStr) >= 8 Then
        MsgBox "Too many permutations!", vbInformation, "Kutools for Excel"
        Exit Sub
    Else
        ActiveSheet.Columns(1).Clear
        ActiveSheet.Columns(2).Clear
        ActiveSheet.Columns(3).Clear
        ActiveSheet.Columns(4).Clear
        ActiveSheet.Columns(5).Clear
        ActiveSheet.Columns(6).Clear
        ActiveSheet.Columns(7).Clear
        ActiveSheet.Columns(8).Clear
        ActiveSheet.Columns(9).Clear
        
        FRow = 1
        Call GetPermutation("", xStr, FRow, yStr, max)
    End If
    Application.ScreenUpdating = xScreen
End Sub
Sub GetPermutation(Str1 As String, Str2 As String, ByRef xRow As Long, yStr As String, max As Integer)
    Dim i As Integer, xLen As Integer
    
    Dim min_comb As String
    xLen = Len(Str2)
    If xLen < 2 Then
        Range("A" & xRow) = Str1 & Str2
        Range("B" & xRow) = CInt(Left(Str1, 3))
        Range("C" & xRow) = CInt(Right(Str1, 2) & Str2)
        Range("D" & xRow) = CInt(Left(Str1, 3)) - CInt(Right(Str1, 2) & Str2)
      
        If (CInt(Left(Str1, 3)) - CInt(Right(Str1, 2) & Str2)) = CInt(yStr) Then
            Range("E" & xRow) = "TRUE"
            Range("F" & 1) = Str1 & Str2
            
        Else
            Range("E" & xRow) = "FALSE"
        
        
        
        End If
        
        If Abs(yStr - (CInt(Left(Str1, 3)) - CInt(Right(Str1, 2) & Str2))) > max Then
        
        max = Abs(yStr - (CInt(Left(Str1, 3)) - CInt(Right(Str1, 2) & Str2)))
        min_comb = Str1 & Str2
        Range("H" & 1) = max
        Range("I" & 1) = min_comb
        
        End If
        
        
        

        xRow = xRow + 1
        
    Else
        
        For i = 1 To xLen
            Call GetPermutation(Str1 + Mid(Str2, i, 1), Left(Str2, i - 1) + Right(Str2, xLen - i), xRow, yStr, max)
        Next
    End If
    
   
End Sub

******************************************************************************
