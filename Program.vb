Imports System
Imports MathNet.Numerics.LinearAlgebra
Imports MathNet.Numerics.LinearAlgebra.Double

Module Program
    '' Globals
    Dim Basic_Combinations As New List(Of List(Of Integer))
    Sub Main(args As String())
        '' Local Variables
        '' Dim A_Canonical As Double()()  '' Makes a Double Array
        Dim A_Canonical = Matrix.Build.DenseOfArray({{2.0, 1.0, 1.0, 1.0, 0.0, 0.0},
                                                    {1.0, 2.0, 3.0, 0.0, 1.0, 0.0},
                                                    {2.0, 2.0, 1.0, 0.0, 0.0, 1.0}})

        Dim b_Canonical = Vector.Build.DenseOfArray({2.0, 5.0, 5.0}) '' Make a Double Array

        Dim c = Vector.Build.DenseOfArray({-3.0, -1.0, -3.0}) '' Objective Coefficients
        Dim Basic_Index As Boolean() '' 1 shows the coumn is basic variable and 0 otherwise 
        Dim nBasic As Integer  '' Number of Basic Variables
        Dim Degenerate As Boolean '' 1 if Degenrate, 0 if Non-Degenrate
        Dim nCols As Integer '' Number of Columns/Variables
        Dim nRows As Integer '' Number of Rows/Constraints
        Dim nIter As Integer = 0 '' Number of Iterations
        Dim r As Double() '' Relative Cost
        Dim z As Double() '' Eqn (23) Page 43


        '' Problem Initialization
        '' We have these matrices somehow
        '' Note: A Has a Full Rank Assumption -> Chapter 2 Page 19
        ''A_Canonical = {({2.0, 1.0, 1.0, 1.0, 0.0, 0.0}),
        ''            ({1.0, 2.0, 3.0, 0.0, 1.0, 0.0}),
        ''          ({2.0, 2.0, 1.0, 0.0, 0.0, 1.0})} '' Values Example 1: Chapter 3 Page 48
        ''b_Canonical = {2.0, 5.0, 5.0} '' Values Example 1: Chapter 3 Page 48
        ''c = {-3.0, -1.0, -3.0} '' Values Example 1: Chapter 3 Page 48

        Basic_Index = Find_Basic(A_Canonical)
        nCols = A_Canonical.ColumnCount '' Number of Columns of A
        nRows = A_Canonical.RowCount '' Number of Rows of A
        nBasic = Sum_Array(Basic_Index) '' Number of Basic Variables
        Degenerate = IsDegenerate(nBasic, nRows) '' 1 if System is Degenerate, 0 if System is Non-Degererate


        Console.WriteLine(A_Canonical(1, 1))


    End Sub

    Private Function Find_Basic(a_Canonical As Matrix) As Boolean()

        Dim nCols As Integer = a_Canonical.ColumnCount '' Number of Columns of A
        Dim nRows As Integer = a_Canonical.RowCount '' Number of Rows of A
        Dim nCols_C_nRows As Integer = nComb(nCols, nRows) '' nCr  
        Dim temp1 As Integer = nCols - nRows '' Number of cols - Number of rows
        Dim Cols As Integer()
        ReDim Cols(nCols - 1) '' Redim Cols
        Dim A_dash As Double()() = New Double(nRows - 1)() {} '' Extract the nRows number of columns to see linear dependence
        Dim M = Matrix(Of Double).Build.Dense(nRows, nRows)
        'ReDim A_dash(nRows)(nRows) ''ReDin A_Dash
        ''Initialize Cols
        For i = 0 To nCols - 1
            Cols(i) = i '' Initialize
        Next
        '' Clear Global Variable
        Basic_Combinations.Clear()
        Find_Basic_Combinations(Cols, nRows) '' Find index of all combinations select nRows number of columns at a time
        '' Update Global Variable
        Dim temp As Integer = 0
        '' Check Linear Independence
        For j = 0 To nRows - 1
            A_dash(j) = New Double(nRows - 1) {}
        Next
        For Each i In Basic_Combinations
            For j = 0 To nRows - 1 '' All rows in A_Canonical
                For temp = 0 To i.Count() - 1
                    A_dash(j)(temp) = a_Canonical(j, i(temp))
                    M(j, temp) = a_Canonical(j, i(temp))
                    ''temp = +1
                Next
            Next
            '' A_dash now has the columns in the ith commnination of BAsic Combination
            '' Now we check if this A_dash is full rank: Det is 0 or not
            Console.WriteLine(M.Determinant)
        Next
        ''Throw New NotImplementedException()
    End Function

    Private Function Find_Basic_Combinations(Cols As Integer(), nRows As Integer)
        Dim temp As New List(Of Integer)
        Dim nCols As Integer = Cols.Count()
        Dim nextnCols As Integer = nCols - 1 '' to pass on to next recurssion
        For i = 0 To nCols - (nCols - nRows) - 1
            temp.Add(i) '' Pass only one elemnt in temp list
            Util_Comb(Cols, nRows, temp)
            temp.Clear()
        Next

        ''Throw New NotImplementedException()
    End Function

    Private Function Util_Comb(cols() As Integer, nRows As Integer, ByVal temp As List(Of Integer))
        Dim nCols As Integer = cols.Count()
        Dim local_temp As New List(Of Integer)
        Dim local_temp1 As New List(Of Integer)
        For Each i In temp
            local_temp.Add(i)
            local_temp1.Add(i)
        Next
        If local_temp.Count() < 3 Then
            For i = (local_temp.Last() + 1) To nCols - 1
                local_temp1.Add(i) '' Pass only one elemnt in temp list
                Util_Comb(cols, nRows, local_temp1)
                local_temp1.RemoveAt(local_temp1.Count - 1)
                '' Reinstate temp
            Next
        Else
            Basic_Combinations.Add(local_temp)
            local_temp1.Clear()
        End If

        ''Throw New NotImplementedException()
    End Function

    Private Function nComb(nCols As Integer, nRows As Integer) As Integer
        If nCols >= nRows Then
            Return (fact(nCols) / (fact(nRows) * fact(nCols - nRows)))
        Else
            Console.WriteLine("Error- nCr n<r")
            Return 0
        End If
        Throw New NotImplementedException()
    End Function

    Private Function fact(i As Integer) As Integer
        If (i = 0 Or i = 1) Then
            Return 1
        Else
            Return i * fact(i - 1)
        End If
        Throw New NotImplementedException()
    End Function

    Private Function IsDegenerate(nBasic As Integer, nRows As Integer) As Boolean
        Return If(nBasic = nRows, 0, 1)
        Throw New NotImplementedException()
    End Function

    Private Function Sum_Array(basic_Index) As Double
        Dim temp As Double = 0
        For Each i In basic_Index
            temp = temp + Convert.ToDouble(i)
        Next
        Return temp
        Throw New NotImplementedException()
    End Function
End Module
