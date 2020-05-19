Imports System
Imports MathNet.Numerics.LinearAlgebra
Imports MathNet.Numerics.LinearAlgebra.Double

Module Program
    '' Globals
    Dim Basic_Combinations As New List(Of List(Of Integer))
    Sub Main(args As String())
        '' Local Variables
        '' Problem Initialization
        '' We have these matrices somehow
        '' Note: A Has a Full Rank Assumption -> Chapter 2 Page 19
        '' Dim A_Canonical As Double()()  '' Makes a Double Array
        Dim A_Canonical = Matrix.Build.DenseOfArray({{2.0, 1.0, 1.0, 1.0, 0.0, 0.0},
                                                    {1.0, 2.0, 3.0, 0.0, 1.0, 0.0},
                                                    {2.0, 2.0, 1.0, 0.0, 0.0, 1.0}}) '' Values Example 1: Chapter 3 Page 48

        Dim b_Canonical = Vector.Build.DenseOfArray({2.0, 5.0, 6.0}) '' Make a Double Array
        Dim c = Vector.Build.DenseOfArray({-3.0, -1.0, -3.0, 0.0, 0.0, 0.0}) '' Objective Coefficients
        Dim Basic_Index As List(Of Integer) ''
        Dim oBasic_Index As Vector(Of Double) '' Ordered Basic Index
        Dim All_Index As List(Of Integer)
        Dim Non_Basic_Index As List(Of Integer)
        Dim nBasic As Integer  '' Number of Basic Variables
        Dim Degenerate As Boolean '' 1 if Degenrate, 0 if Non-Degenrate
        Dim nCols As Integer '' Number of Columns/Variables
        Dim nRows As Integer '' Number of Rows/Constraints
        Dim nIter As Integer = 0 '' Number of Iterations
        Dim z As Vector(Of Double) '' Eqn (23) Page 43


        '' Problem Initialization
        '' We have these matrices somehow
        '' Note: A Has a Full Rank Assumption -> Chapter 2 Page 19
        ''A_Canonical = {({2.0, 1.0, 1.0, 1.0, 0.0, 0.0}),
        ''            ({1.0, 2.0, 3.0, 0.0, 1.0, 0.0}),
        ''          ({2.0, 2.0, 1.0, 0.0, 0.0, 1.0})} '' Values Example 1: Chapter 3 Page 48
        ''b_Canonical = {2.0, 5.0, 5.0} '' Values Example 1: Chapter 3 Page 48
        ''c = {-3.0, -1.0, -3.0} '' Values Example 1: Chapter 3 Page 48

        Basic_Index = Find_Basic(A_Canonical)
        Non_Basic_Index = Find_Non_Basic(A_Canonical, Basic_Index)
        nCols = A_Canonical.ColumnCount '' Number of Columns of A
        nRows = A_Canonical.RowCount '' Number of Rows of A
        Dim A_next = Matrix(Of Double).Build.Dense(nRows, nCols) '' For next iteration A
        Dim b_next = Vector(Of Double).Build.Dense(nRows) '' For next iteration b
        Dim x = Vector(Of Double).Build.Dense(nCols)
        Dim r = Vector(Of Double).Build.Dense(nCols) '' Relative Cost
        x = Get_x(A_Canonical, b_Canonical, Basic_Index, Non_Basic_Index) '' (0, 0, 0,2,5,5) 
        Dim q As Integer '' Incoming Index
        Dim p As Integer '' Leaving Index
        Dim IsOptimal As Boolean '' 1 if Solution is Optimal, 0 otherwise
        Dim IsUnBounded As Boolean '' 1 if problem is unbounded, 0 otherwise
        nBasic = Basic_Index.Count
        Dim ratio = Vector(Of Double).Build.Dense(nBasic)
        Degenerate = IsDegenerate(Basic_Index, x) '' 1 if System is Degenerate, 0 if System is Non-Degererate

        '' Compute r and z
        z = Compute_z(A_Canonical, c, Basic_Index)
        '' Write a Method to see r(j)=c(j)-z(j) vector
        r = Compute_r(c, z)
        '' IsOptimal-> Check Method (Step 2) return a boolean
        '' Hari: r if all positive numbers >=0 , Optimal 1, Any negetive 0 , nonOptimal
        IsOptimal = Optimal_Check(r, Non_Basic_Index)
        '' Incoming Index-> Method (Step 2)
        '' Pranjal: Retun me an index q As Integer: the index of the most negetive element in (r, Non_Basic_Index) 
        q = Incoming_Index(r, Non_Basic_Index)
        ratio = Set_Ratio(A_Canonical, b_Canonical, Basic_Index, q)
        IsUnBounded = Boundedness_Check(ratio)
        oBasic_Index = Order_Basic_Index(Basic_Index, A_Canonical)
        p = Exiting_Index(ratio, oBasic_Index)
        pivot(A_Canonical, b_Canonical, A_next, b_next, p, q)

        While Not IsOptimal
            Basic_Index = Find_Basic(A_Canonical)
            Non_Basic_Index = Find_Non_Basic(A_Canonical, Basic_Index)
            nCols = A_Canonical.ColumnCount '' Number of Columns of A
            nRows = A_Canonical.RowCount '' Number of Rows of A
            x = Get_x(A_Canonical, b_Canonical, Basic_Index, Non_Basic_Index)
            Degenerate = IsDegenerate(Basic_Index, x)
            If Degenerate Then
                Console.WriteLine("Degenerate A: May go into Cyclic Iterations- Not Handeled")
            End If
            nBasic = Basic_Index.Count
            z = Compute_z(A_Canonical, c, Basic_Index)
            r = Compute_r(c, z)
            q = Incoming_Index(r, Non_Basic_Index)
            ratio = Set_Ratio(A_Canonical, b_Canonical, Basic_Index, q)
            IsUnBounded = Boundedness_Check(ratio)
            If IsUnBounded Then
                Console.WriteLine("Problem Unbounded- Stop")
                Exit While
            End If
            oBasic_Index = Order_Basic_Index(Basic_Index, A_Canonical)
            p = Exiting_Index(ratio, oBasic_Index)

            '' Pivot
            pivot(A_Canonical, b_Canonical, A_next, b_next, p, q)
            'A_Canonical = A_next
            '' Deep Copy
            For i = 0 To nRows - 1
                For j = 0 To nCols - 1
                    A_Canonical(i, j) = A_next(i, j)
                Next
                b_Canonical(i) = b_next(i)
            Next

            'b_Canonical = b_next '' Check if this is good copy or not
            A_next.Clear()
            b_next.Clear()
            '' After Pivot
            Basic_Index = Find_Basic(A_Canonical)
            Non_Basic_Index = Find_Non_Basic(A_Canonical, Basic_Index)
            z = Compute_z(A_Canonical, c, Basic_Index)
            r = Compute_r(c, z)

            '' Optimality Check
            IsOptimal = Optimal_Check(r, Non_Basic_Index)
            x = Get_x(A_Canonical, b_Canonical, Basic_Index, Non_Basic_Index)
            Console.WriteLine(nIter)
            Console.WriteLine(c * x)
            nIter = nIter + 1
        End While
        Console.WriteLine(A_Canonical(1, 1))


    End Sub

    Private Sub pivot(a_Canonical As Matrix(Of Double), b_Canonical As Vector(Of Double), ByRef a_next As Matrix(Of Double), ByRef b_next As Vector(Of Double), p As Integer, q As Integer)
        Dim nCols As Integer = a_Canonical.ColumnCount '' Number of Columns of A
        Dim nRows As Integer = a_Canonical.RowCount '' Number of Rows of A
        For i = 0 To nRows - 1
            For j = 0 To nCols - 1
                If i <> p Then
                    a_next(i, j) = a_Canonical(i, j) - (a_Canonical(i, q) / a_Canonical(p, q)) * a_Canonical(p, j)
                    b_next(i) = b_Canonical(i) - (a_Canonical(i, q) / a_Canonical(p, q)) * b_Canonical(p)
                Else
                    a_next(i, j) = (a_Canonical(p, j) / a_Canonical(p, q))
                    b_next(i) = (b_Canonical(p) / a_Canonical(p, q))
                End If

            Next
        Next

        ''Throw New NotImplementedException()
    End Sub

    Private Function Exiting_Index(ratio As Vector(Of Double), oBasic_Index As Vector(Of Double)) As Integer
        Dim p As Integer
        Dim temp As Double = 100000.0 '' Lowest positive Ratio 0
        Dim temp1 As Integer = 0 '' Indexing 
        For Each i In ratio
            If i > 0 And i < temp Then
                temp = i
                p = temp1
            End If
            temp1 = temp1 + 1
        Next
        Return p
        ''Throw New NotImplementedException()
    End Function

    Private Function Order_Basic_Index(basic_Index As List(Of Integer), a_Canonical As Matrix(Of Double)) As Vector(Of Double)
        Dim temp As New List(Of Double) '' Catch the 1s
        Dim temp2 = Vector(Of Double).Build.Dense(basic_Index.Count) '' Reorder
        Dim nCols As Integer = a_Canonical.ColumnCount '' Number of Columns of A
        Dim nRows As Integer = a_Canonical.RowCount '' Number of Rows of A
        For Each j In basic_Index
            For i = 0 To nRows - 1
                If a_Canonical(i, j) = 1 Then
                    temp.Add(i)
                    '' Need to break inner For loop
                End If
            Next
        Next


        For i = 0 To basic_Index.Count - 1

            temp2(temp(i)) = basic_Index(i) '' Reorder here
        Next
        Return temp2
        ''Throw New NotImplementedException()
    End Function

    Private Function Boundedness_Check(ratio As Vector(Of Double)) As Boolean
        Dim temp As Boolean = True
        For Each i In ratio
            If i > 0 Then
                temp = False
            End If
        Next
        Return temp
        ''Throw New NotImplementedException()
    End Function

    Private Function Set_Ratio(a_Canonical As Matrix(Of Double), b_Canonical As Vector(Of Double), basic_Index As List(Of Integer), q As Integer) As Vector(Of Double)
        Dim temp = Vector(Of Double).Build.Dense(basic_Index.Count)
        Dim nCols As Integer = a_Canonical.ColumnCount '' Number of Columns of A
        Dim nRows As Integer = a_Canonical.RowCount '' Number of Rows of A
        For i = 0 To nRows - 1
            temp(i) = b_Canonical(i) / a_Canonical(i, q)
        Next
        Return temp
        ''Throw New NotImplementedException()
    End Function

    Private Function Optimal_Check(r As Vector(Of Double), non_Basic_Index As List(Of Integer)) As Boolean
        Dim temp As Boolean = True
        For Each j In non_Basic_Index
            If r(j) < 0 Then
                temp = False
            End If
        Next
        Return temp
        ''Throw New NotImplementedException()
    End Function

    Private Function Incoming_Index(r As Vector(Of Double), non_Basic_Index As List(Of Integer)) As Integer
        Dim threshold As Integer = 0
        Dim temp As Integer = 0
        For Each j In non_Basic_Index
            If r(j) < threshold Then
                temp = j
                threshold = r(j)
            End If
        Next
        Return temp
        ''Return 1
        Throw New NotImplementedException()
    End Function

    Private Function Compute_r(c As Vector(Of Double), z As Vector(Of Double)) As Vector(Of Double)
        Dim temp = Vector(Of Double).Build.Dense(c.Count())
        For j = 0 To c.Count() - 1
            temp(j) = c(j) - z(j)
        Next
        Return temp
        ''Throw New NotImplementedException()
    End Function

    Private Function Get_x(a_Canonical As Matrix(Of Double), b_Canonical As Vector(Of Double), basic_Index As List(Of Integer), non_Basic_Index As List(Of Integer)) As Vector(Of Double)
        '' Go thorugh all the cols(j) in A
        '' See if col is non basic-> Add temp(j)= 0
        '' See if col is basic-?
        '' Find the non zero row index (Loop All Rows (i)- Find If A(i,j)=1, then temp(j)=b(i))
        Dim nCols As Integer = a_Canonical.ColumnCount '' Number of Columns of A
        Dim nRows As Integer = a_Canonical.RowCount '' Number of Rows of A
        Dim temp = Matrix(Of Double).Build.Dense(nRows, nRows)
        Dim k As Integer = 0 ' k is index for temp columns
        For Each j In basic_Index
            For i = 0 To nRows - 1
                temp(i, k) = a_Canonical(i, j)
            Next
            k = k + 1 '' Increment k
        Next
        Dim temp2 = temp.Inverse * b_Canonical '' This is the x values for all basic variables
        '' Now complete temp3
        k = 0 '' Will use k as flag for temp2 basic variable vector 
        Dim temp3 = Vector(Of Double).Build.Dense(nCols)
        For Each j In basic_Index
            temp3(j) = temp2(k)
            k = k + 1
        Next
        Return temp3

        ''Dim temp = Vector.Build.DenseOfArray({0.0, 0.0, 0.0, 2.0, 5.0, 5.0}) '' Make a Double Array
        ''Return temp
        ''Throw New NotImplementedException()
    End Function

    Private Function Find_Non_Basic(a_Canonical As Matrix(Of Double), basic_Index As List(Of Integer)) As List(Of Integer)
        Dim All_Index As New List(Of Integer)
        Dim Non_Basic_Index As New List(Of Integer)
        Dim nCols As Integer = a_Canonical.ColumnCount '' Number of Columns of A
        Dim nRows As Integer = a_Canonical.RowCount '' Number of Rows of A
        For j = 0 To nCols - 1
            All_Index.Add(j)
            Dim temp = 0
            For Each i In basic_Index
                If j <> i Then
                    temp += 1
                End If
            Next
            If temp = basic_Index.Count() Then
                Non_Basic_Index.Add(j)
            End If
        Next
        Return Non_Basic_Index
        Throw New NotImplementedException()
    End Function

    Private Function Compute_z(a_Canonical As Matrix(Of Double), c As Vector(Of Double), Basic_Index As List(Of Integer)) As Vector(Of Double)
        Dim nCols As Integer = a_Canonical.ColumnCount '' Number of Columns of A
        Dim nRows As Integer = a_Canonical.RowCount '' Number of Rows of A
        Dim temp = Vector(Of Double).Build.Dense(nCols)
        For j = 0 To nCols - 1
            For i = 0 To nRows - 1
                temp(j) = temp(j) + (c(Basic_Index(i)) * a_Canonical(i, j)) '' Check www.youtube.com/watch?v=Enjuh1_mVMY' Index c should be of basic variables only
            Next
        Next
        Return temp
        Throw New NotImplementedException()
    End Function

    Private Function Find_Basic(a_Canonical As Matrix) As List(Of Integer) '' Change to Integer
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
                Next
            Next
            '' A_dash now has the columns in the ith commnination of BAsic Combination
            '' Now we check if this A_dash is full rank: Det is 0 or not
            '' Hemant: Write a methond: Give is this identity
            '' 
            If Util_IsIdentity(M) Then
                ''Console.WriteLine("i is : ", i(0), i(1), i(2))
                Return i
            End If
            '' Break the loop
            '' Return i

        Next

        ''Used for testing
        ''Dim mTest = Matrix.Build.DenseOfArray({{0.0, 1.0, 0.0},
        ''                                           {0.0, 0.0, 1.0},
        ''                                           {1.0, 0.0, 0.0}})
        ''Console.WriteLine("Test is identity : {0}", Util_IsIdentity(mTest))
        ''Throw New NotImplementedException()
    End Function

    Private Function Find_Basic_Combinations(Cols As Integer(), nRows As Integer)
        Dim temp As New List(Of Integer)
        Dim nCols As Integer = Cols.Count()
        Dim nextnCols As Integer = nCols - 1 '' to pass on to next recurssion
        For i = 0 To nCols - (nCols - nRows)
            temp.Add(i) '' Pass only one elemnt in temp list
            Util_Comb(Cols, nRows, temp)
            temp.Clear()
        Next

        ''Throw New NotImplementedException()
    End Function

    Private Function Util_IsIdentity(M As Matrix)
        Dim n = M.ColumnCount  '' Not checking here for square matrix , assuming it's square.
        Dim a(n) As Integer
        For i = 0 To n - 1
            a(i) = 0
        Next

        Dim oneFoundInColFlag = False
        For i = 0 To n - 1  '' i is column index
            oneFoundInColFlag = False
            For j = 0 To n - 1
                If M(j, i) = 1 Then
                    If oneFoundInColFlag = False Then
                        oneFoundInColFlag = True
                    Else
                        Return False '' If it enters 1 case twice in a column becomes false
                    End If
                    a(j) = a(j) + 1 '' This will be updated only once as it will enter in a column only once--> We have a check for this just above
                ElseIf M(j, i) = 0 Then
                    Continue For
                Else
                    Return False
                End If
            Next
        Next

        ''Have reached till here means all elements were either 0 or 1 and each column had only 1 '1'.
        For i = 0 To n - 1
            If a(i) <> 1 Then
                Return False
            End If
        Next

        Return True
    End Function

    Private Function Util_Comb(cols() As Integer, nRows As Integer, ByVal temp As List(Of Integer))
        Dim nCols As Integer = cols.Count()
        Dim local_temp As New List(Of Integer)
        Dim local_temp1 As New List(Of Integer)
        For Each i In temp
            local_temp.Add(i)
            local_temp1.Add(i)
        Next
        If local_temp.Count() < 3 Then '' 3 should be nRows
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

    Private Function IsDegenerate(Basic_Index As List(Of Integer), x As Vector(Of Double)) As Boolean
        Dim temp As Boolean = False
        For Each i In Basic_Index
            If x(i) = 0 Then
                temp = False
            End If
        Next
        Return temp
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
