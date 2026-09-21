def matrix_multiply(A, B):
    # Obtaining the dimensions of the matrix
    rows_a = len(A)
    cols_a = len(A[0])
    rows_b = len(B)
    cols_b = len(B[0])

    # Verfying the matrices can multiply, i.e column no of matrix 1 == row no of matrix two
    if cols_a != rows_b:
        raise ValueError("Columns of A must match rows of B.")

    # Creation of a 2D list of 0's acting as placeholders initially
    result = [[0 for _ in range(cols_b)] for _ in range(rows_a)]

    # Performing the matrix multiplication
    # i acts as row picker
    # j acts as column picker
    # k acts as the traveler (multiplys and adds)
    for i in range(rows_a):
        for j in range(cols_b):
            for k in range(cols_b):
                result[i][j] += A[i][k] * B[k][j]