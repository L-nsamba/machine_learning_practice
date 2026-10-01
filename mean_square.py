def f(x, w, b):
    return w * x + b

def mse(x, y, w, b):
    total_error = 0.0
    n = len(y)

    for i in range (0, n):
        total_error += (y[i] - f(x[i], w, b)) ** 2 

    
    return total_error/n

y = [3, 2]
x =  [1, 2]
w = 1
b = 1

print(mse(x, y, w, b))