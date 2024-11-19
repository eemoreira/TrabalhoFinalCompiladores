f (x, y) ([], []) = ([x], [y])
f (x, y) (ansx, ansy) = 
    (x : ansx, y : ansy)
