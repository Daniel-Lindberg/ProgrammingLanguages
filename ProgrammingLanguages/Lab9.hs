times m lst = [m*x | x<- lst]

mrg [] [] = []
mrg [] x = x
mrg x [] = x
mrg(a:x)(b:y) = if a < b then a: (mrg x (b:y)); else b:(mrg (a:x) y)

hammingNumbers [] = []
hammingNumbers (a:x) = a:(mrg(times a (hammingNumbers (a:x)))  (hammingNumbers x))