{define gcd (x y) (if (y == 0) x (gcd y (x % y)))}
{define factorial (x) (cond [((x == 0) => 1) (else => (x * (factorial (x - 1))))])}
{define fib (x) (cond [((x == 0) => 0) ((x == 1) => 1) (else => ((fib (x - 1)) + (fib (x - 2))))])}
{define x (fib 10)}
(factorial 3)
(fib 5)
(gcd 18 24)
x
(set x 3)
x
