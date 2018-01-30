fun listsum x = 
if null x then 0
else hd x + listsum (tl x);

fun average x =
real(listsum(x)) / real(length(x));

fun reverse l = 
if null l then nil
else reverse(tl l) @ [hd l];

fun strReverse l = 
implode(reverse(explode(l)));

fun fib n = 
if n <= 1 then 1
else fib(n-1) + fib(n-2);

fun realCube(n) =
real(n) * real(n) * real(n);

fun listIndex(index, list) = 
if index = 1 then hd(list)
else listIndex(index - 1, tl(list));

fun cycle(n, list) = 
if n = 0 then list
else cycle(n -1, tl(list) @ [hd(list)]);

fun min3 (a, b, c) =
if a <= b andalso a <= c then a 
else if b <= a andalso b <= c then b
else c;

fun order2 (a, b) =
if b <= a then [b, a]
else [a, b];

fun maxHelper(L, x) = 
if null L then x
else if x > hd L then maxHelper (tl L, x)
else maxHelper (tl L, hd L);





