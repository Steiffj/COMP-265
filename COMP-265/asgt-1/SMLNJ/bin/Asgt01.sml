
(* Comp 265 - Assignment #1
   Jordan Steiff
   Due Feb 2, 2018 *)

(* #2 - Greatest Common Divisor *)
fun gcd (a, b) = 
if (a mod b) = 0 then b
else gcd(b, a mod b);

(* #3 - Prime Numbers - returns true if input is prime, false if not *)
fun divisor (x, y) = 
if x mod y = 0 then y
else divisor (x, y + 1);

fun isPrime (x) = 
if x < 2 then false
else divisor(x, 2) = x; 

(* #4 - Select Primes from a List *)
fun selectPrimes (list) = 
if null list then nil
else if isPrime(hd(list)) then [hd(list)] @ selectPrimes(tl(list))
else selectPrimes(tl(list));
