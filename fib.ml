let rec fib = function x -> if x < 2 then 1 else (fib(x-1))+(fib(x-2));;

fib 5;;
