let rec map = function f -> function l -> if l =[] then [] else (f (car l))::(map f (cdr l));;
