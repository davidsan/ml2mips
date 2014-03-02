let x = 5;; 
let y = 2;;
let z = 3;;
let sum = function x -> function y -> function z ->
	x*x*y*y*z*z
;;

sum x y z;;