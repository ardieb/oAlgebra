#directory "_build";;
#use "arrayMatrix.ml";;
#use "rationals.ml";;
open Matrix;;

module RM = MAKE_MATRIX(RATIONAL);;
#install_printer RM.format;;