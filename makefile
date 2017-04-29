all: program

program:
	gfortran -Wall hangman.f95 -o hangman
