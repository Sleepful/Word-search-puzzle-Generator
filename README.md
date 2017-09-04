# Word Search Puzzle - Generator

A program made in Scheme, which is a Lisp implementation.

There's two main functions two it, both generate a word-search puzzle but one returns
the matrix of letters with just the words in it, useful to look at the solution to the puzzle,
and the other function returns a complete word-search puzzle, with the non-used spaces properly
filled with random letters.

`word_puzzle_generator_show` is the function that generates the puzzle without filling non-used spaces
`word_puzzle_generator` fills the non-used spaces

both functions recieve the same paramters, in the following order:

words size seed

`words:` this is a list of words that are going to go inside the puzzle
        each word is represented as a list, eg: '((h e  l l o)(y e s))
`size:` this is the size of the puzzle, it's represented as a list, eg: '(5 5) for a 5x5 matrix
`seed:` this is the seed used to give the generation of the puzzle some randomness,
       it uses a simple pseudo random generator so giving the same seed will always generate
       the same puzzle.
       
       

to run the program you can use chicken, a scheme implementation:
https://www.call-cc.org/
Run the interpreter in the cmd, like so: 

`$ csi`

Then you just need to load the source file and call the functions:

`(load "mainchicken.scm")`

`(word_puzzle_generator_show '((u n o)(d o s)(t r e s)) '(5 5) 37274)`

`(word_puzzle_generator '((u n o)(d o s)(t r e s)) '(5 5) 37274)`


# Implementation 

The core of the algorithm (after deciding on the word list and the size of the puzzle grid) is just this:

* Pick the next word in the list.
* Add it to a random location in the grid.
* If the word fails to fit anywhere on the grid, backtrack and try from step #2 with the previous word.
* If there is no previous word, fail (because the words cannot all fit on the grid).
* Once the word has been placed successfully, repeat from step #1.
* Once all words have been placed, fill in the unused squares with random letters.
