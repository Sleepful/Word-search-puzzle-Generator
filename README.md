# Word Search Puzzle - Generator

A program made in Scheme, which is a Lisp implementation.

There's two main functions two it, both generate a word-search puzzle but one returns
the matrix of letters with just the words in it, useful to look at the solution to the puzzle,
and the other function returns a complete word-search puzzle, with the non-used spaces properly
filled with random letters.

word_puzzle_generator_show is the function that generates the puzzle without filling non-used spaces
word_puzzle_generator fills the non-used spaces

both functions recieve the same paramters, in the following order:
words size seed
words: this is a list of words that are going to go inside the puzzle
        each word is represented as a list, eg: '((h e  l l o)(y e s))
size: this is the size of the puzzle, it's represented as a list, eg: '(5 5) for a 5x5 matrix
seed: this is the seed used to give the generation of the puzzle some randomness,
       it uses a simple pseudo random generator so giving the same seed will always generate
       the same puzzle.
       
       
