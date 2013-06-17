#  Haskeme
## What is haskeme
Haskeme is a LISP-like language which closely resembles Scheme. The lexer/parser uses the Parsec library.  The Evaluator has been loosely modelled to [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/)

## Features
* lists
* dotted lists
* quoting
* numeric operations
* boolean operations

## Style guide
* We're implementing a context-free grammar. That means, context free code. So we shall avoid monads as much as possible.
  Instead, we will use  Control.Applicative and Control.Alternative as much as possible.

## Usage
## License

