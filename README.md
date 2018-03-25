# Turing Machines
An exercise for me to write a Turing Machine interpreter in multiple languages.

## Languages
- [x] Python
- [x] C
- [x] SML
- [ ] Lisp
- [ ] C++
- [ ] Haskell
- [ ] Racket
- [ ] Go

## Example Turing Machines
### Simple Incrementer
* States: q0, qf
* Initial state: q0
* Final state(s): qf
* Symbols: B, 1
* Blank symbol: B
* Transitions:
	* (q0, 1, 1, RIGHT, q0)
	* (q0, B, 1, STAY, qf)
* Input tape: 1, 1, 1

### Three-State Busy Beaver
* States: a, b, c, halt
* Initial state: a
* Final state(s): halt
* Symbols: 0, 1
* Blank symbol: 0
* Transitions:
	* (a, 0, 1, RIGHT, b)
	* (a, 1, 1, LEFT, c)
	* (b, 0, 1, LEFT, a)
	* (b, 1, 1, RIGHT, b)
	* (c, 0, 1, LEFT, b)
	* (c, 1, 1, STAY, halt)
* Input tape: empty
