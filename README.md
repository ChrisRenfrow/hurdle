# Wordle CLI

A command-line rendition of the popular word game, written in Haskell.

I'm writing this as a way to get familiar with Haskell and functional
programming, and because I enjoy wordle and wanted an offline variant.

![Preview of game output](/example.png)

## Usage
`cabal run`

## Contributions

I welcome constructive feedback and advice. But, as I'm working on
this project solely for myself, please do not expect me to add any
requested/proposed features outside of what I have laid-out in the
**TODO** section.

## TODO

- [X] MVP (select a random word, see result of guesses, display success or failure)
- [x] Load wordlist from file into memory
- [ ] Guess validation
  - [ ] Proper length
  - [ ] Alphabetical characters only
  - [ ] Is valid word (is contained in validation wordlist)
- [ ] Make game copy-able as unicode string for sharing results
- [ ] Make game parameters configurable
  - [ ] Number of guesses
  - [ ] Length of words
  - [ ] Wordlist file path
  - [ ] Emoji used in shareable string

**Optional:**
- [ ] Make output nicer with buffering
- [ ] Display QWERTY (or other configurable layout) letter selection
