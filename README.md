# deBruijn
## A Haskell program to play around with deBruijn sequences.

## How to use :
```
USAGE: ./deBruijn n [a] [--check|--unique|--clean|--lyndon|--circular|--read -> file]

      --check       check if a sequence is a de Bruijn sequence
      --unique      check if 2 sequences are distinct de Bruijn sequences
      --clean       list cleaning
      --read        read a file with instructions
      --lyndon      extract all lyndon words from a sequence
      --circular    shows all circular variante of a sequence
      n             order of the sequence
      a             alphabet [def: “01”]
```

## --read option exemple

```
CHECK
00010111

CHECK
00010111a



GENERATE

UNIQUE
010101
010100

CLEAR
ABVEDQFEF
SFE
101001
001110
00010111
END

LYNDON
00010111


CIRCULAR
00010111
```

All options are supported, comments aren't.

## Dependencies

> Haskell

> Stack 12.18

> Cabal

> Tasty

> Hspec

> Text