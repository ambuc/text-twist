# text-twist

```php
$ runhaskell text-twist.hs
 ┌ T E X T   T W I S T
 ├ Using 'lowdown'. For a hint,
 ├ type ? and then a letter.
┏━━━━━━━━━━━━━━━━━━━━━━┓
┃ ~~~ ~~~ ~~~  ~~~~    ┃
┃ ~~~ ~~~ ~~~  ~~~~    ┃
┃ ~~~ ~~~ ~~~~ lowdown ┃
┃ ~~~ ~~~ ~~~~         ┃
┗┯━━━━━━━━━━━━━━━━━━━━━┛
 ├ Please enter a guess:
down
┏━━━━━━━━━━━━━━━━━━━━━━┓
┃ ~~~ ~~~ ~~~  ~~~~    ┃
┃ ~~~ ~~~ ~~~  ~~~~    ┃
┃ ~~~ ~~~ down lowdown ┃
┃ ~~~ ~~~ ~~~~         ┃
┗┯━━━━━━━━━━━━━━━━━━━━━┛
 ├ Please enter a guess:
?w
 ├ Showing 'w'.
┏━━━━━━━━━━━━━━━━━━━━━━┓
┃ ~~~ ~~~ w~~  w~~~    ┃
┃ ~~w ~w~ w~w  w~~~    ┃
┃ ~~~ ~w~ down lowdown ┃
┃ ~~w w~~ ~~~~         ┃
┗┯━━━━━━━━━━━━━━━━━━━━━┛
```

Text Twist is a [classic old internet flash
game](http://zone.msn.com/gameplayer/gameplayer.aspx?game=texttwist) in which
the  objective is to, given a starting word, find all the unique words which can 
be made from its letters.

This is a implementation in Haskell, playable from the shell. I hope the code is
fairly readable.

## Playing
You can just play the game:
```bash
$ runhaskell text-twist.hs
```
or, compile it first if you like:
```bash
$ ghc text-twist.hs
[1 of 1] Compiling Main             ( text-twist.hs, text-twist.o )
Linking text-twist ...

$ ./text-twist
 ┌ T E X T   T W I S T
 │ Please enter a seed word:
 ...
```

## Hints
You can type `?a` or `?b` or some such to ask Text Twist to show you each
occurence of that letter on the board. Sadly, that hint expires after a few
turns.
