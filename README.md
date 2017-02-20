# cl-ad-lib
Generates random sentences and can fill in blanks in sentences.
Use the macro ad-lib with user input or use ad-lib-structure with a phrase structure list for something random.

To use the ad-lib macro, type `(ad-lib ...)` into the REPL, where the ellipsis is replaced with a sentence of your choosing. In the sentence, brackets containing a part of speech (or a symbol otherwise heading an association list) will be replaced by a random entry from that association list.

The ad-lib-structure function takes one argument - a list of symbols. These lists of symbols can be created by any of the functions which generate phrases, such as `(noun-phrase)`, `(prepositional-phrase)`, or the daddy of them all: `(sentence-structure)`. For example, `(ad-lib-structure (sentence-structure))` returns a random sentence in the form of a string.

Currently I'm trying to tackle a bug in which far too many adjectives and adverbs are generated. The argument to `(one-in (x))` doesn't seem to be reducing their number of occurrances. Sentences currently generated sound ridiculous. This won't be for long, hopefully.
