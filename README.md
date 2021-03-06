# cl-ad-lib
Generates random sentences and can fill in blanks in sentences.
Use the function ad-lib with user input or use ad-lib-structure with a phrase structure list for something random.

To use the ad-lib function, type `(ad-lib ...)` into the REPL, where the ellipsis is replaced with one or more sentences of your choosing, each of which must be surrounded by quotation marks. In the sentence strings, brackets containing a part of speech (or a symbol otherwise heading an association list) will be replaced by a random entry from that association list. For example: `(ad-lib "I saw [name] [preposition] [article] [noun] today.""how interesting!")` might return "I saw Markus by the computer today. How interesting!".

The ad-lib-structure function takes one argument - a list of part-of-speech symbols, and can take an optional argument determining vocabulary list. The list of part-of-speech symbols can be created by any of the functions which generate phrases, such as `(noun-phrase)`, `(prepositional-phrase)`, or the granddaddy of 'em all: `(sentence-structure)`. For example, `(ad-lib-structure (sentence-structure))` returns a random sentence in the form of a string.

New: To use a custom vocabulary list, use the `(custom-ad-lib vocab ...)` where "vocab" is replaced by the name of a vocabulary association list variable. Otherwise, it functions the same as `(ad-lib ...)`.
