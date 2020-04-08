# Éclair - Parsing with derivatives and zippers 

Éclair is a Scala parsing combinator library.
Parsing in Éclair is performed using a novel algorithm based on derivatives and zippers.
Éclair handles all context-free grammars, including left-recursive ones.
Depending on the grammar, the runtime complexity will often be linear, with guaranteed worst-case cubic time.
