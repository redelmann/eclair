package eclair

import eclair.traits._

/** Contains types and methods for building recogniser.
  * The type of results is left unspecified.
  *
  * @group eclair
  */
trait Eclair extends Combinators
                with Graphs
                with Parsers
                with Results
                with Tokens

/** Contains types and methods for building recogniser.
  *
  * @group eclair
  */
trait EclairRecogniser extends Eclair
                          with result.Units

/** Contains types and methods for building a parser.
  * Only a single value is returned in case of successful parses.
  *
  * @group eclair
  */
trait EclairParser extends Eclair
                      with result.Singles

/** Contains types and methods for building a parser.
  * All accepted values are returned (as an iterator) in case of successful parses.
  *
  * @group eclair
  */
trait EclairParserAll extends Eclair
                         with result.Closeds