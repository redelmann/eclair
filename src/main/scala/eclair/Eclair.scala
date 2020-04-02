package eclair

import eclair.traits._
import eclair.traits.result._

/** Contains types and methods for building parsers.
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
trait EclairUnit extends Eclair
                    with ResultUnit

/** Contains types and methods for building a parser.
  * Only a single value is returned in case of successful parses.
  *
  * @group eclair
  */
trait EclairSingle extends Eclair
                      with ResultSingle

/** Contains types and methods for building a parser.
  * All accepted values are returned (as an iterator) in case of successful parses.
  *
  * @group eclair
  */
trait EclairAll extends Eclair
                   with ResultAll