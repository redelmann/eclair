// package eclair

// object Test extends EclairParser {
//   type Token = Char

//   sealed trait Kind
//   case object Digit extends Kind
//   case object Plus extends Kind
//   case object Times extends Kind
//   case object Other extends Kind

//   def time[R](block: => R): R = {
//     val t0 = System.nanoTime()
//     val result = block
//     val t1 = System.nanoTime()
//     println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
//     result
//   }

//   override def getKind(token: Token): Kind = token match {
//     case '+' => Plus
//     case '*' => Times
//     case c if c.isDigit => Digit
//     case _ => Other
//   }


//   lazy val inf: Syntax[Int] = recursive {
//     inf.map(_ + 1) | success(0)
//   }

//   val digit: Syntax[BigInt] = elem(Digit).map(c => BigInt(c.toString))

//   val plus: Syntax[(BigInt, BigInt) => BigInt] = elem(Plus).const(_ + _)

//   val times: Syntax[(BigInt, BigInt) => BigInt] = elem(Times).const(_ * _)

//   val exprLeft: Syntax[BigInt] = infixLeft(digit)(plus)

//   val exprRight: Syntax[BigInt] = infixRight(digit)(plus)

//   val expr: Syntax[BigInt] = digit
//       .leftAssoc(times)
//       .leftAssoc(plus)
// }