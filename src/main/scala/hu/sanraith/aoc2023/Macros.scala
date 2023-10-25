package hu.sanraith.aoc2023

import scala.quoted.* // imports Quotes, Expr
import scala.deriving.Mirror

inline def inspect(inline x: Any): Any = ${ inspectCode('x) }
def inspectCode(x: Expr[Any])(using Quotes): Expr[Any] =
  println(x.show)
  x

inline def makeAList() = ${ makeAList2 }
def makeAList2(using Quotes) =
  Expr.apply(Seq("alma", "korte", "barack"))

inline def findSubclassModulesOfSealedTrait[T](using
    m: scala.deriving.Mirror.SumOf[T]
): Set[T] =
  allInstances[m.MirroredElemTypes, m.MirroredType].toSet

inline def allInstances[ET <: Tuple, T]: List[T] = {
  import scala.compiletime.*

  inline erasedValue[ET] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) =>
      summonInline[ValueOf[t]].value.asInstanceOf[T] :: allInstances[ts, T]
}

inline def findDirectSubModules[T] = ${ findDirectSubModulesImpl[T] }
def findDirectSubModulesImpl[T](using
    quotes: Quotes
) = {
  import quotes.reflect.*
  import scala.quoted.*
  import hu.sanraith.aoc2023.solution.Solution
  import hu.sanraith.aoc2023.solution.Title

  println("--- Listing children ---")
  val annotationSymbol = TypeTree.of[Solution].symbol
  val traitSymbol = TypeTree.of[Title].symbol
  val solutionSymbols =
    traitSymbol.owner.typeMembers
  println(
    solutionSymbols.map(x => s"${x} ${x.typeRef.classSymbol.get}\n")
  )

  Expr.apply("asd")
}
