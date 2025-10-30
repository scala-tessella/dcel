package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingEquivalency.isBoundaryEquivalentTo
import io.github.scala_tessella.dcel.Tree.*
import io.github.scala_tessella.dcel.structure.VertexId

import scala.util.control.TailCalls.{TailRec, done, tailcall}

object TilingUniformity:

  /** Group the ids of the vertices in classes of equivalent TilingDCEL. Uses boundary-only comparison for
    * efficiency in uniformity calculations.
    */
  private def vertexIdClasses(centeredTilings: List[(VertexId, TilingDCEL)]): List[List[VertexId]] =
    centeredTilings
      .foldLeft(List.empty[(TilingDCEL, List[VertexId])]) { case (classes, (vertexId, tiling)) =>
        classes.indexWhere { case (representative, _) =>
          tiling.isBoundaryEquivalentTo(representative)
        } match
          case -1 =>
            // No equivalent class found, create a new one
            classes :+ (tiling, List(vertexId))
          case i  =>
            // Found an equivalent class at index i, add vertexId to it
            val (representative, vertexIds) = classes(i)
            classes.updated(i, (representative, vertexId :: vertexIds))
      }
      .map { case (_, vertexIds) =>
        vertexIds.reverse
      }

  extension (tiling: TilingDCEL)

    /** Calculates the uniformity of the tiling, each leaf a different class of vertices. */
    def uniformityTreeUncompressed: Tree[List[VertexId]] =
      val boundaryVertexIds = tiling.boundaryVertices.map(_.id)

      // Tail-recursive helper using TailCalls
      def deepMap(key: List[Int], vertexIds: List[VertexId]): TailRec[Tree[List[VertexId]]] =
        val distance        = key.length
        val centeredTilings = vertexIds.map(id => id -> tiling.getDcelAtVertex(id, distance).toOption.get)
        //      centeredTilings.filter((_, tiling) => TilingValidation.validate(tiling).isLeft).foreach((id, tiling) =>
        //        println(s"Invalid tiling for vertex $id at distance $distance")
        //      )
        val classes         = vertexIdClasses(centeredTilings)
        val boundaryInfoMap = centeredTilings.map { case (vid, tiling) =>
          vid -> tiling.boundaryVertices.map(_.id).toSet
        }.toMap
        val partitioned     = classes.map(_.partition { vertexId =>
          val localBoundaryVertexIds = boundaryInfoMap(vertexId)
          boundaryVertexIds.toSet.intersect(localBoundaryVertexIds).isEmpty
        })

        // Process children with tail recursion
        def iterate(
            remaining: List[((List[VertexId], List[VertexId]), Int)],
            accumulated: List[Tree[List[VertexId]]]
        ): TailRec[List[Tree[List[VertexId]]]] =
          remaining match
            case Nil                             => done(accumulated.reverse)
            case ((inner, stuck), index) :: tail =>
              val childKey = key :+ index
              if inner.nonEmpty then
                tailcall(deepMap(childKey, inner)).flatMap { childTree =>
                  val updatedChild = childTree match
                    case Leaf(_)                  => Leaf(stuck)
                    case Branch(_, grandchildren) => Branch(stuck, grandchildren)
                  iterate(tail, updatedChild :: accumulated)
                }
              else
                iterate(tail, Leaf(stuck) :: accumulated)

        tailcall(iterate(partitioned.zipWithIndex, Nil)).map { children =>

          Branch(Nil, children)
        }

      // Start from all inner vertices at the root
      deepMap(Nil, tiling.innerVertices.map(_.id)).result
