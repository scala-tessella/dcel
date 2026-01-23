package io.github.scala_tessella.dcel.conversion

import io.github.scala_tessella.dcel.TilingDCEL
import io.github.scala_tessella.dcel.structure.*

object TilingDOT:

  extension (tiling: TilingDCEL)

    /** Generates a simplified DOT representation of the topology of the tiling with only vertices, and edges
      * between them, undirected if inner edges, directed if outer edges.
      */
    def toSimplifiedDOT: String =
      def vNodeId(id: VertexId): String = s"""v:${id.value}"""
//      def vNodeId(v: Vertex): String    = vNodeId(v.id)

      val sb = new StringBuilder
      sb.append("digraph SimplifiedTiling {\n")
      sb.append("  rankdir=LR;\n")
      sb.append("  fontsize=12;\n")
      sb.append("  labelloc=t;\n")
      sb.append("  label=\"Simplified Tiling Topology\";\n")
      sb.append("  node [shape=circle, fontname=\"Helvetica\"];\n\n")

      // Emit all vertices as nodes
      tiling.vertices
        .sortBy:
          _.id.value
        .foreach:
          vertex => sb.append(s"""  "${vNodeId(vertex.id)}" [label="${vertex.id.value}"];\n""")
      sb.append("\n")

      // Boundary (outer-face) edges: directed edges along the boundary half-edges
      val boundaryEdges =
        tiling.boundaryEdges.sortBy:
          _.idUnsafe
      boundaryEdges.foreach: halfEdge =>
        val (origId, destId) = halfEdge.idUnsafe
        sb.append(s"""  "${vNodeId(origId)}" -> "${vNodeId(destId)}";\n""")

      // Inner edges: add a single undirected-looking edge per twin pair (dir=none)
      // Exclude any pair that is on the boundary
      val innerPairsEmitted = scala.collection.mutable.HashSet.empty[(String, String)]
      tiling.halfEdges
        .sortBy:
          _.idUnsafe
        .foreach: halfEdge =>
          val isBoundary = tiling.isBoundaryEdge(halfEdge)
          val twinOpt    = halfEdge.twin

          if !isBoundary && twinOpt.isDefined && !tiling.isBoundaryEdge(twinOpt.get) then
            val (origId, destId) = halfEdge.idUnsafe
            val vA               = vNodeId(origId)
            val vB               = vNodeId(destId)
            // Normalize pair to avoid duplicates (emit once per undirected pair)
            val pair             = if vA <= vB then (vA, vB) else (vB, vA)
            if !innerPairsEmitted.contains(pair) then
              innerPairsEmitted += pair
              // Use a directed edge with dir=none to appear undirected in GraphViz
              sb.append(s"""  "${pair._1}" -> "${pair._2}" [dir=none];\n""")

      sb.append("}\n")
      sb.toString

    /** Generates a DOT representation of the topology of the tiling. */
    def toCompleteDOT: String =
      // Helpers to build stable identifiers for DOT nodes
      def vNodeId(v: Vertex): String   = s"""v:${v.id.value}"""
      def fNodeId(f: Face): String     = s"""f:${f.id.value}"""
      val verticesSorted               = tiling.vertices.sortBy(_.id.value)
      val facesSorted                  = tiling.faces.sortBy(_.id.value)
      val halfEdgesSorted              = tiling.halfEdges.sortBy(_.idUnsafe)
      def eNodeId(e: HalfEdge): String = s"e:${e.idUnsafe}"

      val sb = new StringBuilder

      sb.append("digraph TilingDCEL {\n")
      sb.append("  rankdir=LR;\n")
      sb.append("  fontsize=12;\n")
      sb.append("  labelloc=t;\n")
      sb.append("  label=\"TilingDCEL Topology\";\n\n")

      // Subgraph for vertices
      sb.append("  subgraph cluster_vertices {\n")
      sb.append("    label=\"Vertices\";\n")
      sb.append("    color=lightblue;\n")
      sb.append("    node [shape=circle, style=filled, fillcolor=\"#e6f2ff\", fontname=\"Helvetica\"];\n")
      verticesSorted.foreach { v =>
        val id    = vNodeId(v)
        val label = s"V ${v.id.value}"
        sb.append(s"""    "$id" [label="$label"];\n""")
      }
      sb.append("  }\n\n")

      // Subgraph for faces (include both inner and outer to capture all topology)
      sb.append("  subgraph cluster_faces {\n")
      sb.append("    label=\"Faces\";\n")
      sb.append("    color=lightgreen;\n")
      sb.append("    node [shape=diamond, style=filled, fillcolor=\"#eaffea\", fontname=\"Helvetica\"];\n")
      facesSorted.foreach: face =>
        val id    = fNodeId(face)
        val label = s"F ${face.id.value}"
        sb.append(s"""    "$id" [label="$label"];\n""")
      sb.append("  }\n\n")

      // Subgraph for half-edges
      sb.append("  subgraph cluster_halfedges {\n")
      sb.append("    label=\"HalfEdges\";\n")
      sb.append("    color=lightgrey;\n")
      sb.append("    node [shape=box, style=filled, fillcolor=\"#f5f5f5\", fontname=\"Helvetica\"];\n")
      halfEdgesSorted.foreach: halfEdge =>
        val id    = eNodeId(halfEdge)
        val label = s"HE ${halfEdge.idUnsafe}"
        sb.append(s"""    "$id" [label="$label"];\n""")
      sb.append("  }\n\n")

      // Edges describing topology relations

      // Vertex -> leaving half-edge
      verticesSorted.foreach: v =>
        v.leaving.foreach: e =>
          sb.append(s"""  "${vNodeId(v)}" -> "${eNodeId(e)}" [label="leaving"];\n""")

      // HalfEdge relations: origin, destination, twin, next, prev, incident face
      halfEdgesSorted.foreach: e =>
        // origin
        sb.append(s"""  "${eNodeId(e)}" -> "${vNodeId(e.origin)}" [label="origin"];\n""")

        // destination (if twin available)
        e.destination.foreach: d =>
          sb.append(s"""  "${eNodeId(e)}" -> "${vNodeId(d)}" [label="dest"];\n""")

        // twin (directed both ways to capture the symmetric relation explicitly)
        e.twin.foreach: t =>
          sb.append(s"""  "${eNodeId(e)}" -> "${eNodeId(t)}" [label="twin"];\n""")

        // next / prev (directed links along the face cycle)
        e.next.foreach: n =>
          sb.append(s"""  "${eNodeId(e)}" -> "${eNodeId(n)}" [label="next"];\n""")

        e.prev.foreach: p =>
          sb.append(s"""  "${eNodeId(e)}" -> "${eNodeId(p)}" [label="prev"];\n""")

        // incident face
        e.incidentFace.foreach: f =>
          sb.append(s"""  "${eNodeId(e)}" -> "${fNodeId(f)}" [label="face"];\n""")

      // Face -> outer component; Face -> inner components (if any)
      facesSorted.foreach: f =>
        f.outerComponent.foreach: start =>
          sb.append(s"""  "${fNodeId(f)}" -> "${eNodeId(start)}" [label="outer"];\n""")
//      tiling.faces.foreach { f =>
//        // innerComponents is List[Option[HalfEdge]]; include all present
//        val inner = f match
//          case face: Face => face
//          case _          => f
//        // Access via a safe local since innerComponents is private[dcel]; we can encode via reflection-like?
//        // We cannot access innerComponents directly; only outer boundary is needed for topology in DCEL without holes.
//        // However, if there are holes, they are still other faces; so we skip explicit inner components listing.
//      }

      sb.append("}\n")
      sb.toString
