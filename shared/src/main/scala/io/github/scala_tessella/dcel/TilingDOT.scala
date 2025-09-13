package io.github.scala_tessella.dcel

object TilingDOT:

  extension (tiling: TilingDCEL)

    /** Generates a DOT representation of the topology of the tiling. */
    def toDOT: String =
      // Helpers to build stable identifiers for DOT nodes
      def vNodeId(v: Vertex): String   = s"""v:${v.id.value}"""
      def fNodeId(f: Face): String     = s"""f:${f.id.value}"""
      // Half-edges do not have IDs; use their index in tiling.halfEdges
      val heIndex: Map[HalfEdge, Int]  = tiling.halfEdges.zipWithIndex.toMap
      def eNodeId(e: HalfEdge): String =
        heIndex.get(e).map(i => s"e:$i").getOrElse(s"e:unknown")

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
      tiling.vertices.foreach { v =>
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
      tiling.faces.foreach { f =>
        val id    = fNodeId(f)
        val label = s"F ${f.id.value}"
        sb.append(s"""    "$id" [label="$label"];\n""")
      }
      sb.append("  }\n\n")

      // Subgraph for half-edges
      sb.append("  subgraph cluster_halfedges {\n")
      sb.append("    label=\"HalfEdges\";\n")
      sb.append("    color=lightgrey;\n")
      sb.append("    node [shape=box, style=filled, fillcolor=\"#f5f5f5\", fontname=\"Helvetica\"];\n")
      tiling.halfEdges.foreach { e =>
        val id    = eNodeId(e)
        val idx   = heIndex.getOrElse(e, -1)
        val label = s"HE $idx"
        sb.append(s"""    "$id" [label="$label"];\n""")
      }
      sb.append("  }\n\n")

      // Edges describing topology relations

      // Vertex -> leaving half-edge
      tiling.vertices.foreach { v =>

        v.leaving.foreach { e =>

          sb.append(s"""  "${vNodeId(v)}" -> "${eNodeId(e)}" [label="leaving"];\n""")
        }
      }

      // HalfEdge relations: origin, destination, twin, next, prev, incident face
      tiling.halfEdges.foreach { e =>
        // origin
        sb.append(s"""  "${eNodeId(e)}" -> "${vNodeId(e.origin)}" [label="origin"];\n""")

        // destination (if twin available)
        e.destination.foreach { d =>

          sb.append(s"""  "${eNodeId(e)}" -> "${vNodeId(d)}" [label="dest"];\n""")
        }

        // twin (directed both ways to capture the symmetric relation explicitly)
        e.twin.foreach { t =>

          sb.append(s"""  "${eNodeId(e)}" -> "${eNodeId(t)}" [label="twin"];\n""")
        }

        // next / prev (directed links along the face cycle)
        e.next.foreach { n =>

          sb.append(s"""  "${eNodeId(e)}" -> "${eNodeId(n)}" [label="next"];\n""")
        }
        e.prev.foreach { p =>

          sb.append(s"""  "${eNodeId(e)}" -> "${eNodeId(p)}" [label="prev"];\n""")
        }

        // incident face
        e.incidentFace.foreach { f =>

          sb.append(s"""  "${eNodeId(e)}" -> "${fNodeId(f)}" [label="face"];\n""")
        }
      }

      // Face -> outer component; Face -> inner components (if any)
      tiling.faces.foreach { f =>

        f.outerComponent.foreach { start =>

          sb.append(s"""  "${fNodeId(f)}" -> "${eNodeId(start)}" [label="outer"];\n""")
        }
      }
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
