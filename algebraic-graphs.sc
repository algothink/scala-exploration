object graph {
  // type class

  trait Graph[G[_]] {

    def empty[V]: G[V]
    def vertex[V](v: V): G[V]
    def connect[V](g1: G[V], g2: G[V]): G[V]
    def overlay[V](g1: G[V], g2: G[V]): G[V]
  }

  final implicit class GraphOps[G[_], V](val g: G[V]) extends AnyVal {
    def connect(g2: G[V])(implicit ev: Graph[G]): G[V] =
      ev.connect(g, g2)
    def ~>(g2: G[V])(implicit ev: Graph[G]): G[V] = connect(g2)

    def overlay(g2: G[V])(implicit ev: Graph[G]): G[V] =
      ev.overlay(g, g2)

    def ++(g2: G[V])(implicit ev: Graph[G]): G[V] = overlay(g2)

  }

  final case class Relation[A](domain: Set[A], relation: Set[(A, A)])

  // here's why we needed a Vertex type
  // there's only a graph instance for relation if
  // Vertex type has an Equiv instance
  object Relation {
    implicit def eqInstance[A](implicit ev: Equiv[A]): Equiv[Relation[A]] =
      (r1, r2) => r1.domain == r2.domain && r1.relation == r2.relation

    implicit def grInstance: Graph[Relation] = new Graph[Relation] {
      def empty[V] = Relation(Set.empty, Set.empty)
      def vertex[V](v: V) = Relation(Set(v), Set.empty)
      def connect[V](g1: Relation[V], g2: Relation[V]) =
        Relation(domain = g1.domain ++ g2.domain,
          relation = g1.relation ++ g2.relation ++ (for {
          a <- g1.domain
          b <- g2.domain
        } yield (a,b)))
      def overlay[V](g1: Relation[V], g2:Relation[V]) =
        Relation(g1.domain ++ g2.domain, g1.relation ++ g2.relation)
    }
  }

  final class GraphPartialApply[G[_]](g: Graph[G]) {
    def empty[V]: G[V] = g.empty[V]
    def vertex[V](v: V): G[V] = g.vertex(v)
    def edge[V](v1: V, v2: V): G[V] = clique(List(v1, v2))
    def edges[V](es: List[(V, V)]): G[V] =
      es.map((edge[V] _).tupled).foldRight(g.empty[V])(g.overlay(_, _))
    def graph[V](vs: List[V], es: List[(V, V)]): G[V] =
      g.overlay(vertices(vs), edges(es))
    def vertices[V](vs: List[V]): G[V] =
      vs.map(g.vertex _).foldRight(g.empty[V])(g.overlay(_, _))
    def clique[V](vs: List[V]): G[V] =
      vs.map(g.vertex _).foldRight(g.empty[V])(g.connect(_, _))
  }

  /**
   * ```
   * import AlgGraph._
   * val g: AlgGraph[String] = graph[AlgGraph].empty
   * ```
   */
  def apply[G[_]](implicit ev: Graph[G]) : GraphPartialApply[G] =
    new GraphPartialApply[G](ev)


  // Initial encoding
  sealed trait AlgGraph[+A]
  object AlgGraph { self =>
    case object Empty extends AlgGraph[Nothing]
    final case class Vertex[A](a: A) extends AlgGraph[A]
    final case class Overlay[A](g1: AlgGraph[A], g2: AlgGraph[A]) extends AlgGraph[A]
    final case class Connect[A](g1: AlgGraph[A], g2: AlgGraph[A]) extends AlgGraph[A]
    def empty[A]: AlgGraph[A] = Empty

    implicit val isGraph : Graph[AlgGraph] = new Graph[AlgGraph] {
      def empty[A] = self.empty[A]
      def vertex[A](a: A) = Vertex(a)
      def connect[A](g1: AlgGraph[A], g2: AlgGraph[A]) = (g1, g2) match {
        case (Empty, _) => g2
        case (_, Empty) => g1
        case _ => Connect(g1, g2)
      }
      def overlay[A](g1: AlgGraph[A], g2: AlgGraph[A]) = (g1, g2) match {
        case (Empty, _) => g2
        case (_, Empty) => g1
        case _ => Overlay(g1, g2)
      }
    }

  }
  // Adjacency matrix implementation
  final case class AdjMatrix[A] private[graph](m: Map[Int, List[Int]], vs: Vector[A])

  object AdjMatrix { self =>
    def empty[A]: AdjMatrix[A] = AdjMatrix(Map.empty, Vector.empty[A])

    implicit def isGraph: Graph[AdjMatrix] = new Graph[AdjMatrix] {

      def empty[A] = self.empty[A]

      def vertex[A](a: A) = AdjMatrix(Map(0 -> List.empty), Vector(a))

      def overlay[A](g1: AdjMatrix[A], g2: AdjMatrix[A]) =
        AdjMatrix(g1.m ++ shift(g2.m, g1.vs.size), g1.vs ++ g2.vs)

      def connect[A](g1: AdjMatrix[A], g2: AdjMatrix[A]) = {
        val shiftedG2 = shift(g2.m, g1.vs.size)
        AdjMatrix(g1.m.map{case (v, es) =>
          v -> (es ++ shiftedG2.keys) // connect to all g2 vertices
        } ++ shiftedG2, g1.vs ++ g2.vs)
      }

      private def shift(m: Map[Int, List[Int]], s: Int) =
        m.map{case (v, es) => (v + s, es map (_ + s))}

    }
  }

}
