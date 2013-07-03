package net.machinemuse.MD5lwjgl

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Author: MachineMuse (Claire Semple)
 * Created: 8:28 PM, 7/2/13
 */

class MeshBuilder {
  var shader: Option[String] = None

  var numverts: Option[Int] = None
  val verts: mutable.Buffer[VertexEntry] = new ListBuffer[VertexEntry]

  var numtris: Option[Int] = None
  val tris: mutable.Buffer[TriangleEntry] = new ListBuffer[TriangleEntry]

  var numweights: Option[Int] = None
  val weights: mutable.Buffer[VertexWeight] = new ListBuffer[VertexWeight]

  def build(joints: Array[JointEntry]): Either[String, MD5Mesh] = {
    //    if(shader == None)
    //      throw new IllegalArgumentException("Couldn't find shader for mesh.")
    if (numverts == None)
      return Left("Error loading model: Couldn't find number of vertices for mesh.")
    if (numtris == None)
      return Left("Error loading model: Couldn't find number of triangles for mesh.")
    if (numweights == None)
      return Left("Error loading model: Couldn't find number of weights for mesh.")
    val vertices = new Array[Vertex](numverts.get)
    val triangles = new Array[Triangle](numtris.get)
    val vertexweights = new Array[Weight](numweights.get)
    verts foreach {
      v => vertices(v.index) = new Vertex(v.s, v.t, v.startWeight, v.countWeight)
    }
    tris foreach {
      t => triangles(t.index) = new Triangle(vertices(t.vertices._1), vertices(t.vertices._2), vertices(t.vertices._3))
    }
    weights foreach {
      w => vertexweights(w.index) = new Weight(joints(w.joint), w.bias, w.pos)
    }
    Right(new MD5Mesh(shader.getOrElse(""), vertices, triangles, vertexweights, joints))
  }
}

case class VertexEntry(index: Int, s: Float, t: Float, startWeight: Int, countWeight: Int)

case class TriangleEntry(index: Int, vertices: (Int, Int, Int))

case class VertexWeight(index: Int, joint: Int, bias: Float, pos: Vector3D)

