package net.machinemuse.MD5lwjgl

/**
 * Author: MachineMuse (Claire Semple)
 * Created: 4:47 PM, 7/2/13
 */
class MD5Mesh(val shader: String, val verts: Seq[Vertex], val tris: Seq[Triangle], val weights: Seq[Weight], val joints:Seq[JointEntry])

case class Vertex(s:Float, t: Float, start:Int, count:Int)

case class Triangle(v1:Vertex, v2:Vertex, v3:Vertex)

case class Weight(joint:JointEntry, bias:Float, pos:Vector3D)

case class JointEntry(name: String, parent: Int, pos: Vector3D, orient: Vector3D)