package net.machinemuse.MD5lwjgl

import scala.io.Source
import java.io.BufferedReader
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Author: MachineMuse (Claire Semple)
 * Created: 1:52 PM, 7/3/13
 */
class AnimFileParser {

  import ParseUtils._

  def parseFile(filename: String) = {
    val reader = Source.fromFile(filename).bufferedReader()
    val builder = new AnimFileBuilder
    while (reader.ready()) {
      val line = trimComments(reader.readLine())
      val cmd = line.substring(0, line.indexOf(" "))
      val arg = line.substring(cmd.length).trim
      cmd match {
        case "MD5Version" => builder.MD5Version = parseInt(arg)
        case "commandline" => builder.commandline = Some(arg)

        case "numJoints" => builder.numJoints = parseInt(arg)
        case "numFrames" => builder.numFrames = parseInt(arg)
        case "frameRate" => builder.frameRate = parseInt(arg)
        case "numAnimatedComponents" => builder.numAnimatedComponents = parseInt(arg)

//        case "joints" => builder.joints = parseJoints(reader)
//        case "mesh" => builder.meshes.append(parseMesh(builder, reader))
        case _ =>
      }
    }
    builder.build()
  }

  def parseInt(str: String) = try {
    Some(str.toInt)
  } catch {
    case _: Throwable => None
  }

  def parseMesh(builder: MeshFileBuilder, reader: BufferedReader) = {
    var line = ""
    val mesh = new MeshBuilder
    while (reader.ready() && !line.contains("}")) {
      line = trimComments(reader.readLine())
      val cmd = line.substring(0, line.indexOf(" "))
      val arg = line.substring(cmd.length).trim
      cmd match {
        case "shader" => mesh.shader = Some(arg.replaceAll("\"", ""))
        case "numverts" => mesh.numverts = parseInt(arg)
        case "numtris" => mesh.numtris = parseInt(arg)
        case "numweights" => mesh.numweights = parseInt(arg)
        case "vert" => mesh.verts.append(parseVert(arg))
        case "tri" => mesh.tris.append(parseTri(arg))
        case "weight" => mesh.weights.append(parseWeight(arg))
        case _ =>
      }
    }
    mesh.build(builder.joints)
  }

  def parseVert(arg: String): VertexEntry = {
    val pattern = ParseUtils toRegex "number \\( number number \\) number number"
    val pattern(index, s, t, startweight, countweight) = arg
    VertexEntry(index.toInt, s.toFloat, t.toFloat, startweight.toInt, countweight.toInt)
  }


  def parseTri(arg: String): TriangleEntry = {
    val pattern = ParseUtils toRegex "number number number number"
    val pattern(index, a, b, c) = arg
    TriangleEntry(index.toInt, (a.toInt, b.toInt, c.toInt))
  }

  def parseWeight(arg: String): VertexWeight = {
    val pattern = ParseUtils toRegex "number number number \\( number number number \\)"
    val pattern(index, joint, bias, pox, posy, posz) = arg
    new VertexWeight(index.toInt, joint.toInt, bias.toFloat, new Vector3D(pox.toDouble, posy.toDouble, posz.toDouble))
  }

  def parseJoints(reader: BufferedReader) = {
    val jointEntries: mutable.Buffer[JointEntry] = new ListBuffer[JointEntry]
    val jointpattern = ParseUtils toRegex "\"word\" number \\( number number number \\) \\( number number number \\)"
    var line = ""
    while (reader.ready() && !line.contains("}")) {
      line = trimComments(reader.readLine())
      try {
        val jointpattern(name, parent, posx, posy, posz, ox, oy, oz) = line
        jointEntries.append(new JointEntry(name, parent.toInt, new Vector3D(posx.toDouble, posy.toDouble, posz.toDouble), new Vector3D(ox.toDouble, oy.toDouble, oz.toDouble)))
      } catch {
        case _: Throwable =>
      }
    }
    jointEntries.toArray
  }
}

class AnimFileBuilder {
  var MD5Version: Option[Int] = None
  var commandline: Option[String] = None
  var numJoints: Option[Int] = None
  var numFrames: Option[Int] = None
  var frameRate: Option[Int] = None
  var numAnimatedComponents: Option[Int] = None
  val meshes: mutable.Buffer[MD5Mesh] = new ListBuffer[MD5Mesh]
  var joints: Array[JointEntry] = null

  def build() = {
    if (MD5Version == None)
      throw new IllegalArgumentException("Error loading model: Couldn't find MD5 version.")
    //    if(commandline == None)
    //      throw new IllegalArgumentException("Error loading model: Couldn't find command line args.")
    if (joints == null || numJoints.getOrElse(-1) != joints.size)
      throw new IllegalArgumentException("Error loading model: Number of joints doesn't match declared number.")
//    if (numMeshes.getOrElse(-1) == meshes.size)
    //      throw new IllegalArgumentException("Error loading model: Number of meshes doesn't match declared number.")

    MD5MeshFile(MD5Version.get, commandline.getOrElse(""), joints, meshes)
  }

}