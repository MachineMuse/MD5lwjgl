/**
 * Author: MachineMuse (Claire Semple)
 * Created: 3:19 PM, 7/2/13
 */
// Extending Tuple allows us to inline the variables at compile time, avoiding costly boxing/unboxing :)
class Quaternion(val w: Double, val x: Double, val y: Double, val z: Double) extends Tuple4(w, x, y, z) {

  // Only dealing with unit quaternions in this
  implicit def mk(x: Double, y: Double, z: Double) = new Quaternion(computew(x, y, z), x, y, z)

  private def computew(x: Double, y: Double, z: Double) = {
    val t: Double = 1.0 - x * x - y * y - z * z
    if (t < 0.0) 0.0 else -Math.sqrt(t)
  }

  // Convert from an axis vector by setting w = 0
  implicit def mk(p: Vector3D) = new Quaternion(0, p.x, p.y, p.z)

  // The quaternion multiplication allows to concatenate two rotations.
  // Be careful! Quaternions are non-commutative.
  def *(that: Quaternion) = new Quaternion(
    this.w * that.w - this.x * that.x - this.y * that.y - this.z * that.z,
    this.x * that.w + this.w * that.x + this.y * that.z - this.z * that.y,
    this.y * that.w + this.w * that.y + this.z * that.x - this.x * that.z,
    this.z * that.w + this.w * that.z + this.x * that.y - this.y * that.x)

  // Inverse
  def inv = new Quaternion(w, -x, -y, -z)

  // Rotate a point by a quaternion
  def rotate(p: Vector3D) = this * p * inv
}
