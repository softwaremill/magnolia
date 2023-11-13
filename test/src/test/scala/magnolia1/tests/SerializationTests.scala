package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*

import java.io.*
class SerializationTests extends munit.FunSuite:
  import SerializationTests.*

  private def serializeToByteArray(value: Serializable): Array[Byte] =
    val buffer = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(buffer)
    oos.writeObject(value)
    buffer.toByteArray

  private def deserializeFromByteArray(encodedValue: Array[Byte]): AnyRef =
    val ois = new ObjectInputStream(new ByteArrayInputStream(encodedValue))
    ois.readObject()

  def ensureSerializable[T <: Serializable](value: T): T =
    deserializeFromByteArray(serializeToByteArray(value)).asInstanceOf[T]

  test("generate serializable type-classes") {
    ensureSerializable(new Outer().showAddress)
    ensureSerializable(new Outer().showColor)
  }

object SerializationTests:
  sealed trait Entity
  case class Company(name: String) extends Entity
  case class Person(name: String, age: Int) extends Entity
  case class Address(line1: String, occupant: Person)

  sealed trait Color
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color
  case object Orange extends Color
  case object Pink extends Color
  class Outer:
    val showAddress: Show[String, Address] = summon[Show[String, Address]]
    val showColor: Show[String, Color] = summon[Show[String, Color]]

