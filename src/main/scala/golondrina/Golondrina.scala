package golondrina

//Modelando clases y sus companion objects
case class Golondrina(var energia: Int, nombre: String) {

  def volar(kilometros: Int) = {
    energia -= kilometros / 2
  }

  def comer(gramos: Int) = {
    energia += gramos
  }

}

object Golondrina {

  def apply(blah: Boolean, energia: Int = 100)(nombre: String) = {
    new Golondrina(nombre = nombre, energia = energia)
  }

}
