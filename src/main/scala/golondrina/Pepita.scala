package golondrina

//Modelando WKOs
object Pepita {

  var energia = 100

  def volar(kilometros: Int) = {
    energia -= kilometros / 2
  }

  def comer(gramos: Int) = {
    energia += gramos
  }

}
