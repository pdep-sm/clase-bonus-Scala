package rateme

abstract class Accion {
  def popularidad(usuario: Usuario): Double

  def realizarse(usuario: Usuario)

  def calificacionFinal(calificacion: => Double): Double

}

case class Evento(calificacionMinima: Double) extends Accion {
  override def calificacionFinal(calificacion: => Double) = 0

  override def realizarse(usuario: Usuario): Unit = {}

  override def popularidad(usuario: Usuario): Double = calificacionMinima
}

case class Actividad(interesados: Seq[Usuario]) extends Accion {
  override def calificacionFinal(calificacion: => Double) =
    calificacion

  override def realizarse(usuario: Usuario): Unit = {
    interesados.foreach(_.calificar(usuario, this))
  }

  override def popularidad(usuario: Usuario): Double = {
    interesados.foldLeft(0d) {case (numero:Double, usr: Usuario) => numero + usr.calificacion(usuario, this)}
  }
}


