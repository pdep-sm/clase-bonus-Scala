package rateme

import scala.util.Random
import collection.mutable.{Seq => MutableSeq}

case class Usuario(
                    var calificacionGlobal: Double,
                    accionesRealizadas: MutableSeq[Accion]
                  ) {

  def actualizarCalificacion(calificacion: Double) = {
    calificacionGlobal += calificacion
    calificacionGlobal = calificacionGlobal.min(5).max(1)
  }


  def puedeRealizar(accion: Accion) = {
    accion match {
      case Evento(calificacion) => calificacion < calificacionGlobal
      case _ => true
    }
  }

  def calificacion(usuario: Usuario, accion: Accion) = {
    lazy val factorDeCalificacion = {
      if (criterioEvaluacion(usuario, accion)) 0.1
      else -0.12
    }

    accion.calificacionFinal(calificacionGlobal * factorDeCalificacion)
  }

  def calificar(usuario: Usuario, accion: Accion) = {
    usuario.actualizarCalificacion(calificacion(usuario, accion))
  }

  protected def criterioEvaluacion(usuario: Usuario, accion: Accion) = {
    Evaluador.calificaPositivo(this, accion)
  }
  def realizar(accion: Accion) = {
    if (!puedeRealizar(accion))
      throw new RuntimeException("No se puede realizar la accion")

    accionesRealizadas :+ accion
    accion.realizarse(this)
  }

  def accionMasPopular = {
    accionesRealizadas.maxByOption(_.popularidad(this))
  }
}

object Evaluador {

  def calificaPositivo(usuario: Usuario, accion: Accion) = {
    Random.nextBoolean()
  }
}





