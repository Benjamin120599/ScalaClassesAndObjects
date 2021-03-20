import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Main {

  def main(args: Array[String]): Unit = {

    println("Ingrese el número de checkqueos del paciente: ")
    val num = StdIn.readInt()

    val paciente1 = new Paciente("Paciente 1", "Paciente 1", "Paciente 1", 35, generarArrayFechas(num), generarHoraRegistros(num), generarValoresAleatorios(num, 1, 5), generarValoresAleatoriosDoubles(num, 0, 100), generarValoresAleatoriosDoubles(num, 0, 100))
    println(paciente1)

    println("Promedio Bienestar: "+paciente1.obtenerPromediosBienestar())
    println("\nTemperatura Mayor: ")
    paciente1.obtenerTemperaturaMayor().foreach(println)

    println("\nTemperatura Menor: ")
    paciente1.obtenerTemperaturaMenor().foreach(println)

    println("\n------------------------------------------------------------------------------------------\n")

    println(paciente1.toString)

  }

  def generarValoresAleatorios(tamaño:Int, inicio:Int, limite:Int): ArrayBuffer[Int] = {

    var arreglo = new ArrayBuffer[Int]()

    for(i<-0 until tamaño) {
      var random = ((Math.random() * limite) + inicio).toInt
      //println("Valor entero : "+random)
      arreglo.addOne(random)
    }

    return arreglo

  }

  def generarValoresAleatoriosDoubles(tamaño:Int, inicio:Int, limite:Int): ArrayBuffer[Double] = {

    var arreglo = new ArrayBuffer[Double]()

    for(i<-0 until tamaño) {
      var random = ((Math.random() * limite) + inicio)
      //println("Valor doble: "+random)
      arreglo.addOne(random)
    }

    return arreglo

  }

  def generarArrayFechas(tamaño:Int): ArrayBuffer[String] = {

    var arregloFechas = new ArrayBuffer[String]()

    for(i<-0 until tamaño) {
      var dia = generarValoresAleatorios(1, 1, 31)(0)
      var mes = generarValoresAleatorios(1, 1, 12)(0)
      var año = generarValoresAleatorios(1, 1000, 2021)(0)

      var fecha = ""+dia+"-"+mes+"-"+año+""
      //println("Fecha: "+fecha)
      arregloFechas.addOne(fecha)
    }

    return arregloFechas

  }

  def generarHoraRegistros(tamaño:Int): ArrayBuffer[String] = {

    var arregloHoras = new ArrayBuffer[String]()

    for(i<-0 until tamaño) {
      var hora = generarValoresAleatorios(1, 0, 24)(0)
      var minuto = generarValoresAleatorios(1, 0, 60)(0)
      var segundo = generarValoresAleatorios(1, 0, 60)(0)
      var horas = hora+":"+minuto+":"+segundo

      //println("Hora: "+horas)

      arregloHoras.addOne(horas)
    }

    return arregloHoras

  }

}


class Paciente(nombre:String, primerAp:String, segundoAp:String, edad:Byte, fecha:ArrayBuffer[String], horaRegistro:ArrayBuffer[String], nivelBienestar:ArrayBuffer[Int], temperatura:ArrayBuffer[Double], humedad:ArrayBuffer[Double]) {

  def obtenerPromediosBienestar(): Double = {
    var promedio = 0.0

    for(i<-0 until nivelBienestar.length) {
      promedio = promedio + nivelBienestar(i)
    }

    promedio = promedio/nivelBienestar.size
    return promedio

  }

  def obtenerTemperaturaMayor(): Array[String] = {

    var pos = 0
    var mayor = temperatura(pos)
    val arreglo = new Array[String](5)

    for (x<-1 until temperatura.length) {
      if (temperatura(x) > mayor) {
        mayor = temperatura(x)
        arreglo(0) = mayor.toString
        arreglo(1) = fecha(x)
        arreglo(2) = horaRegistro(x)
        arreglo(3) = nivelBienestar(x).toString
        arreglo(4) = humedad(x).toString
      } else {
        arreglo(0) = mayor.toString
        arreglo(1) = fecha(0)
        arreglo(2) = horaRegistro(0)
        arreglo(3) = nivelBienestar(0).toString
        arreglo(4) = humedad(0).toString
      }
    }

    return arreglo

  } //(junto con nivel de bienestar, fecha, etc.)

  def obtenerTemperaturaMenor(): Array[String] = {
    var pos = 0
    var menor = temperatura(pos)
    val arreglo = new Array[String](5)

    for(x<-1 until temperatura.length) {
      if(temperatura(x) < menor ) {
        menor = temperatura(x)
        arreglo(0) = menor.toString
        arreglo(1) = fecha(x)
        arreglo(2) = horaRegistro(x)
        arreglo(3) = nivelBienestar(x).toString
        arreglo(4) = humedad(x).toString
      } else {
        arreglo(0) = menor.toString
        arreglo(1) = fecha(0)
        arreglo(2) = horaRegistro(0)
        arreglo(3) = nivelBienestar(0).toString
        arreglo(4) = humedad(0).toString
      }
    }

    return arreglo
  } //(junto con nivel de bienestar, fecha, etc.)

  override def toString: String = "Nombre "+nombre+
    "\nPrimer Ap: "+primerAp+
    "\nSegundo Ap: "+segundoAp+
    "\nEdad: "+edad+
    "\nFechas: "+fecha.toString()+
    "\nHoras de Registro: "+horaRegistro.toString()+
    "\nNivel de Bienestar: "+nivelBienestar.toString()+
    "\nTemperaturas: "+temperatura.toString()+
    "\nHumedad: "+humedad.toString()

}
