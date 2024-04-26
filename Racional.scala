class Racional(n: Int, d: Int):

  // validar denominador (não pode ser igual a zero)
  require(d != 0)

  val num: Int = n
  val den: Int = d

  // função para adicionar dois números racionais
  def add(that: Racional): Racional = {
    Racional(num * that.den + den * that.num, den * that.den)
  }

  override def toString = s"$n/$d"
