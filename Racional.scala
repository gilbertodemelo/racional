class Racional(n: Int, d: Int):

  // validar denominador (não pode ser igual a zero)
  require(d != 0)

  private val g = gcd(n.abs, d.abs) // para sempre apresentar a fração em forma reduzida

  val num: Int = n / g
  val den: Int = d / g

  // para quando o denominador for = 1
  def this(n: Int) = this(n , 1)

  // função para adicionar dois números racionais
  def add(that: Racional): Racional = {
    Racional(num * that.den + den * that.num, den * that.den)
  }

  // synthatic sugar --- versão mais curta de add
  def +(that: Racional): Racional = {
    Racional(num * that.den + den * that.num, den * that.den)
  }

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  override def toString = s"$n/$d"
