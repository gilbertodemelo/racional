class Racional(n: Int, d: Int):

  // validar denominador (não pode ser igual a zero)
  require(d != 0)

  private val g = gcd(n.abs, d.abs) // para sempre apresentar a fração em forma reduzida

  val num: Int = n / g
  val den: Int = d / g

  // para quando o denominador for = 1
  def this(n: Int) = this(n , 1)


  // synthatic sugar --- versão mais curta de add
  def + (that: Racional): Racional = {
    Racional(num * that.den + den * that.num, den * that.den)
  }

  def + (x: Int): Racional = {
    Racional(num + x * den, den)
  }

  def - (that: Racional): Racional = {
    Racional(num * that.den - that.num * den, den * that.den)
  }

  def - (x: Int): Racional = {
    Racional(num - x * den, den)
  }

  def * (that: Racional): Racional = {
    Racional(num * that.num, den * that.den)
  }

  def * (x: Int): Racional = {
    Racional(num * x, den)
  }

  def / (that: Racional): Racional = {
    Racional(num * that.den, den * that.num)
  }

  def / (x: Int): Racional = {
    Racional(num, den * x)
  }

  def ^ (x: Int): Racional = {
    Racional(Math.pow(num, x).intValue(), Math.pow(den,x).intValue())
  }

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  override def toString = s"$n/$d"
