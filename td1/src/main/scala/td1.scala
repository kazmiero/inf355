package fr.enst.plnc2014.td1

class ExtSeq[T](s: Seq[T])
{
  def any(f: T => Boolean): Boolean =
    if (s.isEmpty)
      false
    else
      f(s.head) || new ExtSeq(s.tail).any(f)

  def all(f: T => Boolean): Boolean =
    if (s.isEmpty)
      true
    else
      f(s.head) && new ExtSeq(s.tail).all(f)
}

object ExtSeq
{
  implicit def toExtSeq[T](s: Seq[T]): ExtSeq[T] = new ExtSeq(s)
}

object TD1 {

  // Placer ici le code de isOdd & friends
  def isOdd(x: Int): Boolean = x.abs % 2 == 1
  def isEven(x: Int): Boolean = !isOdd(x)

  def myWhile(cond: => Boolean,f: => Any): Unit =
    if (cond){
      f
      myWhile(cond,f)
    }

}

class ExtCond(cond: => Boolean)
{
  def doWhile(f: => Any): Unit =
    if (cond){
      f
      doWhile(f)
   }
}

object ExtCond
{
  implicit def toExtCond(cond: => Boolean): ExtCond = new ExtCond(cond)
}

case class Complex(re: Double, im: Double)
{
  override def toString =
    if (im == 0.0)
      re.toString
    else if (re == 0.0)
      im+"i"
    else if (im > 0.0)
      re+"+"+im+"i"
    else
      re+""+im+"i"

  def reciprocal: Complex = new Complex(re,-im)

  def +(other: Complex): Complex = new Complex(re+other.re, im+other.im)
  
  def -(other: Complex): Complex = new Complex(re-other.re, im-other.im)

  //(a + ib) (c + id) = ac - bd + i (ad + bc)
  def *(other: Complex): Complex = new Complex(re*other.re - im*other.im, re*other.im + im*other.re)
  
  def sqAbs: Double = re*re + im*im
  def abs: Double = math.sqrt(re*re + im*im)

  // (a + ib) / (c + id) = 1 / (c**2+d**2) 
  def /(other: Complex): Complex = new Complex((this*other.reciprocal).re/other.sqAbs, (this*other.reciprocal).im/other.sqAbs)

  def arg: Double = 
    if (im == 0)
      if (re > 0)
	 math.Pi/2
      else if (re < 0)
	 -math.Pi/2
      else
	 throw new Exception("0 has no argument")
    else
      math.atan2(im,re)
}

object Complex
{
  implicit def toComplex(re: Double, im:Double): Complex = new Complex(re, im)
  implicit def toComplex(i: Int): Complex = new Complex(i, 0)
  implicit def toComplex(d: Double): Complex = new Complex(d, 0)
}

object Main extends App {

  import TD1._
  // Placer ici le code à exécuter

}
