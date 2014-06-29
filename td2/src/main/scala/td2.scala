package fr.enst.plnc2014.td2

import rx.lang.scala._
import scala.concurrent.duration._
import scala.io.Source

object TD2 
{
  def fromURL(url: String): Observable[Char] = Observable[Char] { (subscriber: Subscriber[Char]) =>
    Source.fromURL(url).foreach(subscriber.onNext(_))
  }

  def getContent(url: String): Observable[String] = fromURL(url).toSeq.map(_.mkString)

}

object

 Main extends App {
  
  import TD2._
  // Placer ici le code à exécuter
  TD2.getContent("http://ip.jsontest.com/").subscribe(println(_))
}
