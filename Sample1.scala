import scalaz.{Free, Functor, Id, Monad, ~>}

object Sample1 extends App {
  trait Animal[+A]
  case class Human(name: String) extends Animal[String]
  case class Cheetah(kph: Int) extends Animal[Int]
  case class Elephant(weight: Double) extends Animal[Double]
  case class Monster(name: String, kph: Int, weight: Double) extends Animal[(String, Int, Double)]

  def human(name: String): Free[Animal, String] =
    Free.liftF[Animal, String](Human(name))
  def cheetah(kph: Int): Free[Animal, Int] =
    Free.liftF[Animal, Int](Cheetah(kph))
  def elephant(weight: Double): Free[Animal, Double] =
    Free.liftF[Animal, Double](Elephant(weight))
  def monster(name: String, kph: Int, weight: Double): Free[Animal, (String, Int, Double)] =
    Free.liftF[Animal, (String, Int, Double)](Monster(name, kph, weight))

  val interpreter = new (Animal ~> Id.Id) {
    def apply[A](a: Animal[A]): A = a match {
      case Human(name) =>
        println(s"I'm a human. My name is ${name}.")
        name
      case Cheetah(kph) =>
        println(s"I'm a cheetah. I can run at ${kph} kph.")
        kph
      case Elephant(weight) =>
        println(s"I'm an elephant. My weight is ${weight} ton.")
        weight
      case Monster(name, kph, weight) =>
        println(s"I'm a monster. My name is ${name}. I can run at ${kph} kph. My weight is ${weight} ton. hahaha.")
        (name, kph, weight)
    }
  }

  val subs: Free[Animal, String] = for {
    _ <- human("Kenta Katsumata")
    kph <- cheetah(60)
    weight <- elephant(2.5)
    _ <- monster("Hulk", kph, weight)
  } yield (s"complete!")

  val result = subs.foldMap(interpreter)
  println(result)
}