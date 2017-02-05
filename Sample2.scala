import scalaz.{Free, Functor, Id, Monad, -\/, \/-, ~>}

object Sample2 extends App {
  trait Animal[+A]
  // Functorの制約により、各case classにジェネリクスなメンバを含める必要がある
  case class Human[A](name: String, a: A) extends Animal[A]
  case class Cheetah[A](kph: Int, a: A) extends Animal[A]
  case class Elephant[A](weight: Double, a: A) extends Animal[A]
  case class Monster[A](name: String, kph: Int, weight: Double, a: A) extends Animal[A]
  case class None() extends Animal[Nothing]

  def human(name: String): Free[Animal, String] =
    Free.liftF[Animal, String](Human(name, ""))
  def cheetah(kph: Int): Free[Animal, Int] =
    Free.liftF[Animal, Int](Cheetah(kph, 0))
  def elephant(weight: Double): Free[Animal, Double] =
    Free.liftF[Animal, Double](Elephant(weight, 0.0))
  def monster(name: String, kph: Int, weight: Double): Free[Animal, (String, Int, Double)] =
    Free.liftF[Animal, (String, Int, Double)](Monster(name, kph, weight, ("", 0, 0.0)))

  implicit val animalFunctor = new Functor[Animal] {
    def map[A, B](a: Animal[A])(f: A => B): Animal[B] = a match {
      // いずれかのcase文をコメントアウトするとランタイムエラーが発生する
      case Human(name, a)                => Human(name, f(a))
      case Cheetah(kph, a)               => Cheetah(kph, f(a))
      case Elephant(weight, a)           => Elephant(weight, f(a))
      case Monster(name, kph, weight, a) => Monster(name, kph, weight, f(a))
    }
  }

  val subs: Free[Animal, String] = for {
    _ <- human("Kenta Katsumata")
    kph <- cheetah(60)
    weight <- elephant(2.5)
    _ <- monster("Hulk", kph, weight)
  } yield (s"complete!")

  def run(free: Free[Animal, String]):String =
    free.resume match {
      case \/-(a) => a
      case -\/(Human(name, a)) => // いちいち受け取って・・・
        println(s"I'm a human. My name is ${name}.")
        run(a) // いちいち次の処理に渡す必要がある
      case -\/(Cheetah(kph, a)) =>
        println(s"I'm a cheetah. I can run at ${kph} kph.")
        run(a)
      case -\/(Elephant(weight, a)) =>
        println(s"I'm an elephant. My weight is ${weight} ton.")
        run(a)
      case -\/(Monster(name, kph, weight, a)) =>
        println(s"I'm a monster. My name is ${name}. I can run at ${kph} kph. My weight is ${weight} ton. hahaha.")
        run(a)
      case -\/(_) =>
        "not exists."
    }

  run(subs)
}