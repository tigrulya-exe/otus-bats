package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = {
    flatMap(fa)(identity)
  }
}

object Monad {
  def apply[F[_]](implicit instance: Monad[F]): Monad[F] = instance

  implicit class ValueOps[A, F[_]](value: A) {
    def point(implicit monad: Monad[F]): F[A] = monad.point(value)
  }

  implicit class MonadOps[A, F[_]](container: F[A]) {
    def flatMap[B](f: A => F[B])(implicit monad: Monad[F]): F[B] = {
      monad.flatMap(container)(f)
    }

    def map[B](f: A => B)(implicit monad: Monad[F]): F[B] = {
      monad.map(container)(f)
    }
  }

  implicit class NestedMonadOps[A, F[_]](container: F[F[A]]) {
    def flatten(implicit monad: Monad[F]): F[A] = monad.flatten(container)
  }

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    override def point[A](a: A): Option[A] = Option(a)
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    override def point[A](a: A): List[A] = List(a)
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  // kind-projector
  implicit def eitherMonadKind[L]: Monad[Either[L, *]] = new Monad[Either[L, *]] {
    override def flatMap[A, B](fa: Either[L, A])(f: A => Either[L, B]): Either[L, B] = fa.flatMap(f)
    override def point[A](a: A): Either[L, A] = Right(a)
    override def map[A, B](fa: Either[L, A])(f: A => B): Either[L, B] = fa.map(f)
  }

  // type lambdas
  def eitherMonad[L]: Monad[({type l[R] = Either[L, R]})#l] = new Monad[({type l[R] = Either[L, R]})#l] {
    override def flatMap[A, B](fa: Either[L, A])(f: A => Either[L, B]): Either[L, B] = fa.flatMap(f)
    override def point[A](a: A): Either[L, A] = Right(a)
    override def map[A, B](fa: Either[L, A])(f: A => B): Either[L, B] = fa.map(f)
  }
}
