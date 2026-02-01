Writing your own monad
======================

To use ``async``/``await`` with a custom type ``F[_]``, you need to provide an instance of one of the |CpsMonad|_ type classes.
Which one depends on the features you need:

.. list-table::
   :header-rows: 1

   * - Typeclass
     - Provides
   * - |CpsMonad|_
     - Basic ``async``/``await`` (if, loops, no exceptions)
   * - |CpsTryMonad|_
     - + try/catch/finally inside ``async``
   * - ``CpsAsyncMonad``
     - + interop with callbacks and ``Future``
   * - ``CpsConcurrentEffectMonad``
     - + spawning fibers, structured concurrency for lazy effect monads (IO, ZIO, etc.)
   * - ``CpsSchedulingMonad``
     - + spawning for eager monads (Future), ``await[F]`` inside ``async[Future]``


Minimal implementation
----------------------

At minimum, you need to implement ``pure``, ``map``, ``flatMap``, and provide a monad context.
For simple cases where no special context API is needed, mix in |CpsMonadInstanceContext|_:

.. code-block:: scala

   import cps._

   // Example: a monad wrapping Option
   given CpsTryMonad[Option] with CpsTryMonadInstanceContext[Option] {
     def pure[A](a: A): Option[A] = Some(a)
     def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
     def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
     def error[A](e: Throwable): Option[A] = None
     def flatMapTry[A, B](fa: Option[A])(f: Try[A] => Option[B]): Option[B] =
       f(fa.map(Success(_)).getOrElse(Failure(new NoSuchElementException)))
   }

After defining this given instance, you can write:

.. code-block:: scala

   val result: Option[Int] = async[Option] {
     val x = await(Some(1))
     val y = await(Some(2))
     x + y
   }


Concurrent effect monads
-------------------------

For lazy effect monads like ``IO`` or ``ZIO``, where evaluation is deferred, implement ``CpsConcurrentEffectMonad``.
This extends ``CpsAsyncEffectMonad`` (which combines ``CpsAsyncMonad`` with delayed evaluation)
with ``CpsConcurrentMonad``, adding the ability to spawn computations in separate fibers:

.. code-block:: scala

   trait CpsConcurrentMonad[F[_]] extends CpsAsyncMonad[F] {
     type Spawned[A]
     def spawnEffect[A](op: => F[A]): F[Spawned[A]]
     def join[A](op: Spawned[A]): F[A]
     def tryCancel[A](op: Spawned[A]): F[Unit]
   }

   trait CpsConcurrentEffectMonad[F[_]]
     extends CpsConcurrentMonad[F] with CpsAsyncEffectMonad[F]

Here ``Spawned[A]`` represents a running fiber (e.g., ``Fiber[A]`` in Cats Effect, ``Fiber[E, A]`` in ZIO).

For eager monads like ``Future``, use ``CpsSchedulingMonad`` instead, where ``Spawned[A] = F[A]`` and spawning is immediate.

See the :ref:`Integrations` section for ready-made instances for Cats Effect, ZIO, and other frameworks.


Stack-safe monadic recursion (tailRecM)
---------------------------------------

|CpsMonad|_ provides a ``tailRecM`` method for stack-safe monadic recursion:

.. code-block:: scala

   def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]

It iterates ``f`` until the result is ``Right(b)``.  ``Left(a1)`` means "continue with new state ``a1``".

The default implementation uses ``flatMap`` recursively, which is stack-safe for monads with internal trampolining
(``Future``, ``IO``, ``TailRec``, etc.).
However, for **identity-like monads** where ``flatMap`` is just function application, the default causes a stack overflow on deep recursion.

``CpsIdentityMonad`` overrides ``tailRecM`` with a ``@tailrec``-annotated loop:

.. code-block:: scala

   override def tailRecM[A, B](a: A)(f: A => Either[A, B]): B = {
     @annotation.tailrec
     def loop(current: A): B = f(current) match {
       case Left(a1) => loop(a1)
       case Right(b) => b
     }
     loop(a)
   }

If your custom monad does not have internal trampolining (like identity or ``Either``), you should override ``tailRecM``
with an explicitly stack-safe implementation, either using ``@tailrec`` or an explicit loop with a mutable stack.


.. ###########################################################################
.. ## Hyperlink definitions

.. |async| replace:: ``async``
.. _async: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L30

.. |await| replace:: ``await``
.. _await: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L19

.. |CpsMonad| replace:: ``CpsMonad``
.. _CpsMonad: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala#L17

.. |CpsTryMonad| replace:: ``CpsTryMonad``
.. _CpsTryMonad: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala#L156

.. |CpsMonadInstanceContext| replace:: ``CpsMonadInstanceContext``
.. _CpsMonadInstanceContext: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonadContext.scala#L22
