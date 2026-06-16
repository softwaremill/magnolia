package magnolia1.tests

import magnolia1.*
import scala.concurrent.{ExecutionContext, Future}

class MonadicTests extends munit.FunSuite:

  test("Monadic[Future].point lifts the value eagerly without using the ExecutionContext") {
    // An ExecutionContext that never runs any submitted task. With this EC,
    // Future.successful(value) is already completed, while Future(value)
    // (which submits a task) never completes.
    given ExecutionContext = new ExecutionContext:
      def execute(runnable: Runnable): Unit = ()
      def reportFailure(cause: Throwable): Unit = ()

    val f = summon[Monadic[Future]].point(42)

    assert(f.isCompleted, "point(value) should produce an already-completed Future")
    assertEquals(f.value.flatMap(_.toOption), Some(42))
  }
