package cps.celike

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*, given}

/*
// see https://github.com/dotty-cps-async/dotty-cps-async/issues/109
class DBIOPrintTest {

  type ConnectionIO[A] = Future[A]

  object GeneralPracticeUserPermission {

    object InviteUser {
      def check(userId: Int, generalPracticeId: Int): ConnectionIO[Unit] =
        ???
    }

  }

  object GeneralPracticeUserInvitedAt {
    def nowIO: ConnectionIO[java.time.Instant] = Future.successful(java.time.Instant.now())
  }

  trait QueryResult[T] {
    def option: Free[ConnectionIO, T]
  }

  case class Auth(userId: Int)
  val auth = Auth(1)

  object GeneralPracticeUser {}

  val dbIO = async[ConnectionIO] {
    GeneralPracticeUserPermission.InviteUser.check(auth.userId, req.generalPracticeId).await
    val invitedAt = GeneralPracticeUserInvitedAt.nowIO.await

    /** run here causes the exception, it's nonsense, but the compiler should say that it's not awaitable.
 *
 * https://typelevel.org/cats/api/cats/free/Free.html#run(implicitS:cats.Comonad[S]):A
 */
    val existingUser = GeneralPracticeUsers.queryIdByEmail(req.email).option.run.await
    val user = existingUser match {
      case Some(existingUserId) =>
        val assignedAt = GeneralPracticeUserAssignedToPracticeAt.nowIO.toConnectionIO.await
        val role = GeneralPracticeUserRole.Member
        val row = GeneralPracticesToUser.Row(req.generalPracticeId, existingUserId, role, assignedAt)
        GeneralPracticesToUser.Row.insert.toUpdate0(row).run.singleOrThrow_!.await
        val (fullName, phone) = queryGeneralPracticeUserDetails(existingUserId).unique.await
        GeneralPracticesListUsersResponse.ActiveUser(
          existingUserId,
          fullName,
          phone,
          role,
          assignedAt
        )

      case None =>
        val row = GeneralPracticePendingUsers.Row(req.email, req.generalPracticeId, auth.userId, invitedAt)
        GeneralPracticePendingUsers.Row.insert.toUpdate0(row).run.singleOrThrow_!.await
        val (fullName, _) = queryGeneralPracticeUserDetails(auth.userId).unique.await
        GeneralPracticesListUsersResponse.PendingUser(req.email, auth.userId, fullName, invitedAt)
    }
    GeneralPracticesInviteUserResponse(user)
  }

}
 */
