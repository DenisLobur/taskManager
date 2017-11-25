import model._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import slick.dbio.DBIO

import scala.concurrent.Await

object Runner {

  val db = Database.forURL("jdbc:postgresql://localhost/task_manager?user=postgres&password=123456789")
  val userRepository = new UserRepository(db)
  val taskRepository = new TaskRepository(db)

  def exec[T](program: DBIO[T]): T = Await.result(db.run(program), Duration.Inf)

  exec(UserTable.users.schema.drop.asTry andThen UserTable.users.schema.create)
  exec(TaskTable.tasks.schema.drop.asTry andThen TaskTable.tasks.schema.create)

  Await.result(userRepository.create(User("data", "data")), Duration.Inf)
  Await.result(userRepository.create(User("root", "root")), Duration.Inf)
  Await.result(taskRepository.create(Task("shower", true, System.currentTimeMillis(), 1)), Duration.Inf)

  def main(args: Array[String]): Unit = {
    println("Hello task manager")
  }

}
