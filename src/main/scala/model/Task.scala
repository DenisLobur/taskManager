package model

import slick.jdbc.PostgresProfile.api._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

case class Task(title: String, isDone: Boolean, createdAt: Long, userId: Long, id: Long = 0L)

class TaskTable(tag: Tag) extends Table[Task](tag, "tasks") {
  val id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  val title = column[String]("title")
  val isDone = column[Boolean]("is_done")
  val createdAt = column[Long]("created_at")
  val userId = column[Long]("user_id")

  val userFk = foreignKey("user_fk", userId, TableQuery[UserTable])(_.id)

  def * = (title, isDone, createdAt, userId, id) <> (Task.tupled, Task.unapply)
}

object TaskTable {
  val tasks = TableQuery[TaskTable]
}

class TaskRepository(db: Database) {
  val tasksTableQuery = TableQuery[TaskTable]

  def create(task: Task): Future[Task] =
    db.run(tasksTableQuery returning tasksTableQuery += task)

  def update(task: Task): Future[Int] =
    db.run(tasksTableQuery.filter(_.id === task.id).update(task))

  def updateByName(taskName: String): Future[Int] = {
    val currentTask = Await.result(db.run(tasksTableQuery.filter(_.title === taskName).result), Duration.Inf).headOption
    currentTask match {
      case Some(t) => update(Task(t.title, isDone = true, t.createdAt, t.userId, t.id))
      case None => Future(0)
    }
  }

  def deleteById(taskId: Long): Future[Int] =
    db.run(tasksTableQuery.filter(_.id === taskId).delete)

  def deleteByName(taskName: String): Future[Int] =
    db.run(tasksTableQuery.filter(_.title === taskName).delete)

  def getById(taskId: Long): Future[Option[Task]] =
    db.run(tasksTableQuery.filter(_.id === taskId).result.headOption)

  def getByName(taskTitle: String): Future[Option[Task]] =
    db.run(tasksTableQuery.filter(_.title === taskTitle).result.headOption)
}
