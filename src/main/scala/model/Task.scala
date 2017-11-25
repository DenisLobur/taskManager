package model

import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future

case class Task(id: Long, title: String, isDone: Boolean, createdAt: Long, userId: Long)

class TaskTable(tag: Tag) extends Table[Task](tag, "tasks") {
  val id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  val title = column[String]("title")
  val isDone = column[Boolean]("is_done")
  val createdAt = column[Long]("created_at")
  val userId = column[Long]("user_id")

  val userFk = foreignKey("user_fk", userId, TableQuery[UserTable])(_.id)

  def * = (id, title, isDone, createdAt, userId) <> (Task.tupled, Task.unapply)
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

  def delete(taskId: Long): Future[Int] =
    db.run(tasksTableQuery.filter(_.id === taskId).delete)

  def getById(taskId: Long): Future[Option[Task]] =
    db.run(tasksTableQuery.filter(_.id === taskId).result.headOption)
}
