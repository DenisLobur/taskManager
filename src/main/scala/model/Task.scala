package model

import slick.jdbc.PostgresProfile.api._

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


