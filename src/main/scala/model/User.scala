package model

import slick.jdbc.PostgresProfile.api._

case class User(id: Long, name: String, password: String)

class UserTable(tag: Tag) extends Table[User](tag, "users") {
  val id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  val name = column[String]("name")
  val password = column[String]("password")

  def * = (id, name, password) <> (User.tupled, User.unapply)
}

object UserTable {
  val users = TableQuery[UserTable]
}
