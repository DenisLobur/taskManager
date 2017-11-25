package model

import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future

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

class UserRepository(db: Database) {
  val usersTableQuery = TableQuery[UserTable]

  def create(user: User): Future[User] =
    db.run(usersTableQuery returning usersTableQuery += user)

  def update(user: User): Future[Int] =
    db.run(usersTableQuery.filter(_.id === user.id).update(user))

  def delete(userId: Long): Future[Int] =
    db.run(usersTableQuery.filter(_.id === userId).delete)

  def getUserById(userId: Long): Future[Option[User]] =
    db.run(usersTableQuery.filter(_.id === userId).result.headOption)


}
