import model._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import slick.dbio.DBIO

import scala.concurrent.Await
import scala.io.StdIn

object Runner {

  val db = Database.forURL("jdbc:postgresql://localhost/task_manager?user=postgres&password=123456789")
  val userRepository = new UserRepository(db)
  val taskRepository = new TaskRepository(db)

  def exec[T](program: DBIO[T]): T = Await.result(db.run(program), Duration.Inf)

  //  exec(UserTable.users.schema.drop.asTry andThen UserTable.users.schema.create)
  //  exec(TaskTable.tasks.schema.drop.asTry andThen TaskTable.tasks.schema.create)

  //  Await.result(userRepository.create(User("data", "data")), Duration.Inf)
  //  Await.result(userRepository.create(User("root", "root")), Duration.Inf)
  //  Await.result(taskRepository.create(Task("shower", true, System.currentTimeMillis(), 1)), Duration.Inf)

  def main(args: Array[String]): Unit = {
    //    var str = ""
    //    do {
    //      Console.println("Hello from main")
    //      str = StdIn.readLine("What's your name? ")
    //
    //      println(str)
    //    } while (!str.contains("exxit"))
    //    val str = StdIn.readLine("What's your name? ")
    //Iterator.
    //  continually(str).
    //  takeWhile(_ != "exxit").
    //  mkString("\n")
    //var opt = 0
    //    do {
    //opt = readOption
    //    } while (menu(readOption))
    processFirstMenu(readFirstMenu)
  }

  def processFirstMenu(option: Int): Boolean = {
    option match {
      case 1 =>
        enterUser()
        true
      case 2 =>
        println("selected quit")
        false
      case _ => // the else case
        println("Sorry, that command is not recognized")
        true
    }
  }

  def processSecondMenu(option: Int, currentUser: User): Boolean = {
    option match {
      case 1 =>
        //TODO: showw user's tasks
      case 2 =>
        val newTask = createNewTask(currentUser)
        println(s"task ${newTask.title} created")
        processSecondMenu(readSecondMenu, currentUser)
        true
    }
  }

  def enterUser(): Unit = {
    val user = StdIn.readLine("enter user name: ")
    val usr = Await.result(userRepository.getUserByName(user), Duration.Inf)
    usr match {
      case Some(z) => {
        val password = StdIn.readLine("enter password: ")
        if (z.password.equals(password)) {
          println(s"logged in as ${z.name}")
          processSecondMenu(readSecondMenu, z)
        } else {
          println("incorrect password")
          enterPassword(usr)
        }
      }
      case None => {
        println("There is no such user")
        processFirstMenu(1)
      }
    }
  }

  def enterPassword(user: Option[User]): User = user match {
    case Some(usr) => {
      val password = StdIn.readLine("enter password: ")
      if (usr.password.equals(password)) {
        println(s"logged in as ${usr.name}")
        User(usr.name, password)
      } else {
        println("incorrect password")
        enterPassword(Some(usr))
      }
    }
    case None => {
      println("There is no such user")
      processFirstMenu(1)
      User("", "")
    }
  }

  def createNewTask(user: User): Task = {
    val newTask = StdIn.readLine("Enter task name")
    val tsk = Await.result(taskRepository.create(Task(newTask, isDone = false, System.currentTimeMillis(), user.id)), Duration.Inf)
    tsk
  }

  def readFirstMenu: Int = {
    println(
      """|Menu:
         |  1 - login
         |  2 - quit""".stripMargin)
    StdIn.readInt()
  }

  def readSecondMenu: Int = {
    println(
      """|What's next:
         |  1 - show my tasks
         |  2 - create task
         |  3 - delete task
         |  4 - mark task as 'Done'
         |  5 - quit""".stripMargin)
    StdIn.readInt()
  }


}
