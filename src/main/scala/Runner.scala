import java.util.Date

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
        println(s"${currentUser.name}'s tasks: ")
        showUserTasks(currentUser, 1)
        processSecondMenu(readSecondMenu, currentUser)
        true
      case 2 =>
        println(s"${currentUser.name}'s completed tasks: ")
        showUserTasks(currentUser, 2)
        processSecondMenu(readSecondMenu, currentUser)
        true
      case 3 =>
        println(s"${currentUser.name}'s uncompleted tasks: ")
        showUserTasks(currentUser, 3)
        processSecondMenu(readSecondMenu, currentUser)
        true
      case 4 =>
        val newTask = createNewTask(currentUser)
        println(s"task ${newTask.title} created")
        processSecondMenu(readSecondMenu, currentUser)
        true
      case 5 =>
        deleteTask(currentUser)
        processSecondMenu(readSecondMenu, currentUser)
        true
      case 6 =>
        markTaskAsDone()
        processSecondMenu(readSecondMenu, currentUser)
        true
      case 7 =>
        println("selected quit")
        false
      case _ =>
        println("Sorry, that command is not recognized")
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
          println(s"logged in as \'${z.name}\'")
          processSecondMenu(readSecondMenu, z)
        } else {
          println("incorrect password")
          enterPassword(usr)
          processSecondMenu(readSecondMenu, z)
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
        println(s"logged in as \'${usr.name}\'")
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

  def showUserTasks(user: User, kind: Int): Unit = {
    def timeParser(longTime: Long): String = {
      new Date(longTime).toString
    }

    def printPretty(rawTask: Task): Unit = {
      rawTask match {
          //TODO: check for empty seq
        case c => println("None")
        case _ => {
          println("------------------------------------------------------------------------")
          println(s"title: ${rawTask.title}")
          println(s"created at: ${timeParser(rawTask.createdAt)}")
          println(s"This task is ${if (rawTask.isDone) "already done" else "not done yet"}")
          println("------------------------------------------------------------------------")
        }
      }
    }

    kind match {
      case 1 =>
        exec(taskRepository.tasksTableQuery.filter(_.userId === user.id).result).foreach(task => printPretty(task))
      case 2 =>
        exec(taskRepository.tasksTableQuery.filter(_.userId === user.id).filter(_.isDone).result).foreach(task => printPretty(task))
      case 3 =>
        exec(taskRepository.tasksTableQuery.filter(_.userId === user.id).filter(!_.isDone).result).foreach(task => printPretty(task))
    }
  }

  def createNewTask(user: User): Task = {
    val newTask = StdIn.readLine("Enter task name")
    val tsk: Task = Await.result(taskRepository.create(Task(newTask.trim, isDone = false, System.currentTimeMillis(), user.id)), Duration.Inf)
    tsk
  }

  def deleteTask(user: User): Unit = {
    val taskToDelete = StdIn.readLine("enter task name to delete it from your schedule")
    val tsk = Await.result(taskRepository.deleteByName(taskToDelete), Duration.Inf)
    if (tsk != 0) {
      println(s"task \'$taskToDelete\' was successfuly deleted from your schedule")
    } else {
      println(s"no task \'$taskToDelete\' in your schedule")
    }
  }

  def markTaskAsDone(): Unit = {
    val markAsDone = StdIn.readLine("enter task name to mark it as 'Done'")
    val tsk = Await.result(taskRepository.updateByName(markAsDone), Duration.Inf)
    if (tsk != 0) {
      println(s"task \'$markAsDone\' was marked as 'Done'")
    } else {
      println(s"no task \'$markAsDone\' in your schedule")
    }
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
         |  1 - show all my tasks
         |  2 - show done tasks
         |  3 - show uncompleted tasks
         |  4 - crete new task
         |  5 - delete task
         |  6 - mark task as 'Done'
         |  7 - quit""".stripMargin)
    StdIn.readInt()
  }


}
