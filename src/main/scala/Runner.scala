import java.util.Date

import model._
import slick.collection.heterogeneous.Zero.+
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import slick.dbio.DBIO

import scala.collection.IterableLike
import scala.concurrent.Await
import scala.io.StdIn

object Runner {

  //for local Db
  //val db = Database.forURL("jdbc:postgresql://localhost/task_manager?user=postgres&password=123456789")
  //for Heroku Db
  val db = Database.forURL("jdbc:postgresql://ec2-54-83-48-188.compute-1.amazonaws.com:5432/dan1m28kemev23?user=hubbkjgxhyovxb&password=440d0fd15dbad0de86b3e8f1a32d8fd3926c2693ec40680d5e44c35cd0cdf5f0&sslmode=require")
  val userRepository = new UserRepository(db)
  val taskRepository = new TaskRepository(db)

  def exec[T](program: DBIO[T]): T = Await.result(db.run(program), Duration.Inf)

//    Need to comment this after firs run
    exec(UserTable.users.schema.drop.asTry andThen UserTable.users.schema.create)
    exec(TaskTable.tasks.schema.drop.asTry andThen TaskTable.tasks.schema.create)
    Await.result(userRepository.create(User("data", "data")), Duration.Inf)
    Await.result(userRepository.create(User("root", "root")), Duration.Inf)
    Await.result(taskRepository.create(Task("shower", true, System.currentTimeMillis(), 1)), Duration.Inf)
    exec(taskRepository.tasksTableQuery ++=createInitialTasks())

  def createInitialTasks() = Seq(
    Task("play chess", isDone = false, System.currentTimeMillis() - 1000, 1),
    Task("play footbal", isDone = true, System.currentTimeMillis() - 2000, 2),
    Task("play majong", isDone = false, System.currentTimeMillis() - 3000, 1),
    Task("play checkers", isDone = true, System.currentTimeMillis() - 4000, 2),
  )

  def main(args: Array[String]): Unit = {
    processFirstMenu(readFirstMenu)
  }

  def processFirstMenu(option: Int): Boolean = {
    option match {
      case 1 =>
        enterUser()
        true
      case 2 =>
        val user = addUser()
        processSecondMenu(readSecondMenu, user)
        true
      case 3 =>
        println("selected quit")
        false
      case _ =>
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
    val user = StdIn.readLine("enter user name:\n")
    val usr = Await.result(userRepository.getUserByName(user), Duration.Inf)
    usr match {
      case Some(z) => {
        val password = StdIn.readLine("enter password:\n")
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
      val password = StdIn.readLine("enter password:\n")
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

  def addUser(): User = {
    val newUserName = StdIn.readLine("add new user name\n")
    val existingUser = Await.result(userRepository.getUserByName(newUserName), Duration.Inf)
    existingUser match {
      case Some(u) =>
        println(s"user \'${u.name}\' already exists")
        addUser()
      case None =>
        val newUserPassword = StdIn.readLine("add new user password\n")
        val usr = User(newUserName, newUserPassword)
        println(s"new user \'$newUserName\' added")
        Await.result(userRepository.create(usr), Duration.Inf)
    }
  }

  def showUserTasks(user: User, kind: Int): Unit = {
    def timeParser(longTime: Long): String = {
      new Date(longTime).toString
    }

    def printPretty(rawTask: Task): Unit = {
      println("------------------------------------------------------------------------")
      println(s"title: ${rawTask.title}")
      println(s"created at: ${timeParser(rawTask.createdAt)}")
      println(s"This task is ${if (rawTask.isDone) "already done" else "not done yet"}")
      println("------------------------------------------------------------------------")
    }

    def emtyChecker(tasks: Seq[Task], emtyAnswer: String): Unit = {
      tasks match {
        case Seq() =>
          println(emtyAnswer)
        case x =>
          tasks.foreach(printPretty)
      }
    }

    kind match {
      case 1 =>
        val allTasks = exec(taskRepository.tasksTableQuery.filter(_.userId === user.id).result)
        emtyChecker(allTasks, "You do not have any tasks. Create some")
      case 2 =>
        val doneTasks = exec(taskRepository.tasksTableQuery.filter(_.userId === user.id).filter(_.isDone).result)
        emtyChecker(doneTasks, "No completed tasks")
      case 3 =>
        val undoneTasks = exec(taskRepository.tasksTableQuery.filter(_.userId === user.id).filter(!_.isDone).result)
        emtyChecker(undoneTasks, "No uncompleted tasks")
    }
  }

  def createNewTask(user: User): Task = {
    val newTask = StdIn.readLine("Enter task name\n")
    val existingTask = Await.result(taskRepository.getByName(newTask), Duration.Inf)
    existingTask match {
      case Some(t) =>
        println(s"task \'$newTask\' already exists")
        createNewTask(user)
      case None =>
        val tsk: Task = Await.result(taskRepository.create(Task(newTask.trim, isDone = false, System.currentTimeMillis(), user.id)), Duration.Inf)
        println(s"task \'$newTask\' created")
        tsk
    }
  }

  def deleteTask(user: User): Unit = {
    val taskToDelete = StdIn.readLine("enter task name to delete it from your schedule\n")
    val tsk = Await.result(taskRepository.deleteByName(taskToDelete), Duration.Inf)
    if (tsk != 0) {
      println(s"task \'$taskToDelete\' was successfuly deleted from your schedule")
    } else {
      println(s"no task \'$taskToDelete\' in your schedule")
    }
  }

  def markTaskAsDone(): Unit = {
    val markAsDone = StdIn.readLine("enter task name to mark it as 'Done'\n")
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
         |  2 - add user
         |  3 - quit""".stripMargin)
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
