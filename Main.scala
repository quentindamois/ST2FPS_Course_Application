import scala.io.StdIn.readLine



object CourseApplication {
    def main(args: Array[String]) : Unit = {
        var ValueEnteredUser : String = "hello"
        val MemoryCourseInstance = MemoryCourse(List(Student("Henri", "henri.euro@gmail.net", 15.5), Student("Pierre", "Pieree.Leroy@efrei.com", 17.5)))
        println("Welcome to this course app where you can read, write and a List of student belongin to a course.\nThe command are:\nlist : see all student\nadd : add a student to the list\ndelete : remove a student from the list\nupdate : update information from a student from the list")
        while (ValueEnteredUser != "quit") {
            print(">")
            ValueEnteredUser = readLine()
            ValueEnteredUser match
                case "list" =>  UserInput.PrintListStudent(MemoryCourseInstance)
                case "add" => UserInput.AddStudentGuided(MemoryCourseInstance)
                case "delete" => UserInput.DeleteStudentGuided(MemoryCourseInstance)
                case "update" => UserInput.UpdateStudentGuided(MemoryCourseInstance)
                case _ => println("[error] You did not use one of the available command:\nlist : see all student\nadd : add a student to the list\ndelete : remove a student from the list\nupdate : update information from a student from the list")
        }
    }
}
/**The class Student is the class used to store the Student information
  *@constructor The constructor of the Student class
  *@param name The name of the student
  *@param email The name of the student
  *@param mark The mark of the student on a course
  */
case class Student(name: String, email: String, mark: Double)
/**The class MemoryCourse is a List of the Student part of this class.
  *@constructor The constructor of the MemoryCourse Class
  *@param ListStudentMemoryCourse is a List of object Student
  */
case class MemoryCourse(var ListStudentMemoryCourse: List[Student]) :
    /**AddStudent is the function to add a Student object to the list of object Student called ListStudentMemoryCourse
      *@param s a Student object to add to the list
    */
    def AddStudent(s : Student) : Unit = {
        ListStudentMemoryCourse = ListStudentMemoryCourse.appended(s)
    }
    /**DeleteStudent is the function to delete an object Student from the list of object Student called ListStudentMemoryCourse
      *@param NameOldStudent a String corresponding the name of the student that will be deleted
    */
    def DeleteStudent(NameOldStudent : String) : Unit = {
        ListStudentMemoryCourse = ListStudentMemoryCourse.filter(s => s.name != NameOldStudent)
    }
    /**UpdateStudent is the function to modify the information related to a student.
      *@param OldName a String corresponding to the previous name of the student whose information are being modified
      *@param NewName a String corresponding to the new name of the object Student
      *@param NewEmail a String corresponding the new email of the object Student
      *@param NewMark a Double corresponding to the new mark of the object Student
    */
    def UpdateStudent(OldName : String, NewName : String, NewEmail : String, NewMark : Double) : Unit = {
        ListStudentMemoryCourse = ListStudentMemoryCourse.updated(ListStudentMemoryCourse.indexWhere(s => s.name == OldName),  Student(NewName, NewEmail, NewMark))
    }
end MemoryCourse

/**The object UserInput is used to ask the user for input and call the function of the object MemoryCourse
*/
object UserInput :
    /**AddStudentGuided is the function is used to ask for the user input when they want to add a student to the List of Student StudentMemoryCourse
      *@param m the MemoryCourse whose ListStudentMemoryCourse will receive a new student
    */
    def AddStudentGuided(m : MemoryCourse) : Unit = {
        println("You are adding a new student to the course:")
        print("Name:")
        val NameNewStudent : String = readLine()
        print("Email:")
        val EmailNewStudent : String = readLine()
        EmailNewStudent match { // We chceck to see if the email address id correctly formated
            case s"$_@$_." if EmailNewStudent.count(x => x == '@') == 1 => println(s"[error] The email address must have text after the '.'.\nYou entered $EmailNewStudent")
            case s"$_@.$_" if EmailNewStudent.count(x => x == '@') == 1 => println(s"[error] The email address must have text between the '@' and the '.'.\nYou entered $EmailNewStudent")
            case s"@$_.$_" if EmailNewStudent.count(x => x == '@') == 1 => println(s"[error] The email address must have text between befor the '@'.\nYou entered $EmailNewStudent")
            case s"$_@$_.$_" if EmailNewStudent.count(x => x == '@') == 1 => {
                print("Mark:")
                val MarkNewStudent : Double = (readLine()).toDouble
                MarkNewStudent match { // we check to see if the mark is between 0.0 and 20.0
                    case MarkNewStudent if MarkNewStudent <= 20.0 && MarkNewStudent >= 0.0 => m.AddStudent(Student(NameNewStudent, EmailNewStudent, MarkNewStudent))
                    case MarkNewStudent if MarkNewStudent > 20.0 => println(s"[error] The mark must be between 20.0 and 0.0 included.\n$MarkNewStudent is above 20.0.")
                    case MarkNewStudent if MarkNewStudent < 0.0 => println(s"[error] The mark must be between 20.0 and 0.0 included.\n$MarkNewStudent is below 0.0.")
                }
            }
            case _ => println(s"[error] The email address must contain a @ and at leat one . after the @.\nYou entered $EmailNewStudent")
        }
        
    }
    /**The function PrintStudent is used to print the information of one object Student
      *@param s an object Student whose field will be display
    */
    def PrintStudent(s : Student) : Unit = {
        val name : String = s.name
        val email : String = s.email
        val mark : Double = s.mark
        println(s"|name: $name\temail: $email\tmark: $mark")
    }
    /**The function ListStudent is used to display the information of all the object Student present in the List of Student ListStudentMemoryCourse of one object MemoryCourse
      *@param m the object MemoryCourse whose ListStudentMemoryCourse we want to display
    */
    def PrintListStudent(m : MemoryCourse) : Unit = {
        m.ListStudentMemoryCourse.map(PrintStudent)
    }
    /**The function DeleteStudenGuided is used to ask for the user input when deleting a student from a object MemoryCourse's List of Student ListStudentMemoryCourse
      *@param m the MemoryCourse who will have one of the student in its ListStudentMemoryCourse deleted
    */
    def DeleteStudentGuided(m : MemoryCourse) : Unit = {
        println("Please enter the name of the student you want to delete:")
        print("Name:")
        val NameStudent : String = readLine()
        if m.ListStudentMemoryCourse.exists(s => s.name == NameStudent) then { // we check to see if their is a student with that name
            m.DeleteStudent(NameStudent)
            println(s"The student $NameStudent has been deleted.")
        }
        else 
            println(s"[error] The student $NameStudent does not exist.")
    }
    /**The function DeleteStudenGuided is used to ask for the user input when deleting a student from a object MemoryCourse's List of Student ListStudentMemoryCourse
      *@param m the MemoryCourse who will have one of the student in its ListStudentMemoryCourse deleted
    */
    def UpdateStudentGuided(m : MemoryCourse) : Unit = {
        println("Please enter the name of the student you want to update:")
        print("Name:")
        val StudentName : String = readLine()
        if m.ListStudentMemoryCourse.exists(s => s.name == StudentName) then { // we check to see if their is a student with that name
            println("Please enter the new information of the student:")
            print("Name:")
            val StudentNameNew : String = readLine()
            if m.ListStudentMemoryCourse.exists(s => s.name == StudentNameNew) && StudentNameNew != StudentName then { // We check to see if the new name does not already belong to another object Student
                println(s"[error] A student already has the name $StudentNameNew.")
            } else {
                print("Email:")
                val StudentEmail : String = readLine()
                StudentEmail match { // We check to see the email is correctly formated
                    case s"$_@$_." if StudentEmail.count(x => x == '@') == 1 => println(s"[error] The email address must have text after the '.'.\nYou entered $StudentEmail")
                    case s"$_@.$_" if StudentEmail.count(x => x == '@') == 1 => println(s"[error] The email address must have text between the '@' and the '.'.\nYou entered $StudentEmail")
                    case s"@$_.$_" if StudentEmail.count(x => x == '@') == 1 => println(s"[error] The email address must have text between befor the '@'.\nYou entered $StudentEmail")
                    case s"$_@$_.$_" if StudentEmail.count(x => x == '@') == 1 => {
                        print("Mark:")
                        val StudentMark : Double = (readLine()).toDouble
                        StudentMark match { // we check to see if the mark is between 0.0 and 20.0
                            case StudentMark if StudentMark <= 20.0 && StudentMark >= 0.0 => m.UpdateStudent(StudentName, StudentNameNew, StudentEmail, StudentMark)
                            case StudentMark if StudentMark > 20.0 => println(s"[error] The mark must be between 20.0 and 0.0 included.\n$StudentMark is above 20.0.")
                            case StudentMark if StudentMark < 0.0 => println(s"[error] The mark must be between 20.0 and 0.0 included.\n$StudentMark is below 0.0.")
                        }
                    }
                    case _ => println(s"[error] The email address must contain a @ and at leat one . after the @.\nYou entered $StudentEmail")
                }
            }
        }
        else println(s"[error] The student $StudentName is not present.")
    }
end UserInput
