import fansi.Str
object Exp1 {
  case class Employee(name: String)
  trait Payroll {
    def processEmployees(employees: Vector[Employee]): Either[Throwable, String]
  }

  trait USPayroll extends Payroll {
    override def processEmployees(
        employees: Vector[Employee]
    ): Either[Throwable, String] =
      Right("US payroll")

  }
  trait JPNPayroll extends Payroll {
    override def processEmployees(
        employees: Vector[Employee]
    ): Either[Throwable, String] =
      Right("Japan payroll")

  }
}

object Exp2 {
  
  trait PayrollSystem {
    case class Employee(name: String)
    type P <: Payroll
    trait Payroll {
      def processEmployees(employees: Vector[Employee]) : Either[Throwable, String]
    }

    def processPayroll(p: P): Either[Throwable, String]

  }
  
  trait USPayrollSystem extends PayrollSystem {
    class USPayroll extends Payroll {
      override def processEmployees(employees: Vector[Employee]) = {
        println("US Payroll")
        Right("US payroll")
      }
    }

    def processPayroll(p: USPayroll): Either[Throwable, String] = {
      p.processEmployees(Vector.empty)
    }
  }

  trait ContractorPayrollSystem extends PayrollSystem {
    case class Contractor(name: String)

    trait ContractorPayroll extends super.Payroll {
      def processContractors(contractors: Vector[Contractor]) = {
        println("US Payroll")
        Right("US payroll")
      }
    }

    def processPayroll(p: ContractorPayroll): Either[Throwable, String] = {
      p.processContractors(Vector.empty)
    }
  }


}
