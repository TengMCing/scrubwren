test_that("py_class defines classes and convert toggles work", {
  skip_if_no_python()

  # Define class as in examples
  Employee <- py_class(
    "Employee", convert = FALSE,
    `__init__` = function(self, name, id) {
      self$name <- name
      self$id <- id
      return(py_builtins$None)
    },
    get_email = function(self) {
      paste0(self$name, "_", self$id, "@company.com")
    }
  )

  Mike <- Employee("Mike", "1234")
  expect_true(inherits(Mike$get_email(), "python.builtin.str"))
  expect_identical(reticulate::py_to_r(Mike$get_email()), "Mike_1234@company.com")

  # Conversion on/off
  py_convert_on(Mike)
  expect_type(Mike$get_email(), "character")
  py_convert_off(Mike)
  expect_true(inherits(Mike$get_email(), "python.builtin.str"))
})

test_that("py_super and py_super_init support superclass init", {
  skip_if_no_python()

  Employee <- py_class(
    "Employee", convert = FALSE,
    `__init__` = function(self, name, id) {
      self$name <- name
      self$id <- id
      return(py_builtins$None)
    },
    get_email = function(self) {
      paste0(self$name, "_", self$id, "@company.com")
    }
  )

  Salary <- py_class(
    "Salary", inherit = Employee, convert = FALSE,
    `__init__` = function(self, name, id, salary) {
      py_super_init(name, id)
      self$salary <- salary
      return(py_builtins$None)
    },
    get_salary_summary = function(self) {
      list(ID = self$id,
           Name = self$name,
           Email = self$get_email(),
           Salary = self$salary)
    }
  )

  Mike_salary <- Salary("Mike", "1234", 1000)
  ss <- Mike_salary$get_salary_summary()
  expect_true(reticulate::is_py_object(ss))
  expect_identical(reticulate::py_to_r(ss[["ID"]]), "1234")
  expect_identical(reticulate::py_to_r(ss[["Name"]]), "Mike")
  expect_identical(reticulate::py_to_r(ss[["Email"]]), "Mike_1234@company.com")
  expect_equal(as.numeric(reticulate::py_to_r(ss[["Salary"]])), 1000)
})

test_that("py_super() can be used to call superclass __init__", {
  skip_if_no_python()

  Base <- py_class(
    "Base", convert = FALSE,
    `__init__` = function(self, x) {
      self$x <- x
      return(py_builtins$None)
    }
  )

  Child <- py_class(
    "Child", inherit = Base, convert = FALSE,
    `__init__` = function(self, x, y) {
      # Use implicit super() injected by reticulate
      py_super()$`__init__`(x)
      self$y <- y
      return(py_builtins$None)
    }
  )

  obj <- Child(1L, 2L)
  expect_identical(reticulate::py_to_r(obj$x), 1L)
  expect_identical(reticulate::py_to_r(obj$y), 2L)
})
