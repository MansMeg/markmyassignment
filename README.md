[![Build Status](https://travis-ci.org/MansMeg/markmyassignment.svg?branch=master)](https://travis-ci.org/MansMeg/markmyassignment)

markmyassignment
================

A R package for automatic marking of R assignments/labs using the `testthat` package.

## Installation
To install the package use the following:

```r
install.packages("devtools")
devtools::install_github("MansMeg/markmyassignment")
library(markmyassignment)
```

## For students

To mark an assignment you need to set the assignment to mark in the followning way:

```r
set_assignment([assignment path])
```

where `[assignment path]` should be supplied by the teacher.

You can see which tasks that are included in the assignment with `show_tasks()`. 

```r
show_tasks()
```

To mark all tasks in an assignment use `mark_my_assignment()`. Remember that the task needs to be in R:s global environment. You can also mark specific tasks with the `tasks` argument.

```r
# Mark all tasks in global environment
mark_my_assignment()

# Mark the tasks "foo" and "bar" 
mark_my_assignment(tasks = c("foo", "bar"))
```

As a last step the whole lab file can be marked. To do this use the `mark_file` argument and give a search path to the file to mark. 

**Note!** The global environment needs to be empty when marking a file.

```r
# Mark all tasks in a file.
mark_my_assignment(mark_file = [my search path to file])

# Mark the tasks "foo" and "bar" in the file
mark_my_assignment(tasks = c("foo", "bar"), mark_file = [my search path to file])
```


## Teachers

To use `markmyassignment` as a teacher the following steps are needed. 

#### 1. Create a lab and task unit test files using `testthat`

The first step is to create a lab. =) Then produce unit tests using the `testthat` package. If the task is to create a numeric vector with the values `pi` and `e` an example test file can be found [here](https://github.com/MansMeg/markmyassignment/blob/master/inst/extdata/example_task1.R).

In a similar way one or more mandatory test files can be used for testing that should be tested. This is more to test the structure of the lab file. One example is that there should exist a `name` character vector with the students name.

#### 2. Create an assignment yml file

The next step is to put tasks together to an assignment. This is collection of testfiles and mandatory test files ordered hiearchically. The assignment files is in [yaml](http://www.yaml.org/) format and consists of the root nodes `name` (assignment name), `description` (assignment description),`reporter` (reporter to use, see below), `tasks` (tasks in assignments as separate nodes) and `mandatory` (optional, with urls to mandatory test). 

A working example can be seen [here](https://github.com/MansMeg/markmyassignment/blob/master/inst/extdata/example_assignment01.yml).

#### 3. Let the students work with the assignments and retrun lab R files with soulutions.

#### 4. Mark all student solutions with `set_assignment()` and `mark_my_dir()`.

All solutions from the students can be collected and stored in a local directory. The assignment to test needs to be set with `set_assignment()`. The `mark_my_dir()` function can the be used for the directory with the student solutions. The function returns a `data.frame` with one row per unit test file and lab file. 


### Additional testthat expectations

In marking of student lab files there are som extra exceptions created with this specific purpose. The purpose of this is to have specific test for common problems for students.

The extra exceptations implemented are:

- `expect_self_contained` tests if a function is self contained. 
- `expect_package_not_used` tests that an packas is *not* used.
- `expect_function_arguments` tests the argument namings of a function.

### Reporters

To return the results to the students all `testthat` reporters can be used. As default the `summary` reporter is used.

One extra reporter is implemented in `markmyassignment` called `student` reporter with minimal information (only test name and expectation information). The purpose is to minimize (more complex) unit tests results.

### Future work

- more expectations 
  - Expect tidy code
  - Expect ROxygen
- oauth connections to github
