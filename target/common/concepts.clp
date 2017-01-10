; basic concepts that apply to all architectures
(defgeneric save-register
            "Save a register's contents somewhere!")
(defgeneric restore-register
            "Restore a given register's value!")


(defgeneric call-direct
            "Branch to a given fixed address and save where we came from")
(defgeneric call-indirect
            "Branch to a given indirect address and save where we came from")
(defgeneric branch-direct
            "Branch to a given fixed address")
(defgeneric branch-indirect
            "Branch to a given indirect address")
(defgeneric rtemp0
            "Get the name of the temporary register!")
(defgeneric !read
            "Read from a given address")
(defgeneric !write
            "write a given value to a given address")
(defgeneric @label
            "Make a label")
(defgeneric goto
            "Goto a named place")
(defgeneric cmp:eq
            "Perform an equality comparison")
(defgeneric cmp:neq
            "Perform a not equal comparison")
(defgeneric cmp:lt
            "Perform a less than comparison")
(defgeneric cmp:gt
            "Perform a greater than comparison")
(defgeneric cmp:ge
            "Perform a greater than or equal to comparison")
(defgeneric cmp:le
            "Perform a less than or equal to comparison")
(defgeneric conditional-branch
            "Branch to a given address if a given condition is met")
(defgeneric conditional-branch-indirect
            "Branch to a given indirect address if a given condition is met")
