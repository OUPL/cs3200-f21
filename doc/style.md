# Pyret programming style guidelines for CS 3200

In this course we will be programming in a functional style. This
means a few things:

* No mutable variables. All variables should be immutable (the default
  in Pyret) unless explicit permission to use mutation is given by the
  instructor.

* All iteration is done via recursion or functional combinators such
  as 'map' and 'fold' (which themselves are implemented via
  recursion). I.e., no imperative-style 'while' or 'for' loops.

In addition, you should:

* Include your name and OU ID at the top of the submission file.

* Make sure your submission has the '.arr' file extension.

* Always include type annotations for arguments and return types of
  toplevel functions (not necessary for lambdas or auxiliary functions
  defined within the scope of another function).
  
  E.g., the following function that adds 1 to its argument:

  ```
  fun add1(n):
    n + 1
  end
  ```

  should instead be written as:

  ```
  fun add1(n :: Number) -> Number:
    n + 1
  end
  ```

* Use reasonable variable names and formatting. Pyret is not
  indentation-sensitive in the way that, e.g., Python is, but it's
  probably a good idea to adhere to a similar indentation discipline
  anyway. Generally speaking, the easier it is for the TA to read your
  code, the more likely you are to receive a good grade.

* Ensure that your program is well-typed (i.e., that it passes Pyret's
  static typechecker).

* Avoid redundant expressions. E.g., `b == true` can be replaced by
  just `b`, and `b == false` by `not(b)`.

* Refer to
  [here](https://www.pyret.org/docs/latest/Pyret_Style_Guide.html) for
  style advice from the authors of Pyret.