# Organization of Programming Languages

[Course Discord server](https://discord.gg/jXJA6424Dy)

[Pyret style guide](./doc/style.md)

## Fall 2021

There are thousands of programming languages, from A#.NET to ZPL and everything in between. Do you need to know all of them to be a good programmer/engineer/computer scientist? 

The goal of this course is to convince you that the answer to this question is no. In fact, many programming languages — while superficially distinct at the level of syntax — are actually quite similar once you take a closer look. 

This semester, we'll explore by boiling a number of programming languages down to a small set of more fundamental language features, including structured data, control, mutable state, (higher-order) functions, types, polymorphism, and objects. Once you understand how these features work in isolation, you'll start seeing them (or not!) in all your favorite programming languages. This, in turn, will make it easy to pick up new languages with minimal fuss. 

To learn many of these features, you'll be implementing them yourselves within a series of increasingly complex interpreters for small programming languages. The meta-language for programming and discussion is Pyret, a new PL developed primarily by Shriram Krishnamurthi at Brown University. Try it out now at [code.pyret.org/editor](https://code.pyret.org/editor). 

## Pyret QuickStart Links
* [Online Editor](https://code.pyret.org/editor)
* [Language Concepts](https://www.pyret.org/docs/latest/Language_Concepts.html)
* [Libraries](https://www.pyret.org/docs/latest/Builtins_and_Libraries.html)
* [A Tour of Pyret](https://www.pyret.org/docs/latest/A_Tour_of_Pyret.html)

## Prerequisites
CS 2650 and 3000, but also: Some mathematical maturity (at the level of "I've seen and done a few proofs before") and (most importantly) a desire to learn!

|                       |         Details      |
|-----------------------|----------------------|
| **Lecture**           | MWF 3:05-4:00pm in Stocker 103 |
| **Instructor**        | Alexander Bagnall (ab667712@ohio.edu) |
| **Office Hours**      | MWF 2-3pm (Stocker 379) |
| **TA**                | Jacob Schaupp (js400421@ohio.edu) |

## Textbook

The primary textbook is available online: [Programming and Programming Languages (PAPL)](https://papl.cs.brown.edu/2020/). Be sure to use the 2020 edition!

I may also assign select readings from [Types and Programming Languages (TAPL)](https://www.cis.upenn.edu/~bcpierce/tapl/). Any such readings will be made available on Blackboard.

## Course Structure

We'll meet MWF from 3:05-4pm. Attendance in class is required.
Homework consists of programming assignments and Blackboard quizzes. We'll have both a traditional in-class midterm and a final.

### Grade Breakdown

| Component               | Percentage |
|-------------------------|-----|
| Programming assignments | 40% |
| Attendance and Quizzes  | 10% |
| Midterm exam            | 20% |
| Final exam              | 30% |

Blackboard will be used to report grades and to post lecture notes and reading material. Up-to-date information on all other aspects of the course (assignment due dates, etc.) will be posted on this website.

## Schedule

The schedule is subject to revision.

| Week                        | Topic                                 | Reading                        | Assignment |
|-----------------------------|---------------------------------------|--------------------------------|------------|
| Week 1 (23 Aug) | Intro. to PL, Pyret | [PAPL 1](https://papl.cs.brown.edu/2020/Introduction.html), [3](https://papl.cs.brown.edu/2020/getting-started.html), [4.1-4.3](https://papl.cs.brown.edu/2020/Naming_Values.html), [5.1-5.3](https://papl.cs.brown.edu/2020/From_Repeated_Expressions_to_Functions.html) | [PA0: Intro. to Pyret](pa/0.md) (28 Aug) |
| Week 2 (30 Aug) | Natural numbers, induction, lists | [PAPL 6.1-6.5](https://papl.cs.brown.edu/2020/Conditionals_and_Booleans.html), [10](https://papl.cs.brown.edu/2020/processing-lists.html) | Q0 (3 Sep) |
| Week 3 (6 Sep) | Structured and conditional data, binary trees | [PAPL 11](https://papl.cs.brown.edu/2020/intro-struct-data.html) | [PA1: Lists](pa/1.md) (11 Sep) |
| Week 4 (13 Sep) | Collections, recursive data | [PAPL 12](https://papl.cs.brown.edu/2020/Collections_of_Structured_Data.html), [13](https://papl.cs.brown.edu/2020/Recursive_Data.html) | Q1 (17 Sep) |
| Week 5 (20 Sep) | Higher-order functions | [PAPL 15](https://papl.cs.brown.edu/2020/testing.html), [16](https://papl.cs.brown.edu/2020/func-as-data.html) | [PA2: Binary trees](pa/2.md) (25 Sep) |
| Week 6 (27 Sep) | Balanced BSTs | [PAPL 18](https://papl.cs.brown.edu/2020/set-representations.html) | Q2 (1 Oct) |
| Week 7 (4 Oct) | State, Equality | [PAPL 22](https://papl.cs.brown.edu/2020/State__Change__and_More_Equality.html) | [PA3: BSTs](pa/3.md) (9 Oct) |
| Week 8 (11 Oct) | Abstract syntax, parsing | [PAPL 24](https://papl.cs.brown.edu/2020/Processing_Programs__Parsing.html), Supplementary: TAPL 3 | Midterm Exam (15 Oct) |
| Week 9 (18 Oct) | Interpretation | [PAPL 25](https://papl.cs.brown.edu/2020/first-interp.html) | [PA4: Scheme0](pa/4.md) (23 Oct) |
| Week 10 (25 Oct) | Interpreting conditionals and functions | [PAPL 26](https://papl.cs.brown.edu/2020/growing-lang.html), [27](https://papl.cs.brown.edu/2020/Interpreting_Functions.html) | Q3 (29 Oct) |
| Week 11 (1 Nov) | Types, typing judgments, Supplementary: TAPL 8 | [PAPL 28](https://papl.cs.brown.edu/2020/types.html) | [PA5: Typed Scheme0](pa/5.md) (6 Nov) |
| Week 12 (8 Nov) | Types contd., type safety | [PAPL 29](https://papl.cs.brown.edu/2020/safety-soundness.html) | Q4 (12 Nov) |
| Week 13 (15 Nov) | Parametric polymorphism | [PAPL 30](https://papl.cs.brown.edu/2020/para-poly.html) | [PA6: Scheme1](pa/6.md) (20 Nov) |
| Week 14 (22 Nov) | TBD / Thanksgiving | TBD | No quiz |
| Week 15 (29 Nov) | TBD / Final review | TBD | No quiz -- study for finals! |
| Exam week (6 Dec) | **FINAL EXAM** | | |

Assignments are due in Blackboard at 11:59pm unless otherwise specified. **Q0**, **Q1**, etc., denote quizzes in Blackboard, generally due on the Fridays of weeks with no due programming assignments (PAs).

## Homework and Collaboration Policies

### Acceptable Collaboration Matrix

|            | Instructor/TA	| Noninstructor (e.g., Another Student) | 
|------------|----------------|---------------------------------------|
| ***You***  | All collaboration allowed | High-level discussion (of the problems, not your code!) allowed but only after you've started the assignment; must be documented in README as described below |

Unless otherwise noted, homeworks are due Saturdays by 11:59 p.m. Late homework assignments will be penalized according to the following formula:

* Up to 24 hours late: no deduction, for a max 2 late homeworks per student across the entire semester
* Homeworks later than 24 hours, or from students who have already turned in 2 late homeworks, will receive 0 points.

You may discuss the homework with other students in the class, but only after you've attempted the problems on your own first. If you do discuss the homework problems with others, write the names of the students you spoke with, along with a brief summary of what you discussed, in a README comment at the top of each submission. Example:

```
(* README Alex Bagnall, Assn #1 
I worked with X and Y. We swapped tips regarding the use of pattern-matching in Rust. *)
```

However, **under no circumstances are you permitted to share or directly copy code or other written homework material**, except with course instructors. The code and proofs you turn in must be your own. Remember: homework is there to give **you** practice in the new ideas and techniques covered by the course; it does you no good if you don't engage!

That said, if we find that you have cheated on an assignment in this course, you will immediately:

* Be referred to the Office of Community Standards (which may take disciplinary action against you, possibly expulsion); and
* Flunk the course (receive a final grade of F).

Students in EECS courses such as this one must adhere to the Russ College of Engineering and Technology [Honor Code](https://www.ohio.edu/engineering/academics/academic-integrity.cfm##code), and to the OU [Student Code of Conduct](http://www.ohio.edu/communitystandards/academic/students.cfm). If you haven't read these policies, do so now.

## Students with Disabilities

If you suspect you may need an accommodation based on the impact of a disability, please contact me privately to discuss your specific needs. If you're not yet registered as a student with a disability, contact the Office of Student Accessibility Services first.

## Student Outcomes vs. Course Learning Outcomes

1. An ability to analyze a complex computing problem and to apply principles of computing and other relevant disciplines to identify solutions. Students will be able to:
* Design and implement structured data types to solve computational problems
* Design and implement higher-order functions to solve computational problems
* Use pattern-matching to analyze and compute on structured data
* Use recursion to write functions that manipulate recursive collection types such as abstract syntax trees and lists
* Use polymorphism to implement a generic collection type such as a symbol table
* Analyze and reason equationally about a functional program in order to prove its correctness

6. An ability to apply computer science theory and software development fundamentals to produce computing-based solutions. Students will be able to:
* Apply understanding of grammars and syntax trees to implement a parser for an extended arithmetic expression language that conforms to a BNF specification
* Apply understanding of inductively defined data types, pattern-matching, recursion, and programming language semantics to implement an interpreter for an extended arithmetic expression language
* Apply understanding of type systems, type judgments, and inductively defined typing rules to implement a type checker for an extended arithmetic expression language
* Apply understanding of programming language design and implementation to extend an existing implementation of a language (parser, type checker, interpreter) to support new language features such as higher-order functions, mutable references, or closures
