# Google Foobar
These are my solutions to the Google Foobar challenge, done first in Haskell and then in Python.

This repo's purpose is to share my thought process on these challenges for posterity and education.

This repo consists of two projects: a Stack-based Haskell project,
and a PyCharm Python project.

## ⚠️ WARNING: YOU VIEW THESE SOLUTIONS AT YOUR PERIL ⚠️
### THIS REPOSITORY IS *NOT* HERE FOR YOU TO CHEAT.




<iframe width="560" height="315" src="https://www.youtube.com/embed/OpKpE0J3Rgw" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>





## What is Google Foobar?
Google Foobar is a not-so-secret, invitation-only series of timed programming challenges by Google, located at https://foobar.withgoogle.com. You can discover an invitation to Google Foobar by googling certain programming-related topics or by snooping through the Google doodles. After enough time (and a little algorithmic luck), Google will present you with Google Foobar with a cool animation on a search page. Google sometimes offers interviews to and hires people who complete Google Foobar.

Google Foobar consists of 5 levels. Each level has a number of problems:
* Level 1 has 1 problem
* Level 2 has 2 problems
* Level 3 has 3 problems
* Level 4 has 2 problems
* Level 5 has 1 problem

The problems primarily involve data structures and algorithms, much like what you would find on Project Euler, LeetCode, or in a technical interview -- Google isn't going to, for instance, ask you in one problem to build a GUI or make a program in Assembly that prints a triangle of asterisks.

You have the option of implementing your solutions in either Java 8 or Python 2. Either way, you submit your solution directly in the browser in a vim-like editor, and then Google runs your solution against a set of tests, most of which are hidden. You must pass *all* of the tests in order to complete the problem. That being said, *avoid a O(n^2) solution at all costs.* At least one of those tests will time out if your solution is inefficient, but Google Foobar won't tell you *why* your test is failing.  

Each problem involves a lot of flavour text: it's your job to determine which details matter. Pay close attention to patterns in the problems.

It is even possible to hack on Google Foobar...

For information, please see:
https://medium.com/chingu/my-experience-with-the-google-foobar-challenge-and-tips-on-what-to-do-if-you-get-it-9848d31d3d20
https://medium.com/plutonic-services/things-you-should-know-about-google-foobar-invitation-703a535bf30f
https://www.quora.com/What-is-Google-Foobar-1

## How Did I Find Google Foobar?


## My Approach
In general, for these challenges, I wrote the initial implementation in Haskell and then translated that over to Python 3, and then adjusted it for Python 2, which is what Google Foobar accepts. I intend for the Haskell implementation to serve as a specification for the behaviour of the Python 2/3 programs.

This is very intentional: I have been knee-deep in functional programming land lately, and I have been having tons of fun working through the Project Euler problems in Haskell. I have found it to be easier to think about and work through algorithm problems in Haskell because the higher-order functions in Haskell allow me to abstract away a lot of boilerplate.

Even though Python is hostile to functional programming, I have found the functions in `itertools` to be sufficient. I have also liberally used `@functools.lru_cache()` to speed up the recursive Python functions in an effort to imitate the memoization GHC does for similar functions.

I have also included basic tests in the Python 3 programs. The `main` function simply runs these tests since Google Foobar doesn't use the `main` function in the Python 2 program.

Please note: I have not included the final Python 2 solutions because they are just trimmed-down versions of the Python 3 solutions.

## Build Instructions
### Haskell
0. Install Stack for your system. Stack will take care of the dependencies for this project, including downloading GHC.
1. Clone or download this repository.
2. `cd` into the `GoogleFoobarHS` directory.
3. Run `stack build`.
4. Run `stack ghci`. This will start a Haskell REPL in this project.
5. Run `<PROBLEM>.solution`, where <PROBLEM> is the problem you want to run, e.g., run `SolarDoomsday.solution` to run the solution for the Solar Doomsday problem.
6. (If you've never used GHCi before) Enter `:quit` when you want to quit the REPL.

### Python 3
If you have Python 3 installed, the choice is yours here:
you can hook the project up to PyCharm, or you can run the individual files on the command line.

There are no external dependencies.
Each problem has its own folder, and each folder has its own `main.py`.

## Challenges
----
### Level 1
----
#### Solar Doomsday
**Completed: 1 June 2020**

### Level 2
----
#### Lovely Lucky LAMBs
**Completed: 3 June 2020**

### Level 3
----


### Level 4
----


### Level 5
----