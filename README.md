Pianola is a Haskell library that lets you monitor and control Java Swing applications through a network connection. This can be useful for performing automated GUI testing. 

To work with the library, the Application Under Test (AUT) must have been instrumented with a Java-side Pianola agent. For instructions on how to compile an set up the agent, check the README file in the **backends/java-swing** folder.

A Java Swing example application is included in the **backends/java-swing-testapp** folder.

For instructions on how to use the library, check out the Haddock documentation for the **Pianola.Tutorial** package.

The **tests** folder contains automated tests for the library. For the tests to work, the Java example application must be up and running.

The **examples** folder contains scripts for interacting with popular Java Swing applications (only **DbVisualizer** at the moment). Check out the corresponding README for instructions on how to set up the Pianola agent for these applications.      

Pianola offers some of the functionality of tools like [Marathon](http://marathontesting.com/), although it doesn't have any recording capabilities. The scripts must be written by hand. Also, unlike Marathon, Pianola doesn't concern itself with launching the AUT, which must be started through other means.

Rationale
=========

* I dislike the "recording" approach taken by many GUI test automation frameworks for developing tests scripts. In any minimally complex test case, you will have to refactor the script aggresively anyway, to remove duplication and increase modularity. I prefer to program the scripts in an incremental manner, ideally with the help of a good REPL. 

* Sometimes, it can become inconvenient if the test framework needs to control the launch of the AUT, instead of having it started by other means.

* Pianola doesn't require the client and the AUT to reside in the same machine.

* I'm partial to statically typed languages. I find very annoying when a long-running script developed in a dynamically typed language fails midway due to a dumb error like the misspelling of a variable's name. I realize that this inconvenience is solved through unit testing, but when then things you are developing are *themselves* test scripts, isn't it a bit of overkill to have tests of tests?

* Related to the previous point: statically typed languages give you more assurances that you don't break anything when refactoring.

* Haskell is (at least in theory) good at handling tree-like structures, like the containment hierarchy of the components in a GUI.

* Haskell's higher-order functions should (at least in theory) give more flexibility when locating individual components. For example, identifying a component depending on an arbitrary predicate applied to its text. 

* [Marathon](http://marathontesting.com/) is a versatile tool that can handle many corner cases when testing Swing GUIs. Still, there are annoyances. There isn't (to my knowledge) an easy way to identify a component by its text in other manner than by total equality.

* Marathon annoyance #2: There isn't (to my knowledge) an easy way to handle components which may or may not appear during a test's execution. For example, a warning dialog which appears only occasionally. There doesn't seem to be a "find component, if it exists" operation. 

* Marathon annoyance #3: There isn't (to my knowledge) a way to make Marathon scripts fail quickly when they can't find a component in the GUI. Sometimes they block for a surprisingly long time before failing!


