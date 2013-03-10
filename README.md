Pianola is a Haskell library that lets you monitor and control Java Swing applications through a network connection. This can be useful for performing automated GUI testing. 

Pianola offers some of the functionality of tools like [Marathon](http://marathontesting.com/), altough it doesn't have any recording capabilities. The scripts must be written by hand. Also, unlike Marathon, Pianola doesn't concern itself with launching the application under test, which must be started through other means.

To work with the library, the application under test must have been instrumented with a Java-side agent. For instructions for how to compile an set up the agent, check the README file in the **backends/java-swing** folder.

A Java Swing test application is included in the **backends/java-swing-testapp** folder.

For instructions on how to use the library itself, check the Haddock documentation for the **Pianola.Tutorial** package.

The **examples** folder contains scripts for interacting with popular Java Swing applications (only **DbVisualizer** at the moment). Check the README there for instructions on how to set up the Pianola agent for these applications.      
