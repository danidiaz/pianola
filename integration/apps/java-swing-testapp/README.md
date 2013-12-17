This is a test Java Swing application for the Pianola agent. 

Compiling the test application
==============================

The pianola agent must have been previously compiled with **mvn install** so that its jar is accessible in the local maven repository.

Before compiling the application, edit the **pom.xml** file. Find the line:

>  <argument>-javaagent:${settings.localRepository}/info/danidiaz/pianola/pianola-driver/${myprops.pianola-driver.version}/pianola-driver-${myprops.pianola-driver.version}.jar=port/26060,popupTrigger/release</argument>

In Linux, **popupTrigger/release** should be changed to **popupTrigger/press**.

Compile the application with

> mvn install

Executing the test application
==============================

Once compiled, execute the application with 

> mvn exec:exec

The GUI should appear, and client libaries should be able to interact with it through the pianola agent.
