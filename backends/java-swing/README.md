The Pianola agent instruments a running Java Swing desktop application.

When the application starts, the agent opens a network socket (default port 26060) and begins listening for queries and requests. Keep in mind that, as of now, the connection is **completely unsecured**!

The agent offers a "snapshot" method which serializes the current component hierarchy of the GUI and sends it back to the client. After analyzing the snapshot, the client can invoke methods to generate events on specific components, like for example clicking on a button. 

**Important**: Every change in the GUI, however minimal, must be followed by getting a new snapshot. Re-using a snapshot in the client to invoke several actions based on it won't work and should be discouraged by the client-side libraries.

When an snapshot is taken, the agent also stores in memory image captures of all the windows at the moment of the request. However, these images are not sent along with the snapshot for efficiency reasons. They are requested through an special method. When a new snapshot is taken, the old images are discarded.

Building the agent 
==================

Building the agent requires Maven. 

Invoke

> mvn install 

If the compilation is successful, the pianola-driver jar should appear in the **target** folder. Additionally, the **target/dependency** folder should contain **javassist** and **msgpack** jars (also a **json-simple** jar, but this isn't used for anything). 

Configuring the Application Under Test
======================================

The **pianola**, **javassist** and **msgpack** jars must be in the classpath of the AUT.

Additionally, when starting the AUT an argument of the following form must be passed to the JVM:

> -javaagent:C:\Path\To\Pianola\pianola-driver-1.0.jar=port/26060,popupTrigger/release

See the documentation of the [java.lang.instrument](http://docs.oracle.com/javase/6/docs/api/java/lang/instrument/package-summary.html) packgage for details on how to start agents from the command line.

The correct value of the **popupTrigger** parameter varies depending on the operating system. On Windows it is **popupTrigger/release**, Linux users should use **popupTrigger/press**. 
 
If configured correctly, the agent should start automatically with the application.



