Db Visualizer
=============

Tested with **Db Visualizer 9.0.5**

http://www.dbvis.com/

Compile the Java agent. Copy **xanela-driver-1.0.jar**, **javassist-3.16.1-GA.jar** and **msgpack-0.6.6.jar** into the **lib/** folder of the Db Visualizer installation.

Edit file **dbvis.vmoptions** from the Db Visualizer installation and add a line like the following:

> -javaagent:C:\Progs\DbVisualizer\lib\pianola-driver-1.0.jar=port/26060,popupTrigger/release

In Linux, use **popupTrigger/press** instead of **popupTrigger/release**.


