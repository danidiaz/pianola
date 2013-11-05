Dir.chdir(File.join('backends','java-swing')) {
    puts `mvn install`
}

Dir.chdir(File.join('backends','java-swing-testapp')) {
    puts `mvn install`
    #pid = Process::spawn "mvn exec:exec"
    pid = Process::spawn 'java -cp "..\java-swing\target\dependency\*;target\*" -javaagent:..\java-swing\target\pianola-driver-1.0.jar=port/26060,popupTrigger/release info.danidiaz.pianola.testapp.Main'
}

Process::exec 'cabal install --enable-tests'

Process::kill(:SIGINT,pid)

# java -cp target\* info.danidiaz.pianola.robot.Main
