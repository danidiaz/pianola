Dir.chdir(File.join('backends','java-swing')) {
    puts `mvn install`
}
Dir.chdir(File.join('backends','java-swing-testapp')) {
    puts `mvn install`
    pid = Process::spawn "mvn exec:exec"
    Process::detach pid
}
Process::exec 'cabal install --enable-tests'

