package info.danidiaz.pianola.testapp;

import javax.swing.JFrame;

public class Main 
{       
    public static void main( String[] args )
    {
        System.out.println( "Hello World!" );
        
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                new Main().createAndShowGUI();
            }
        });
    }
        
    private void createAndShowGUI() {
        //Create and set up the window.
        final JFrame frame = new TestAppFrame();
        
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }

}
