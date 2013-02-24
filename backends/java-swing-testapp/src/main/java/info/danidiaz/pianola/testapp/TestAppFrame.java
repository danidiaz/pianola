package info.danidiaz.pianola.testapp;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GraphicsConfiguration;
import java.awt.GridLayout;
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.SwingConstants;
import javax.swing.border.BevelBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;

public class TestAppFrame extends JFrame {

    private final JTextField textField = new JTextField(18);
    private final JLabel statusLabel;
    
    public TestAppFrame() throws HeadlessException {
        super("Test app frame");
        
        getContentPane().setLayout(new BorderLayout());
        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.addTab("tab one", createTabOne());
        tabbedPane.setToolTipTextAt(0, "tooltip for tab one");
        tabbedPane.addTab("tab two", createTabTwo());
        tabbedPane.setToolTipTextAt(1, "tooltip for tab two");
        tabbedPane.addTab("tab JTree a", createTabJTree(true));
        tabbedPane.setToolTipTextAt(2, "tooltip for tab three");
        tabbedPane.addTab("tab JTree b", createTabJTree(false));
        tabbedPane.setToolTipTextAt(3, "tooltip for tab four");
        tabbedPane.addTab("labels", createTabLabeledFields());
        tabbedPane.setToolTipTextAt(4, "tooltip for labels");

        getContentPane().add(tabbedPane, BorderLayout.CENTER);
                     
        
        JPanel statusPanel = new JPanel();
        statusPanel.setBorder(new BevelBorder(BevelBorder.LOWERED));
        statusPanel.setPreferredSize(new Dimension(this.getWidth(), 16));
        statusPanel.setLayout(new BoxLayout(statusPanel, BoxLayout.X_AXIS));
                
        statusLabel = new JLabel("status");
        statusLabel.setName("status label");
        statusLabel.setHorizontalAlignment(SwingConstants.LEFT);
        statusPanel.add(statusLabel);

        getContentPane().add(statusPanel, BorderLayout.SOUTH);
                
        JMenu menu = new JMenu("Menu1");
        JMenuItem item1 = new JMenuItem("item1"); 
        JMenuItem item2 = new JMenuItem("item2");        
        menu.add(item1);
        menu.add(item2);
        JMenu subMenu = new JMenu("SubMenu1");     
        JMenuItem menuitem1 = new JMenuItem("submenuitem1");
        menuitem1.addActionListener(new ActionListener() {
            
            @Override
            public void actionPerformed(ActionEvent arg0) {
                textField.setText("nananiero nananiero nananiero");
                
            }
        });
        subMenu.add(menuitem1);
        subMenu.add(new JCheckBoxMenuItem("submenuitem2"));
        menu.add(subMenu);         
        
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(menu);
        setJMenuBar(menuBar);
        
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
                
        

    }
    
    private void createAndShowDialog(JFrame frame) {
        final JDialog dialog = new JDialog(frame,true);
        dialog.setTitle("foo dialog");
        dialog.getContentPane().setLayout(new BorderLayout());
        dialog.getContentPane().add(new JTextArea(18, 10),BorderLayout.CENTER);
        JButton dialogButton = new JButton("close dialog");
        dialogButton.addActionListener(new ActionListener() {
            
            @Override
            public void actionPerformed(ActionEvent e) {
                dialog.setVisible(false);
                
            }
        });
        dialog.getContentPane().add(dialogButton,BorderLayout.NORTH);
        //Display the window.
        dialog.pack();
        dialog.setLocationRelativeTo(null);
        dialog.setVisible(true);        
    }
   
    private JPanel createTabOne() {
        JPanel mainPanel = new JPanel(new BorderLayout());
        JPanel textPanel = new JPanel(new BorderLayout());
        
        textPanel.add(textField,BorderLayout.NORTH); 
        
        textPanel.add(new JTextArea(18, 28),BorderLayout.CENTER);
        JButton fooButton = new JButton("open dialog");
        fooButton.setToolTipText("open dialog tooltip");
        fooButton.addActionListener(new ActionListener() {
            
            @Override
            public void actionPerformed(ActionEvent e) {
                
                createAndShowDialog(TestAppFrame.this);
            }
        });
        textPanel.add(fooButton,BorderLayout.SOUTH);
        
        JPanel westPanel = new JPanel(new GridLayout(6,1));
        westPanel.add(new JComboBox(new Object [] { "aaa","bbb","ccc",
                "ddd",
                "eee",
                "fff",
                "ggg",
                "hhh",
                "iii",
                "111",
                "222",
                "333"                
            }));
        westPanel.add(new JCheckBox("This is a checkbox"));
        
        
        JLabel label = new JLabel("This is a label");
        
        final JPopupMenu popup = new JPopupMenu();
        JMenuItem popupitem1 = new JMenuItem("popupitem1"); 
        JMenuItem popupitem2 = new JMenuItem("popupitem2");
        popup.add(popupitem1);
        popup.add(popupitem2);
        label.addMouseListener(new  MouseAdapter() {
            
            @Override
            public void mousePressed(MouseEvent e){
                if (e.isPopupTrigger())
                    doPop(e);
            }
            @Override
            public void mouseReleased(MouseEvent e){
                if (e.isPopupTrigger())
                    doPop(e);
            }

            private void doPop(MouseEvent e){                
                popup.show(e.getComponent(), e.getX(), e.getY());
            }
             
        });
      
        westPanel.add(label);  
  
        final JFileChooser fc = new JFileChooser();
        final JButton fileChooserButton = new JButton("Open file chooser");
        fileChooserButton.addActionListener(new ActionListener() {
            
            @Override
            public void actionPerformed(ActionEvent arg0) {
                int returnVal = fc.showOpenDialog(fileChooserButton);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    File file = fc.getSelectedFile();
                    System.out.println("file is: " + file);                    
                } 
            }
        });
        westPanel.add(fileChooserButton);
       
        mainPanel.add(textPanel,BorderLayout.CENTER);        
        mainPanel.add(westPanel,BorderLayout.EAST);
        
        return mainPanel;
    }
    
    private JPanel createTabTwo() {
        JPanel mainPanel = new JPanel(new BorderLayout());
        
        JTable table = new JTable(new DefaultTableModel(               
                    new Object[][] {
                            new Object[] { "row1", 2, 3  },
                            new Object[] { "row2", 4 , 6 },
                            new Object[] { "row3", 6 , 7 },
                    },                
                    
                    new Object[] { "col1", "col2", "col3" }                              
                ));
        mainPanel.add(table,BorderLayout.CENTER);
        return mainPanel;
    }

    private JPanel createTabJTree(boolean showroot) {
        JPanel mainPanel = new JPanel(new BorderLayout());
        DefaultMutableTreeNode root = new DefaultMutableTreeNode("this is the root");
        DefaultMutableTreeNode leafa = new DefaultMutableTreeNode("leaf a");
        root.add(leafa);
        DefaultMutableTreeNode leafaa = new DefaultMutableTreeNode("leaf a a");
        leafa.add(leafaa);
        
        DefaultMutableTreeNode leafaaa = new DefaultMutableTreeNode("leaf a a a");
        DefaultMutableTreeNode leafaab = new DefaultMutableTreeNode("leaf a a b");
        DefaultMutableTreeNode leafaac = new DefaultMutableTreeNode("leaf a a c");
        leafaa.add(leafaaa);
        leafaa.add(leafaab);
        leafaa.add(leafaac);
        
        DefaultMutableTreeNode leafab = new DefaultMutableTreeNode("leaf a b");
        leafa.add(leafab);
        DefaultMutableTreeNode leafb = new DefaultMutableTreeNode("leaf b");
        root.add(leafb);
        
        DefaultTreeModel model = new DefaultTreeModel(root);
      
        JTree tree = new JTree(model);
        tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        tree.setRootVisible(showroot);
        mainPanel.add(new JScrollPane(tree),BorderLayout.CENTER);
        return mainPanel;
    }
    
    private JPanel createTabLabeledFields() {
        JPanel mainPanel = new JPanel(new BorderLayout());
        JPanel gridPanel = new JPanel(new GridLayout(3,2,10,10));
        gridPanel.add(new JLabel("label1"));
        gridPanel.add(new JTextField(10));
        gridPanel.add(new JLabel("label2"));
        gridPanel.add(new JTextField(10));
        gridPanel.add(new JLabel("label3"));        
        gridPanel.add(new JButton("boo"));
        mainPanel.add(gridPanel,BorderLayout.NORTH);
        return mainPanel;
    }
    
}
