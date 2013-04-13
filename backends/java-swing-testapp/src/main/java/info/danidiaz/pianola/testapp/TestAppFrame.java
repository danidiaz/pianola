package info.danidiaz.pianola.testapp;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
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
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.SwingWorker;
import javax.swing.border.BevelBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

public class TestAppFrame extends JFrame {

    private final JTextField textField = new JTextField(18);
    
    private final JLabel statusLabel;
    private final JButton clearStatusLabel;        
    
    public TestAppFrame() throws HeadlessException {
        super("Test app frame");
        
        getContentPane().setLayout(new BorderLayout());
        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.addTab("tab one", createTabOne());
        tabbedPane.setToolTipTextAt(0, "tooltip for tab one");
        tabbedPane.addTab("tab two", createTabTwo());
        tabbedPane.setToolTipTextAt(1, "tooltip for tab two");
        JPanel panels [] = createTabJTree();
        tabbedPane.addTab("tab JTree a", panels[0]);
        tabbedPane.setToolTipTextAt(2, "tooltip for tab three");
        tabbedPane.addTab("tab JTree b", panels[1]);
        tabbedPane.setToolTipTextAt(3, "tooltip for tab four");
        tabbedPane.addTab("labels", createTabLabeledFields());
        tabbedPane.setToolTipTextAt(4, "tooltip for labels");

        getContentPane().add(tabbedPane, BorderLayout.CENTER);
                     
                       
        JPanel statusPanel = new JPanel(new BorderLayout());
        statusPanel.setBorder(new BevelBorder(BevelBorder.LOWERED));
        statusPanel.setPreferredSize(new Dimension(this.getWidth(), 36));
        //statusPanel.setLayout(new BoxLayout(statusPanel, BoxLayout.X_AXIS));
                
        statusLabel = new JLabel("");
        statusLabel.setName("status bar");
        statusLabel.setHorizontalAlignment(SwingConstants.LEFT);
        statusPanel.add(statusLabel,BorderLayout.CENTER);
        
        clearStatusLabel = new JButton("clear");
        clearStatusLabel.addActionListener(new ActionListener() {
            
            @Override
            public void actionPerformed(ActionEvent arg0) {
                    statusLabel.setText("");
                
            }
        });
        statusPanel.add(clearStatusLabel, BorderLayout.EAST);
        

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
                statusLabel.setText("clicked on submenuitem1");               
            }
        });
        subMenu.add(menuitem1);
        final JCheckBoxMenuItem checkBoxMenuItem = new JCheckBoxMenuItem("submenuitem2");
        checkBoxMenuItem.addItemListener(new ItemListener() {
            
            @Override
            public void itemStateChanged(ItemEvent arg0) {
                statusLabel.setText("checkbox in menu is now "+checkBoxMenuItem.isSelected());                
            }
        });
        subMenu.add(checkBoxMenuItem);
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
        
        JButton actionButton = new JButton("click this");
        actionButton.addActionListener(new ActionListener() {            
            @Override
            public void actionPerformed(ActionEvent arg0) {                
                statusLabel.setText("clicked button in dialog");
            }
        });        
        
        JButton dialogButton = new JButton("close dialog");
        dialogButton.addActionListener(new ActionListener() {
            
            @Override
            public void actionPerformed(ActionEvent e) {
                dialog.setVisible(false);
                
            }
        });
        dialog.getContentPane().add(actionButton,BorderLayout.NORTH);
        dialog.getContentPane().add(dialogButton,BorderLayout.SOUTH);
        //Display the window.
        dialog.pack();
        dialog.setLocationRelativeTo(null);
        dialog.setVisible(true);        
    }
   
    private JPanel createTabOne() {
        JPanel mainPanel = new JPanel(new BorderLayout());
        JPanel textPanel = new JPanel(new BorderLayout());
        
        textField.setText("En un lugar de la Mancha");
        textField.getDocument().addDocumentListener(new DocumentListener() {
            
            @Override
            public void removeUpdate(DocumentEvent arg0) {
                statusLabel.setText(textField.getText());                               
            }
            
            @Override
            public void insertUpdate(DocumentEvent arg0) {
                statusLabel.setText(textField.getText());                            
            }
            
            @Override
            public void changedUpdate(DocumentEvent arg0) {
                statusLabel.setText(textField.getText());                
            }
        });
        
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
        
        JPanel dialogButtonPanel = new JPanel(new GridLayout(2,1));
        dialogButtonPanel.add(fooButton);
        JButton slowDialogButton = new JButton("open slow dialog");
        slowDialogButton.addActionListener(new ActionListener() {
            
            @Override
            public void actionPerformed(ActionEvent arg0) {                       
                new DialogOpenDelayer(TestAppFrame.this,statusLabel).execute();                
            }
        });
        dialogButtonPanel.add(slowDialogButton);        
        
        textPanel.add(dialogButtonPanel,BorderLayout.SOUTH);
        
        JPanel westPanel = new JPanel(new GridLayout(6,1));
        JComboBox combo = new JComboBox(new Object [] { "aaa","bbb","ccc",
                "ddd",
                "eee",
                "fff",
                "ggg",
                "hhh",
                "iii",
                "111",
                "222",
                "333"                
            }); 
        combo.addItemListener(new ItemListener() {            
            @Override
            public void itemStateChanged(ItemEvent arg0) {
                statusLabel.setText("selected in combo: " + arg0.getItem());
                
            }
        });
                    
        westPanel.add(combo);
        final JCheckBox checkBox = new JCheckBox("This is a checkbox");
        checkBox.addItemListener(new ItemListener() {
            
            @Override
            public void itemStateChanged(ItemEvent arg0) {
                statusLabel.setText("checkbox is now "+checkBox.isSelected());                                
            }
        });
        westPanel.add(checkBox);
        
        
        JLabel label = new JLabel("This is a label");
        
        final JPopupMenu popup = new JPopupMenu();
        JMenuItem popupitem1 = new JMenuItem("popupitem1"); 
        JMenuItem popupitem2 = new JMenuItem("popupitem2");
        popupitem2.addActionListener(new ActionListener() {
            
            @Override
            public void actionPerformed(ActionEvent arg0) {
                statusLabel.setText("clicked on popupitem2");                
            }
        });
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
  
        JLabel label2 = new JLabel("click dbl click");
        label2.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                
                if (e.getClickCount() == 1) {
                    statusLabel.setText("clicked on label");
                } else if (e.getClickCount() == 2) {
                    statusLabel.setText("double-clicked on label");
                }
            }
        });
        westPanel.add(label2);
        
        final JFileChooser fc = new JFileChooser();
        final JButton fileChooserButton = new JButton("Open file chooser");
        fileChooserButton.addActionListener(new ActionListener() {
            
            @Override
            public void actionPerformed(ActionEvent arg0) {
                int returnVal = fc.showOpenDialog(fileChooserButton);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    File file = fc.getSelectedFile();
                    statusLabel.setText("file is: " + file);                    
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
        
        final JTable table = new JTable(new DefaultTableModel(               
                    new Object[][] {
                            new Object[] { "row1", 2, 3  },
                            new Object[] { "row2", 4 , 6 },
                            new Object[] { "row3", 6 , 7 },
                    },                
                    
                    new Object[] { "col1", "col2", "col3" }                              
                ));
        table.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            
            @Override
            public void valueChanged(ListSelectionEvent arg0) {
                ListSelectionModel lsm = (ListSelectionModel)arg0.getSource();
                if (lsm.isSelectionEmpty()) {
                } else {                    
                    int minIndex = lsm.getMinSelectionIndex();
                    statusLabel.setText("selected index in table: "+ minIndex);
                }                
            }
        });
        table.getModel().addTableModelListener(new TableModelListener() {
            
            @Override
            public void tableChanged(TableModelEvent arg0) {
                int row = arg0.getFirstRow();
                int col = arg0.getColumn();                
                Object o = table.getModel().getValueAt(row,col);
                statusLabel.setText("table value at row "+row+" col " +col + " is "+o);
            }
        });
        mainPanel.add(table,BorderLayout.CENTER);
        return mainPanel;
    }

    private JPanel[] createTabJTree() {
        JPanel[] panels = new JPanel[2];
        panels [0] = new JPanel(new BorderLayout());
        panels [1] = new JPanel(new BorderLayout());
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
        DefaultMutableTreeNode leafba = new DefaultMutableTreeNode("leaf b a");
        leafb.add(leafba);
        
        root.add(leafb);
        
        DefaultTreeModel model = new DefaultTreeModel(root);
      
        Object o [] = new Object[] { root, leafa };
        final TreePath treepath = new TreePath(o);
                               
        final JTree tree = new JTree(model);
        tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        tree.setRootVisible(true);
        tree.addTreeExpansionListener(new TreeExpansionListener() {
            
            @Override
            public void treeExpanded(TreeExpansionEvent arg0) {
                statusLabel.setText("leaf a is collapsed: "+tree.isCollapsed(treepath));                
            }
            
            @Override
            public void treeCollapsed(TreeExpansionEvent arg0) {
                statusLabel.setText("leaf a is collapsed: "+tree.isCollapsed(treepath));                
            }
        });
        panels[0].add(new JScrollPane(tree),BorderLayout.CENTER);
        
        final JTree tree2 = new JTree(model);
        tree2.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        tree2.setRootVisible(false);
        tree2.addTreeExpansionListener(new TreeExpansionListener() {
            
            @Override
            public void treeExpanded(TreeExpansionEvent arg0) {
                statusLabel.setText("leaf a is collapsed: "+tree2.isCollapsed(treepath));                
            }
            
            @Override
            public void treeCollapsed(TreeExpansionEvent arg0) {
                statusLabel.setText("leaf a is collapsed: "+tree2.isCollapsed(treepath));                
            }
        });        
        panels[1].add(new JScrollPane(tree2),BorderLayout.CENTER);
                
        return panels;
    }
    
    private JPanel createTabLabeledFields() {
        JPanel mainPanel = new JPanel(new BorderLayout());
        JPanel gridPanel = new JPanel(new GridLayout(3,2,10,10));
        gridPanel.add(new JLabel("label1"));
        
        final JTextField textField = new JTextField(10);
        textField.getDocument().addDocumentListener(new DocumentListener() {
            
            @Override
            public void removeUpdate(DocumentEvent arg0) {
                statusLabel.setText(textField.getText());                               
            }
            
            @Override
            public void insertUpdate(DocumentEvent arg0) {
                statusLabel.setText(textField.getText());                            
            }
            
            @Override
            public void changedUpdate(DocumentEvent arg0) {
                statusLabel.setText(textField.getText());                
            }
        });        
        gridPanel.add(textField);
        gridPanel.add(new JLabel("label2"));
        gridPanel.add(new JTextField(10));
        gridPanel.add(new JLabel("label3"));        
        gridPanel.add(new JButton("boo"));
        mainPanel.add(gridPanel,BorderLayout.NORTH);
        return mainPanel;
    }
    
    private static class DialogOpenDelayer extends SwingWorker<Object, Object> {

        JFrame frame;
        JLabel statusLabel;                     

        public DialogOpenDelayer(JFrame frame, JLabel statusLabel) {
            super();
            this.frame = frame;
            this.statusLabel = statusLabel;
        }

        @Override
        protected Object doInBackground() throws Exception {
            Thread.currentThread().sleep(3000);
            return "";
        }

        @Override
        protected void done() {
            super.done();
            
            final JDialog dialog = new JDialog(frame,true);
            dialog.setTitle("slow dialog");
            dialog.getContentPane().setLayout(new BorderLayout());
            dialog.getContentPane().add(new JTextArea(18, 10),BorderLayout.CENTER);
            
            JButton dialogButton = new JButton("close dialog");
            dialogButton.addActionListener(new ActionListener() {
                
                @Override
                public void actionPerformed(ActionEvent e) {
                    new DialogCloseDelayer(dialog, statusLabel).execute();
                    
                }
            });
            dialog.getContentPane().add(dialogButton,BorderLayout.SOUTH);
            //Display the window.
            dialog.pack();
            dialog.setLocationRelativeTo(null);
            dialog.setVisible(true);        
        }                
    }
    
    private static class DialogCloseDelayer extends SwingWorker<Object, Object> {

        JDialog dialog;
        JLabel statusLabel;

        public DialogCloseDelayer(JDialog dialog,JLabel statusLabel) {
            super();
            this.dialog = dialog;
            this.statusLabel = statusLabel;
        }

        @Override
        protected Object doInBackground() throws Exception {
            Thread.currentThread().sleep(3000);
            return "";
        }

        @Override
        protected void done() {
            super.done();
            statusLabel.setText("Performed delayed close");
            dialog.setVisible(false);
        }                
    }
}
