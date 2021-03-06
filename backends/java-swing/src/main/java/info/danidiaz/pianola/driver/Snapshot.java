package info.danidiaz.pianola.driver;

import java.awt.Component;
import java.awt.Container;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JList;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.JTree;
import javax.swing.ListCellRenderer;
import javax.swing.RootPaneContainer;
import javax.swing.SwingUtilities;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableModel;
import javax.swing.text.JTextComponent;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.msgpack.packer.Packer;

public class Snapshot {
    
    private ImageBin imageBin;
 
    private List<Window> windowArray = new ArrayList<Window>();
    private Map<Window,BufferedImage> windowImageMap = new HashMap<Window,BufferedImage>();
    
    private List<Component> componentArray = new ArrayList<Component>();
    
    boolean releaseIsPopupTrigger;
    
    public Snapshot(Snapshot pianola, boolean releaseIsPopupTrigger) {
        this.imageBin = pianola==null ? new ImageBin() : pianola.obtainImageBin();
        this.releaseIsPopupTrigger = releaseIsPopupTrigger;
    }
    public void buildAndWrite(final Packer packer) throws IOException {
        
        try {
            SwingUtilities.invokeAndWait(new Runnable() {
                
                @Override
                public void run() {
                    try {
                        Window warray[] = Window.getOwnerlessWindows();
                        writeWindowArray(packer, warray);
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            });
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        } finally {
            this.imageBin.flush();
        }
    }
    
    private static int countShowing(Component[] warray) {
        int visibleCount = 0;
        for (int i=0;i<warray.length;i++) {                            
            if (warray[i].isShowing()) {
                visibleCount++;
            }
        }
        return visibleCount;
    }
    
    private void writeWindowArray(Packer packer, Window warray[]) throws IOException {
        packer.writeArrayBegin(countShowing(warray));
        for (int i=0;i<warray.length;i++) {
            Window w = warray[i];
            if (w.isShowing()) {
                writeWindow(packer,w);
            }        
        }
        packer.writeArrayEnd();
    }
    
    private void writeWindow(Packer packer, Window w) throws IOException {
        
        int windowId = windowArray.size();
        windowArray.add(w);
        BufferedImage image = imageBin.obtainImage(w.getSize());
        w.paint(image.getGraphics());
        windowImageMap.put(w, image);
        
        packer.write((int)windowId);
        
        String title = "";
        if (w instanceof JFrame) {
            title = ((JFrame)w).getTitle();
        } else if (w instanceof JDialog) {
            title = ((JDialog)w).getTitle();                                    
        }

        packer.write(title);
        packer.writeArrayBegin(2);
        {
            Point loc = w.getLocationOnScreen();
            packer.write((int)loc.getX());
            packer.write((int)loc.getY());
        }
        packer.writeArrayEnd();
        packer.writeArrayBegin(2);
        {
            packer.write((int)w.getHeight());
            packer.write((int)w.getWidth());
        }
        packer.writeArrayEnd();
        
        writeMenuBar(packer, w);
        
        writePopupLayer(packer,w);
                        
        RootPaneContainer rpc = (RootPaneContainer)w;
        writeComponent(packer, (Component) rpc.getContentPane(), w, false);                                                               
        
        writeWindowArray(packer, w.getOwnedWindows());
    }
    
    private void writeMenuBar(Packer packer, Window w) throws IOException {        
        JMenuBar menubar = null;
        if (w instanceof JFrame) {
            menubar = ((JFrame)w).getJMenuBar();
        } else if (w instanceof JDialog) {
            menubar = ((JDialog)w).getJMenuBar();                                    
        }
        if (menubar==null) {
            packer.writeArrayBegin(0);
            packer.writeArrayEnd();
        } else {
            packer.writeArrayBegin(menubar.getMenuCount());
            for (int i=0; i<menubar.getMenuCount();i++) {
                writeComponent(packer,menubar.getMenu(i), w, false);
            }
            packer.writeArrayEnd();

        }                
    }
    
    private void writePopupLayer(Packer packer, Window w) throws IOException {
        Component[] popupLayerArray = new Component[] {};
        if (w instanceof JFrame) {
            popupLayerArray = ((JFrame)w).getLayeredPane().getComponentsInLayer(JLayeredPane.POPUP_LAYER);
        } else if (w instanceof JDialog) {
            popupLayerArray = ((JDialog)w).getLayeredPane().getComponentsInLayer(JLayeredPane.POPUP_LAYER);                                    
        }
        packer.writeArrayBegin(countShowing(popupLayerArray));        
        for (int i=0;i<popupLayerArray.length;i++) {
            Component c = (Component) popupLayerArray[i];
            if (c.isShowing()) {
                writeComponent(packer, c, w, false);    
            }
        }
        packer.writeArrayEnd();
    }
        
    private void writeComponent(Packer packer, Component c, Component coordBase, boolean isRenderer) throws IOException {
        
        int componentId = componentArray.size();
        componentArray.add(c);
        
        packer.write((int)componentId);
        
        packer.writeArrayBegin(2);
        {
            if (isRenderer) { // getLocationOnScreen does not work on renderers.
                packer.write((int)0);
                packer.write((int)0);
            } else {
                Point loc = c.getLocationOnScreen();
                packer.write((int)loc.x);
                packer.write((int)loc.y);
            } 
        }
        packer.writeArrayEnd();
        
        packer.writeArrayBegin(2);
        {
            packer.write((int)c.getHeight());
            packer.write((int)c.getWidth());
        }
        packer.writeArrayEnd();
        
        writePotentiallyNullString(packer,c.getName());
        String tooltipText = (c instanceof JComponent) ? ((JComponent)c).getToolTipText() : "";
        writePotentiallyNullString(packer,tooltipText);
        
        if (c instanceof AbstractButton) {
            writePotentiallyNullString(packer,((AbstractButton)c).getText());
        } else if (c instanceof JLabel) {
            writePotentiallyNullString(packer,((JLabel)c).getText());
        } else if (c instanceof JTextComponent) {
            writePotentiallyNullString(packer,((JTextComponent)c).getText());
        } else {
            packer.writeNil();
        }

        packer.write(c.isEnabled());        
        
        writeComponentType(packer, c, coordBase);
        
        Component children[] = new Component[]{};
        if (c instanceof Container) {            
            children = ((Container)c).getComponents();
        }
                              
        packer.writeArrayBegin(countShowing(children));
        for (int i=0;i<children.length;i++) {
            if (children[i].isShowing()) {                                
                writeComponent(packer, (Component)children[i],coordBase,isRenderer);
            }
        }
        packer.writeArrayEnd();
    }
    
    private void writeComponentType(Packer packer, 
                Component c, 
                Component coordBase 
            ) throws IOException 
    {
        if (c instanceof JPanel) {
            packer.write((int)1);
        } else if (c instanceof JToggleButton || c instanceof JCheckBoxMenuItem || c instanceof JRadioButtonMenuItem) {
            packer.write((int)2);
            packer.write(((AbstractButton)c).isSelected());                 
        } else if (c instanceof AbstractButton) { // normal button, not toggle button
            packer.write((int)3);
        } else if (c instanceof JTextField ) {
            packer.write((int)4);
            JTextField textField = (JTextField) c;
            packer.write((boolean)textField.isEditable());
        } else if (c instanceof JLabel) {
            packer.write((int)5);
        } else if (c instanceof JComboBox) {
            packer.write((int)6);

            JComboBox comboBox = (JComboBox)c;
            ListCellRenderer renderer = comboBox.getRenderer();
            JList dummyJList = new JList();

            if (comboBox.getSelectedIndex()==-1) {
                packer.writeNil();
            } else {
                Component cell = (Component)renderer.getListCellRendererComponent(dummyJList, 
                                comboBox.getModel().getElementAt(comboBox.getSelectedIndex()), 
                                comboBox.getSelectedIndex(), 
                                false, 
                                false
                            );
                writeComponent(packer, cell, coordBase,true);
            }                          
                       
        } else if (c instanceof JList) {
            packer.write((int)7);
            JList list = (JList) c;            
            ListCellRenderer renderer = list.getCellRenderer();
            
            packer.writeArrayBegin((int)list.getModel().getSize());
            for (int rowid=0; rowid<list.getModel().getSize(); rowid++) {
                Point loc = list.getLocationOnScreen();
                Rectangle cellBounds = list.getCellBounds(rowid, rowid);
                writeCell(  packer, 
                            rowid, 
                            0,
                            loc.x + cellBounds.x,
                            loc.y + cellBounds.y,                            
                            (Component)renderer.getListCellRendererComponent(list, 
                                    list.getModel().getElementAt(rowid), 
                                    rowid,
                                    false, 
                                    false
                                ), 
                            coordBase,
                            false
                            );                                
            }
            packer.writeArrayEnd();
            
        } else if (c instanceof JTable) {
            packer.write((int)8);
            JTable table = (JTable) c;
            TableModel model = table.getModel();
            
            int rowcount = model.getRowCount();
            int columncount = model.getColumnCount();
            packer.writeArrayBegin(columncount);            
            for (int j=0;j<columncount;j++) {            
                packer.writeArrayBegin(rowcount);
                for (int i=0;i<rowcount;i++) {
                    Point loc = table.getLocationOnScreen();
                    Rectangle cellBounds = table.getCellRect(i, j, false);
                    
                    TableCellRenderer renderer = table.getCellRenderer(i, j);                    
                    writeCell(  
                            packer, 
                            i, 
                            j, 
                            loc.x + cellBounds.x,
                            loc.y + cellBounds.y,                              
                            (Component)renderer.getTableCellRendererComponent(table, 
                                    model.getValueAt(i, j),  
                                    false, 
                                    false,
                                    i,
                                    j
                                ), 
                            coordBase,
                            false 
                            );                                                                        
                }
                packer.writeArrayEnd();
            }                        
            packer.writeArrayEnd();            
            
        } else if (c instanceof JTree) {
            packer.write((int)9);
            JTree tree = (JTree) c;
            TreeModel model = tree.getModel();
            TreeCellRenderer renderer = tree.getCellRenderer();
            
            packer.writeArrayBegin(tree.isRootVisible()?1:model.getChildCount(model.getRoot()));
            int basepathcount = tree.isRootVisible()?1:2;
            int expectedpathcount = basepathcount;
            for (int rowid=0;rowid<tree.getRowCount();rowid++) {
                TreePath path = tree.getPathForRow(rowid);
                if (path.getPathCount()<expectedpathcount) {
                    for (int i=0; i < expectedpathcount - path.getPathCount();i++) {
                        packer.writeArrayEnd();
                    }
                    expectedpathcount = path.getPathCount();
                }                
                Point loc = tree.getLocationOnScreen();
                Rectangle cellBounds = tree.getRowBounds(rowid);
                writeCell(  
                        packer, 
                        rowid, 
                        0, 
                        loc.x + cellBounds.x,
                        loc.y + cellBounds.y,                         
                        (Component)renderer.getTreeCellRendererComponent(
                                tree,
                                path.getLastPathComponent(),
                                tree.isRowSelected(rowid),
                                tree.isExpanded(rowid),
                                model.isLeaf(path.getLastPathComponent()),
                                rowid,
                                true
                            ), 
                        coordBase,
                        true
                        );                                                 
                
                if (tree.isExpanded(rowid)) {
                    packer.writeArrayBegin(model.getChildCount(path.getLastPathComponent()));
                    expectedpathcount++;
                } else {
                    packer.writeArrayBegin(0);
                    packer.writeArrayEnd();   
                }                
            }
            for (int i=0; i < expectedpathcount - basepathcount;i++) {
                packer.writeArrayEnd();
            }
            packer.writeArrayEnd();
            
        } else if (c instanceof JPopupMenu) {                    
            packer.write((int)50);
        
        } else if (c instanceof JTabbedPane) {
            packer.write((int)70);
            JTabbedPane tpane = (JTabbedPane)c;
            packer.writeArrayBegin(tpane.getTabCount());
            for (int i=0; i<tpane.getTabCount();i++) {
                packer.write((int)i);
                packer.write(tpane.getTitleAt(i));
                writePotentiallyNullString(packer,tpane.getToolTipTextAt(i));
                packer.write(i==tpane.getSelectedIndex());
            }
            packer.writeArrayEnd();
        } else {
            packer.write((int)77);
            packer.write(c.getClass().getName());
        }
    }
    
    private void writeCell(
                Packer packer, 
                int rowid, 
                int colid, 
                int xPos,
                int yPos,
                Component rendererc, 
                Component coordBase,
                boolean belongsToJTree 
            ) throws IOException 
    {
        packer.write((int)rowid);
        packer.write((int)colid);
        
        packer.writeArrayBegin(2);
        packer.write((int)xPos);
        packer.write((int)yPos);
        packer.writeArrayEnd();
        
        packer.writeArrayBegin(2);
        packer.write((int)rendererc.getHeight());
        packer.write((int)rendererc.getWidth());
        packer.writeArrayEnd();
        
        writeComponent(packer, rendererc, coordBase, true);
        packer.write((boolean)belongsToJTree);
    }
    
    
    private static void writePotentiallyNullString(Packer packer, String s) throws IOException {
        if (s==null) {
            packer.writeNil();
        } else {
            packer.write(s);
        }
    }

   public void click(int componentid) {
        
        final Component c = (Component)componentArray.get(componentid);
        Point point = new Point(c.getWidth()/2,c.getHeight()/2);
        postMouseEvent(c, MouseEvent.MOUSE_ENTERED, 0, point, 0, false);
        pressedReleasedClicked1(c, new Rectangle(0, 0, c.getWidth(), c.getHeight()), 1);
    }
    
    public void doubleClick(int componentid) {
        
        final Component c = (Component)componentArray.get(componentid);
        Point point = new Point(c.getWidth()/2,c.getHeight()/2);
        postMouseEvent(c, MouseEvent.MOUSE_ENTERED, 0, point, 0, false);
        Rectangle rect =  new Rectangle(0, 0, c.getWidth(), c.getHeight());
        pressedReleasedClicked1(c, rect, 1);
        pressedReleasedClicked1(c, rect, 2);
    }
    
    public void rightClick(final int componentid) {
        // http://stackoverflow.com/questions/5736872/java-popup-trigger-in-linux
        final Component button = (Component)componentArray.get(componentid);
        
        Point point = new Point(button.getWidth()/2,button.getHeight()/2);

        postMouseEvent(button, MouseEvent.MOUSE_ENTERED, 0, point, 0, false);
        postMouseEvent(button, MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON3_MASK, point, 1, !releaseIsPopupTrigger);
        postMouseEvent(button, MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON3_MASK, point, 1, releaseIsPopupTrigger);
        postMouseEvent(button, MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON3_MASK, point, 1, false); 
    }    
    
    public void clickButton(int buttonId) {
        
        final AbstractButton button = (AbstractButton)componentArray.get(buttonId);
        Point point = new Point(button.getWidth()/2,button.getHeight()/2);
        postMouseEvent(button, MouseEvent.MOUSE_ENTERED, 0, point, 0, false);
        pressedReleasedClicked1(button, new Rectangle(0, 0, button.getWidth(), button.getHeight()), 1);
    }
    
    public void toggle(final int buttonId, final boolean targetState) {

        SwingUtilities.invokeLater(new Runnable() {
            
            @Override
            public void run() {
                final AbstractButton button = (AbstractButton)componentArray.get(buttonId);
                
                if (button.isSelected() != targetState) {
                    clickButton(buttonId);
                }                 
            }
        });

    }        

    public void clickCombo(final int buttonId) {
        
        SwingUtilities.invokeLater(new Runnable() {
            
            @Override
            public void run() {
                final JComboBox button = (JComboBox)componentArray.get(buttonId);
                button.showPopup();
            }
        });                 
    }    
    
    public void setTextField(final int componentid, final String text) {
        
        SwingUtilities.invokeLater(new Runnable() {
            
            @Override
            public void run() {
                final JTextField textField = (JTextField)componentArray.get(componentid);
                textField.setText(text);
            }
        });                 
    }
    
    public void clickCell(final int componentid, final int rowid, final int columnid) {

        SwingUtilities.invokeLater(new Runnable() {
            
            @Override
            public void run() {        
                    final Component component = componentArray.get(componentid);
                    Rectangle bounds = new Rectangle(0,0,0,0);
                    if (component instanceof JList) {
                        JList list = (JList) component;
                        bounds = list.getCellBounds(rowid, rowid);
                        list.ensureIndexIsVisible(rowid);
                    } else if (component instanceof JTable) {
                        JTable table = (JTable) component;            
                        bounds = table.getCellRect(rowid, columnid, false);
                        table.scrollRectToVisible(bounds);
                    } else if (component instanceof JTree) {
                        JTree tree = (JTree) component;
                        bounds = tree.getRowBounds(rowid);
                        tree.scrollRowToVisible(rowid);            
                    } else {
                        throw new RuntimeException("can't handle component");
                    }
                    pressedReleasedClicked1(component, bounds, 1);
            }
        });                 
                    
    }
    
    public void doubleClickCell(final int componentid, final int rowid, final int columnid) {

        SwingUtilities.invokeLater(new Runnable() {
            
            @Override
            public void run() {   
                    final Component component = componentArray.get(componentid);
                    Rectangle bounds = new Rectangle(0,0,0,0);
                    if (component instanceof JList) {
                        JList list = (JList) component;
                        bounds = list.getCellBounds(rowid, rowid);
                        list.ensureIndexIsVisible(rowid);
                    } else if (component instanceof JTable) {
                        JTable table = (JTable) component;            
                        bounds = table.getCellRect(rowid, columnid, false);
                        table.scrollRectToVisible(bounds);
                    } else if (component instanceof JTree) {
                        JTree tree = (JTree) component;
                        bounds = tree.getRowBounds(rowid);
                        tree.scrollRowToVisible(rowid);                        
                    } else {
                        throw new RuntimeException("can't handle component");
                    }
                    pressedReleasedClicked1(component, bounds, 1);
                    pressedReleasedClicked1(component, bounds, 2);
            }
        });                         
    }
    
    public void rightClickCell(final int componentid, final int rowid, final int columnid) {

        SwingUtilities.invokeLater(new Runnable() {
            
            @Override
            public void run() {   
                    final Component component = componentArray.get(componentid);
                    Rectangle bounds = new Rectangle(0,0,0,0);
                    if (component instanceof JList) {
                        JList list = (JList) component;
                        bounds = list.getCellBounds(rowid, rowid);
                        list.ensureIndexIsVisible(rowid);
                    } else if (component instanceof JTable) {
                        JTable table = (JTable) component;            
                        bounds = table.getCellRect(rowid, columnid, false);
                        table.scrollRectToVisible(bounds);
                    } else if (component instanceof JTree) {
                        JTree tree = (JTree) component;
                        bounds = tree.getRowBounds(rowid);
                        tree.scrollRowToVisible(rowid);                        
                    } else {
                        throw new RuntimeException("can't handle component");
                    }
                    
                    Point point = new Point(bounds.x + bounds.width/2,bounds.y + bounds.height/2);

                    postMouseEvent(component, MouseEvent.MOUSE_ENTERED, 0, point, 0, false);
                    postMouseEvent(component, MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON3_MASK, point, 1, !releaseIsPopupTrigger);
                    postMouseEvent(component, MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON3_MASK, point, 1, releaseIsPopupTrigger);
                    postMouseEvent(component, MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON3_MASK, point, 1, false); 
            }
        });                         
    }
    
    public void expandCollapseCell(final int componentid, final int rowid, final boolean expand) {
                       
       SwingUtilities.invokeLater(new Runnable() {
            
            @Override
            public void run() {
                final Component component = componentArray.get(componentid);
                
                if (component instanceof JTree) {
                    JTree tree = (JTree)component;
                    if (expand) {
                        tree.expandRow(rowid);
                    } else {
                        tree.collapseRow(rowid);
                    }
                }
            }
        });
    }
    
    public void selectTab(final int componentid, final int tabid) {
       SwingUtilities.invokeLater(new Runnable() {            
            @Override
            public void run() {
                final JTabbedPane tpane = (JTabbedPane) componentArray.get(componentid);
                tpane.setSelectedIndex(tabid);
            }
        });
    }
              
    public BufferedImage getWindowImage(final int windowId) {
       Window window = windowArray.get(windowId);
       return windowImageMap.get(window);
    }

    public void closeWindow(final int windowId) {
        Window window = windowArray.get(windowId);
        
        java.awt.Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(
                    new WindowEvent(window, WindowEvent.WINDOW_CLOSING) 
                );
    }
    
    public void toFront(final int windowId) {
        final Window window = windowArray.get(windowId);
        
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                window.setAlwaysOnTop(true);
                window.toFront();
                window.requestFocus();
                window.setAlwaysOnTop(false);
            }            
        });
    }
        
    public void escape(final int windowid) {
        Window window = windowArray.get(windowid);
        
        java.awt.Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(
                new KeyEvent( window, 
                            KeyEvent.KEY_PRESSED, 
                            System.currentTimeMillis(), 
                            0, 
                            KeyEvent.VK_ESCAPE,
                            (char)KeyEvent.VK_ESCAPE       
                        ));
    }    
    
    public void enter(final int windowid) {
        Window window = windowArray.get(windowid);
        
        java.awt.Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(
                new KeyEvent( window, 
                            KeyEvent.KEY_PRESSED, 
                            System.currentTimeMillis(), 
                            0, 
                            KeyEvent.VK_ENTER,
                            (char)KeyEvent.VK_ENTER       
                        ));
    }     
        
    private ImageBin obtainImageBin() {
        return new ImageBin(windowImageMap.values());
    }
    
    private static void postMouseEvent(Component component, 
            int type, 
            int mask, 
            Point point,
            int clickCount,
            boolean popupTrigger) 
    {
        java.awt.Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(
                new MouseEvent( component, 
                            type, // event type 
                            0, 
                            mask, // modifiers 
                            point.x, // x 
                            point.y, // y
                            clickCount, 
                            popupTrigger                        
                        ));  
    }
    
    private static void pressedReleasedClicked1(Component component, Rectangle bounds, int clickCount) {
        Point point = new Point(bounds.x + bounds.width/2,bounds.y + bounds.height/2);
        
        postMouseEvent(component, MouseEvent.MOUSE_PRESSED, MouseEvent.BUTTON1_MASK, point, clickCount, false);
        postMouseEvent(component, MouseEvent.MOUSE_RELEASED, MouseEvent.BUTTON1_MASK, point, clickCount, false);
        postMouseEvent(component, MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1_MASK, point, clickCount, false);
    }
}
