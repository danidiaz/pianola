package info.danidiaz.pianola.driver;

import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;

import org.msgpack.MessagePack;
import org.msgpack.MessageTypeException;
import org.msgpack.packer.MessagePackPacker;
import org.msgpack.packer.Packer;
import org.msgpack.unpacker.MessagePackUnpacker;
import org.msgpack.unpacker.Unpacker;

public class Driver implements Runnable
{
    
    // http://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xml
    private final static int DEFAULT_PORT = 26060;
    
    private final ServerSocket serverSocket;
    private final MessagePack messagePack;
    
    boolean releaseIsPopupTrigger;
    
    private int lastSnapshotId = 0;
    private Snapshot lastSnapshot = null; 
    
    private ByteArrayOutputStream imageBuffer = new ByteArrayOutputStream();
    
    // http://docs.oracle.com/javase/6/docs/api/java/lang/instrument/package-summary.html
    public static void premain(String agentArgs) {
        agentArgs = agentArgs == null ? "" : agentArgs;
        
        System.out.println( "Hi, I'm the agent, started with options: " + agentArgs );
                
        try {
            int port = DEFAULT_PORT;
            boolean releaseIsPopupTrigger = true;            
            String [] splittedArgs = agentArgs.split(",",0);
            for (int i=0;i<splittedArgs.length;i++) {
                String arg = splittedArgs[i];
                if (arg.startsWith("port")) {
                    port = Integer.decode(arg.substring(arg.indexOf('/')+1));
                } else if (arg.startsWith("popupTrigger")) {
                    releaseIsPopupTrigger =
                            arg.substring(arg.indexOf('/')+1).equals("release");
                }
            }                        
            
            final ServerSocket serverSocket = new ServerSocket(DEFAULT_PORT);
            MessagePack messagePack = new MessagePack(); 
                        
            Thread serverThread = new Thread(new Driver(serverSocket,messagePack,releaseIsPopupTrigger));
            serverThread.setDaemon(true);
            serverThread.start();
            System.out.println("Pianola server started at port " + port);
        } catch (NumberFormatException e) {
            e.printStackTrace();
        }catch (IOException e) {       
            e.printStackTrace();
        }
            
    }

    public Driver(ServerSocket serverSocket, MessagePack messagePack,boolean releaseIsPopupTrigger) {
        super();
        this.serverSocket = serverSocket;
        this.messagePack = messagePack;
        this.releaseIsPopupTrigger = releaseIsPopupTrigger;
    }
    
    private enum Action {
        CLICK("click") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException  {
                int cId = unpacker.readInt();
                snapshot.click(cId);                
            }  },
        DOUBLECLICK("doubleClick") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int cId = unpacker.readInt();
                snapshot.doubleClick(cId);
            }  },
        RIGHTCLICK("rightClick") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int cId = unpacker.readInt();
                snapshot.rightClick(cId);               
            }  },
        CLICKBUTTON("clickButton") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int buttonId = unpacker.readInt();
                snapshot.clickButton(buttonId);
            }  },
        TOGGLE("toggle") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int buttonId = unpacker.readInt();
                boolean targetState = unpacker.readBoolean();
                snapshot.toggle(buttonId,targetState);                
            }  },
        CLICKCOMBO("clickCombo") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int buttonId = unpacker.readInt();
                snapshot.clickCombo(buttonId);                
            }  },
        SETTEXTFIELD("setTextField") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int buttonId = unpacker.readInt();
                String text = unpacker.readString();
                snapshot.setTextField(buttonId,text);                
            }  },
        CLICKCELL("clickCell") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int componentId = unpacker.readInt();
                int rowId = unpacker.readInt();
                int columnId = unpacker.readInt();
                snapshot.clickCell(componentId,rowId,columnId);               
            }  },
        DOUBLECLICKCELL("doubleClickCell") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int componentId = unpacker.readInt();
                int rowId = unpacker.readInt();
                int columnId = unpacker.readInt();
                snapshot.doubleClickCell(componentId,rowId,columnId);                
            }  },
        RIGHTCLICKCEll("rightClickCell") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int componentId = unpacker.readInt();
                int rowId = unpacker.readInt();
                int columnId = unpacker.readInt();
                snapshot.rightClickCell(componentId,rowId,columnId);                
            }  },
        EXPANDCOLLAPSECELL("expandCollapseCell") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException  {
                int componentId = unpacker.readInt();
                int rowId = unpacker.readInt();
                int columnId = unpacker.readInt(); // not actually used
                boolean expand = unpacker.readBoolean();
                snapshot.expandCollapseCell(componentId,rowId,expand);                
            }  },
        SELECTTAB("selectTab") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException  {
                int componentId = unpacker.readInt();
                int tabid = unpacker.readInt();
                snapshot.selectTab(componentId,tabid);               
            }  },
        GETWINDOWIMAGE("getWindowImage") {
            @Override
            public void unpackInvokePack(Unpacker unpacker, Snapshot snapshot,
                    ByteArrayOutputStream imageBuffer, Packer packer)
                    throws Exception {
                int windowId = unpacker.readInt();
                BufferedImage image = snapshot.getWindowImage(windowId);
                imageBuffer.reset();
                ImageIO.write(image, "png", imageBuffer);
                packer.write((int)0);
                packer.write(imageBuffer.toByteArray());
            } },
        CLOSEWINDOW("closeWindow") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int windowId = unpacker.readInt();
                snapshot.closeWindow(windowId);                
            }  },
        TOFRONT("toFront") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int windowId = unpacker.readInt();
                snapshot.toFront(windowId);                
            }  },
        ESCAPE("escape") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int windowId = unpacker.readInt();
                snapshot.escape(windowId);                
            }  },
        ENTER("enter") {
            @Override
            public void unpackInvoke(Unpacker unpacker, Snapshot snapshot) throws IOException {
                int windowId = unpacker.readInt();
                snapshot.enter(windowId);                
            }  };
        
        private final String name;

        private Action(String name) {
            this.name = name;
        }
        
        public String getName() {
            return name;
        }
        
        public void unpackInvokePack(
                Unpacker unpacker,
                Snapshot snapshot,
                ByteArrayOutputStream imageBuffer,
                Packer packer
            ) 
        throws Exception {
            try {
                unpackInvoke(unpacker, snapshot);
            } catch (Exception e) {
                e.printStackTrace();
                
                packer.write((int)1); // An error happened.
                packer.write((int)2); // Internal server error.
                StringWriter sw = new StringWriter();
                PrintWriter pw = new PrintWriter(sw);
                packer.write(sw.toString());
                return;
            }
            packer.write((int)0); // No error happened.
            packer.writeNil();
        }
        
        // For those requests which usually respond null
        public void unpackInvoke(Unpacker unpacker,Snapshot snapshot) throws Exception { }
    }
    
    @Override
    public void run() {
        try {
            Map<String,Action> actionMap = new HashMap<String,Action>();
            for (Action a: Action.values()) {
                actionMap.put(a.getName(),a);
            }
                
            boolean shutdownServer = false;
            while (!shutdownServer) {
                Socket  clientSocket = serverSocket.accept();
                
                InputStream sistream =  new BufferedInputStream(clientSocket.getInputStream());
                Unpacker unpacker = new MessagePackUnpacker(messagePack,sistream);
                
                OutputStream sostream =  new BufferedOutputStream(clientSocket.getOutputStream());
                Packer packer = new MessagePackPacker(messagePack,sostream);
               
                try {
                    String methodName = unpacker.readString();                
                    if (methodName.equals("shutdown")) {
                        shutdownServer = true;
                    } else if (methodName.equals("snapshot")) {
                        lastSnapshotId++;
                        Snapshot pianola = new Snapshot(lastSnapshot,releaseIsPopupTrigger);
                        packer.write((int)0); // No error happened.
                        packer.write((int)lastSnapshotId);
                        pianola.buildAndWrite(packer);
                        lastSnapshot = pianola;     
                    } else {
                        int snapshotId = unpacker.readInt();
                        if (snapshotId == lastSnapshotId) {
                            if (actionMap.containsKey(methodName)) {
                                actionMap.get(methodName).unpackInvokePack(unpacker,
                                        lastSnapshot,
                                        imageBuffer,
                                        packer
                                    );
                            } else {
                                packer.write((int)1); // An error happened. 
                                packer.write((int)2); // Server error. 
                                packer.write("Unsupported method: " + methodName);
                            }
                        } else {
                            packer.write((int)1); // An error happened. 
                            packer.write((int)1); // Snapshot mismatch error. 
                            packer.write((int)snapshotId);
                            packer.write((int)lastSnapshotId); 
                        }
                    }
                    sostream.flush();
                } catch (IOException ioe) {
                    ioe.printStackTrace();    
                } catch (MessageTypeException msgte) {                
                    msgte.printStackTrace();
                } catch (Exception e) {
                    e.printStackTrace();
                } finally {
                    sistream.close();
                    sostream.close();
                    clientSocket.close();
                }
            }
            serverSocket.close();
        } catch (IOException ioe) {
            ioe.printStackTrace();    
        }  
    } 
}
