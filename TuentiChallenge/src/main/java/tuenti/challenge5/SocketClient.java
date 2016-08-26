package tuenti.challenge5;

import java.io.*;
import java.net.Socket;
import java.util.LinkedList;

public class SocketClient {

    private Socket socket;
    private BufferedWriter out;
    private BufferedReader in;

    private LinkedList<String> messages = new LinkedList<String>();

    public SocketClient(String host, int port) {

        try {
            socket = new Socket(host, port);
            out =    new BufferedWriter(new PrintWriter(socket.getOutputStream(), true));
            in =     new BufferedReader(new InputStreamReader(socket.getInputStream()));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        /* We read in parallel whatever the game says  */
        new Thread(){

            public void run(){

                String line;

                try {
                    while(null != (line = in.readLine())){
                        messages.addLast(line);
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }.start();

    }

    public void write(char c){
        try {
            out.write(c);
            out.newLine();
            out.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void enter(){
        try {
            out.newLine();
            out.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public String getLastRelevantMessage(){

        String message;

        do{
            message = messages.removeLast();
        }while(message == null || message.trim().isEmpty());

        try{
            return message;
        } finally {
            messages.clear();
        }
    }

    public void close(){
        try {
            in.close();
            out.close();
            socket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
