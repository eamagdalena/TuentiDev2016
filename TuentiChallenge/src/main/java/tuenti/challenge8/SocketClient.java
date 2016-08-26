package tuenti.challenge8;

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
                        System.out.println(line);
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

    public static void main(String[] args){

        SocketClient sc = new SocketClient("52.49.91.111",1986);

        for(int i = 0; i < 65536; i++){

            System.out.println("Writing " + i + " " + ((char) i));
            sc.write((char) i);


        }
    }

}
