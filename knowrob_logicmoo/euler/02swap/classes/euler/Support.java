// $Id:Support.java 1295 2007-05-11 16:52:51Z josd $

package euler;

import java.io.*;
import java.net.*;
import java.util.*;
import org.mozilla.javascript.*;

/**
 * Support methods
 *
 * @author Jos De Roo
 */

public class Support extends ScriptableObject {
  public String getClassName() {
    return "Support";
  }

  public static String jsFunction_fromWeb(Context cx, Scriptable thisObj, Object[] args, Function funObj) {
    String uri = (String)args[0];
    StringBuffer sb = new StringBuffer();
    String rl = null;
    if (uri.startsWith("data:")) return uri.substring(uri.indexOf(',') + 1);
    else if (uri.startsWith("http:")) {
      try {
        URL r = new URL(uri);
        String p = r.getFile();
        String hp = System.getProperty("http_proxy");
        if (hp != null && !hp.equals("") && !hp.equals("%http_proxy%") && !uri.startsWith("http://localhost")) {
          r = new URL(hp);
          p = uri;
        }
        Socket so = new Socket(r.getHost(), r.getPort()==-1?80:r.getPort());
        so.setTcpNoDelay(true);
        DataOutputStream dos = new DataOutputStream(new BufferedOutputStream(so.getOutputStream()));
        DataInputStream dis = new DataInputStream(new BufferedInputStream(so.getInputStream()));
        PrintWriter pw = new PrintWriter(new OutputStreamWriter(dos, "UTF8"), true);
        pw.print("GET " + p + " HTTP/1.0\r\n");
        pw.print("\r\n");
        pw.flush();
        BufferedReader br = new BufferedReader(new InputStreamReader(dis, "UTF8"));
        if ((rl = br.readLine()).indexOf("200") == -1) throw new RuntimeException(rl);
        int g = -1;
        while (true) {
          rl = br.readLine();
          if (rl.equals("")) break;
          StringTokenizer st = new StringTokenizer(rl);
          String nt = st.nextToken().toLowerCase();
          if (nt.equals("content-length:")) g = Integer.parseInt(st.nextToken());
        }
        if (g != -1) {
          char[] b = new char[65536];
          int n = 0, count = 0;
          while (n < g) {
            if (g - n >= 65536) count = br.read(b, 0, 65536);
            else count = br.read(b, 0, g - n);
            if (count <= 0) break;
            sb.append(b, 0, count);
            n += count;
          }
        }
        else {
          while (true) {
            rl = br.readLine();
            if (rl == null) break;
            sb.append(rl + "\n");
          }
        }
        dis.close();
        dos.close();
        so.close();
      }
      catch (Exception e) {
        System.err.println(e);
      }
    }
    else {
      String file = uri;
      if (file.startsWith("file:")) file = file.substring(file.lastIndexOf(':') + 1);
      if (file.indexOf('#') != -1) file = file.substring(0, file.lastIndexOf('#'));
      try {
        String path = System.getProperty("user.dir").replace(File.separatorChar, '/');
        path = path.substring(path.indexOf('/')) + "/";
        FileInputStream fis = new FileInputStream(file);
        DataInputStream dis = new DataInputStream(new BufferedInputStream(fis));
        BufferedReader br = new BufferedReader(new InputStreamReader(dis, "UTF8"));
        while ((rl = br.readLine()) != null) {
          sb.append(rl);
          sb.append('\n');
        }
        dis.close();
        fis.close();
      }
      catch (Exception e) {
        System.err.println(e);
      }
    }
    return sb.toString();
  }
}
