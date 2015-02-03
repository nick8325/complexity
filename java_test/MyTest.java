
import java.util.*;
import java.io.*;
import java.nio.charset.*;
import com.ericsson.otp.erlang.*;
import javassist.*;


public class MyTest
{
  public MyTest()
  {
  }

  public void clean()
  {
    long freeMemory;
    do {
      freeMemory = Runtime.getRuntime().freeMemory();
      System.runFinalization();
      System.gc();
    } while(Runtime.getRuntime().freeMemory() > freeMemory);
  }

  static int n = 0;
  public long run(int iterations, int innerIterations, String command) throws Exception
  {
    CtClass cc = ClassPool.getDefault().getAndRename("TestCase", "ConcreteTestCase" + n);
    n++;
    cc.getDeclaredMethod("run").setBody(command);
    Command c = (Command) cc.toClass().newInstance();
    long minTime = -1;
    for(int i = 0; i < iterations; i++)
    {
//      clean();
//      System.gc();
      long startTime = System.nanoTime();
      for(int k = 0; k < innerIterations; k++) c.run();
      long stopTime = System.nanoTime();
      long elapsedTime = stopTime - startTime;
      if(minTime == -1 || elapsedTime < minTime) minTime = elapsedTime;
    }
    cc.detach();
    return minTime;
  }
}
