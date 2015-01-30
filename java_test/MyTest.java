
import java.util.*;
import com.ericsson.otp.erlang.*;


public class MyTest
{
  public MyTest()
  {
  }

  private void eval_cmd(MyClass myClass, String command, int arg) throws InterruptedException
  {
    if(command.equals("add")) myClass.add(arg);
    else myClass.remove(arg);
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


  public long run(int iterations, String[] commands, int[] args) throws InterruptedException
  {
    long minTime = -1;
    for(int i = 0; i < iterations; i++)
    {
      clean();
      long startTime = System.nanoTime();
      for(int k = 0; k < 100; k++)
      {
        MyClass myClass = new MyClass();
        for(int j = 0; j < commands.length; j++) eval_cmd(myClass, commands[j], args[j]);
      }
      long stopTime = System.nanoTime();
      long elapsedTime = stopTime - startTime;
      if(minTime == -1 || elapsedTime < minTime) minTime = elapsedTime;
    }
    return minTime;
  }
}
