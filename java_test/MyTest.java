
import java.util.*;
import com.ericsson.otp.erlang.*;


public class MyTest
{
  public MyTest()
  {
  }

  private void eval_cmd(MyClass myClass, String command, int arg) throws InterruptedException
  {
    if(command == "add") myClass.add(arg);
    else myClass.remove(arg);
  }

  public long run(int iterations, String[] commands, int[] args) throws InterruptedException
  {
    long minTime = -1;
    for(int i = 0; i < iterations; i++)
    {
      MyClass myClass = new MyClass();
      System.gc();
      long startTime = System.nanoTime();
      for(int j = 0; j < commands.length; j++) eval_cmd(myClass, commands[j], args[j]);
      long stopTime = System.nanoTime();
      long elapsedTime = stopTime - startTime;
      if(minTime == -1 || elapsedTime < minTime) minTime = elapsedTime;
    }
    return minTime;
  }
}
