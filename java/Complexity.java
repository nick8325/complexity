
import javassist.*;


public class Complexity 
{
  // Interface used to interface with dynamically generated test runs.
  public interface ITestRun {
      public void run() throws Exception;
  }


  // The template class used to dynamically generate test runs. 
  public static class TestRun implements ITestRun {
    public void run() throws Exception {}
  } 


  // Used to generate unique class names.
  private static int classCounter = 0;


  // Cleans the runtime by garbage collecting the memory.
  public static void cleanRuntime()
  {
    long freeMemory;

    do {
      freeMemory = Runtime.getRuntime().freeMemory();
      System.runFinalization();
      System.gc();
    } while(Runtime.getRuntime().freeMemory() > freeMemory);
  }

  public static long run(boolean cleanRuntime, int iterations, String command) throws Exception
  {
    // Generate a new class for the provided command.
    classCounter++;
    CtClass cc = ClassPool.getDefault().getAndRename("Complexity$TestRun", "Complexity$TestRun" + classCounter);
    cc.getDeclaredMethod("run").setBody(command);
    ITestRun testRun = (ITestRun)cc.toClass().newInstance();

    if(cleanRuntime) cleanRuntime();

    long startTime = System.nanoTime();
    for(int i = 0; i < iterations; i++) testRun.run();
    long elapsedTime = System.nanoTime() - startTime;

    cc.detach();

    return elapsedTime;
  }
}

