
import java.util.*;
import java.lang.*;
import java.io.*;


public class MyClass
{
  private ArrayList m_list;


  public MyClass()
  {
    m_list = new ArrayList();
  }

  public void add(int x)
  {
    m_list.add(x);
  }

  public boolean remove(int x) throws InterruptedException
  {
    for(int i = 0; i < m_list.size(); i++)
    {
      if(m_list.get(i) == x)
      {
        m_list.remove(i);
        return true;
      }
    }

    return false;
  }
}
