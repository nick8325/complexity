
import java.util.*;
import java.lang.*;
import java.io.*;


/*
  A simple implementation of a set (that allows duplicated elements). 
*/
public class MyClass
{
  private ArrayList m_list;


  public MyClass()
  {
    m_list = new ArrayList();
  }

  public void add(Object x)
  {
    m_list.add(x);
  }

  public boolean remove(Object x) throws InterruptedException
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

