
import java.util.*;
import java.lang.*;
import java.io.*;


/*
  A wrapper class for ArrayList.
*/
public class ArrayListWrapper 
{
  private ArrayList m_list;


  public ArrayListWrapper()
  {
    m_list = new ArrayList();
  }

  public boolean add(Object x)
  {
    return m_list.add(x);
  }

  public boolean remove(Object x)
  {
    return m_list.remove(x);
  }
}

