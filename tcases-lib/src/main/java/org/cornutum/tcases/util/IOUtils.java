package org.cornutum.tcases.util;

import java.io.Closeable;
import java.io.IOException;

public class IOUtils
  {

  public static void closeQuietly( Closeable closeable)
    {
    try
      {
      if ( closeable != null)
        {
        closeable.close();
        }
      } catch ( IOException var2) {
      }
    }
  }
