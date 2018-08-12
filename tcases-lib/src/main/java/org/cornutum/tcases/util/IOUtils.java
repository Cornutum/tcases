package org.cornutum.tcases.util;

import java.io.Closeable;
import java.io.IOException;

public final class IOUtils
  {
  private IOUtils()
    {
    // Static methods only
    }
  
  public static void closeQuietly( Closeable closeable)
    {
    try
      {
      if ( closeable != null)
        {
        closeable.close();
        }
      }
    catch ( IOException ignore)
      {
      }
    }
  }
