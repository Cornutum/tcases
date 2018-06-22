package org.cornutum.tcases.util;

public class FilenameUtils
  {

  public static String getBaseName( String filepath)
    {
    if ( filepath == null)
      {
      return null;
      }
    int lastUnixPos = filepath.lastIndexOf( 47);
    int lastWindowsPos = filepath.lastIndexOf( 92);
    int indexOfLastSeparator = Math.max( lastUnixPos, lastWindowsPos);
    String filename = filepath.substring( indexOfLastSeparator + 1);
    int extensionPos = filename.lastIndexOf( 46);
    return extensionPos == -1 ? filename : filename.substring( 0, extensionPos);
    }
  }
