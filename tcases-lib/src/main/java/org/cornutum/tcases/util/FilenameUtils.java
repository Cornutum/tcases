package org.cornutum.tcases.util;

public final class FilenameUtils
  {
  private FilenameUtils()
    {
    // Static methods only
    }

  /**
   * @deprecated Use <CODE>org.apache.commons.io.FilenameUtils.getBaseName</CODE>
   */
  @Deprecated public static String getBaseName( String filepath)
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
