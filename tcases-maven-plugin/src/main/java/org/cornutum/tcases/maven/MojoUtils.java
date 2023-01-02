//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.maven;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Defines common methods for Mojo implementations
 */
public final class MojoUtils
  {
  /**
   * Creates a new MojoUtils instance.
   */
  private MojoUtils()
    {
    // Static methods only
    }

  /**
   * Returns the file name defined by applying the given pattern to the project name.
   * Returns null if the file pattern is invalid.
   */
  public static String getProjectFile( String projectName, String filePattern)
    {
    String projectFile = null;
    if( !StringUtils.isBlank( filePattern))
      {
      Matcher matcher = projectFilePattern_.matcher( filePattern);
      if( matcher.matches())
        {
        projectFile =
          StringUtils.isBlank( matcher.group(2))
          ? filePattern
          : matcher.group(1) + projectName + matcher.group(3);
        }
      }
    
    return projectFile;
    }

  /**
   * If the given path is not absolute, returns it as an absolute path relative to the
   * given base directory. Otherwise, returns the given absolute path.
   */
  public static File getDirPath( File baseDir, File dirPath)
    {
    return
      dirPath == null?
      baseDir :
      
      dirPath.isAbsolute()?
      dirPath :

      new File( baseDir, dirPath.getPath());
    }

  private static final Pattern projectFilePattern_ = Pattern.compile( "([^\\*]*)(\\*?)([^\\*]*)");
  }
