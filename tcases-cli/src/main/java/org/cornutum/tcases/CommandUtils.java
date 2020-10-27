//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.apache.commons.io.FilenameUtils;

import java.io.File;
import java.io.InputStream;
import java.util.Properties;

/**
 * Defines common CLI command methods.
 */
public class CommandUtils
  {
  /**
   * Creates a new CommandUtils instance.
   */
  private CommandUtils()
    {
    // Static methods only
    }

  /**
   * Returns a description of the current version.
   */
  public static String getVersion()
    {
    Properties tcasesProperties = new Properties();
    String tcasesPropertyFileName = "/tcases.properties";
    
    try( InputStream tcasesPropertyFile = Tcases.class.getResourceAsStream( tcasesPropertyFileName))
      {
      tcasesProperties.load( tcasesPropertyFile);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read " + tcasesPropertyFileName, e);
      }

    return
      String.format(
        "Tcases %s (%s)",
        tcasesProperties.getProperty( "tcases.version"),
        tcasesProperties.getProperty( "tcases.date"));
    }

  /**
   * Returns the name of the project for the given input definition file.
   */
  public static String getProjectName( File inputDefFile)
    {
    String projectName = null;
    if( inputDefFile != null)
      {
      String inputBase = FilenameUtils.getBaseName( inputDefFile.getName());

      projectName =
        inputBase.toLowerCase().endsWith( "-input")
        ? inputBase.substring( 0, inputBase.length() - "-input".length())
        : inputBase;
      }

    return projectName;
    }

  /**
   * Throws a IllegalArgumentException reporting a command line error.
   */
  public static void throwUsageException( String detail)
    {
    throwUsageException( detail, null);
    }

  /**
   * Throws a IllegalArgumentException reporting a command line error.
   */
  public static void throwUsageException( String detail, Exception cause)
    {
    throw getUsageException( detail, cause);
    }

  /**
   * Returns an IllegalArgumentException reporting a command line error.
   */
  public static IllegalArgumentException getUsageException( String detail, Exception cause)
    {
    return
      new IllegalArgumentException
      ( "Invalid command line argument. For all command line details, use the -help option.",
        new IllegalArgumentException( detail, cause));
    }

  /**
   * Throws a IllegalArgumentException reporting a missing option value
   */
  public static void throwMissingValue( String option)
    {
    throwUsageException( String.format( "No value given for %s option", option));
    }
  }
