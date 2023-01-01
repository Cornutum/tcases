//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.apache.commons.io.IOUtils;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.URL;
import java.nio.charset.Charset;

/**
 * Base class for CLI command tests.
 */
public abstract class CommandTest
  {
  /**
   * Return the file for the given resource.
   */
  protected File getResourceFile( String resource)
    {
    URL classUrl = getClass().getResource( getClass().getSimpleName() + ".class");
    return new File( new File( classUrl.getFile()).getParent(), resource);
    }

  /**
   * Performs the given Runnable, using the given standard input/output.
   * If <CODE>stdIn</CODE> is non-null, redirect standard input to read from the given file.
   * If <CODE>stdOut</CODE> is non-null, redirect standard output to write to the given buffer.
   */
  protected void runWithStdIO( Runnable runnable, File stdIn, StringBuffer stdOut)
    {
    InputStream prevIn = System.in;
    PrintStream prevOut = System.out;

    InputStream newIn = null;
    PrintStream newOut = null;
    ByteArrayOutputStream newOutBytes = null;
    
    try
      {
      if( stdIn != null)
        {
        System.setIn( (newIn = new FileInputStream( stdIn)));
        }

      if( stdOut != null)
        {
        stdOut.delete( 0, stdOut.length());
        System.setOut( (newOut = new PrintStream( (newOutBytes = new ByteArrayOutputStream()))));
        }

      runnable.run();
      }
    catch( RuntimeException r)
      {
      throw r;
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't run", e);
      }
    finally
      {
      IOUtils.closeQuietly( newIn, null);
      IOUtils.closeQuietly( newOut, null);

      System.setIn( prevIn);
      System.setOut( prevOut);

      if( newOutBytes != null)
        {
        stdOut.append( new String( newOutBytes.toByteArray(), Charset.forName( "UTF-8")));
        }
      }
    }

  /**
   * Performs the given Runnable, using the given standard error.
   */
  protected void runWithStdErr( Runnable runnable, StringBuffer stdErr)
    {
    PrintStream prevErr = System.err;

    PrintStream newErr = null;
    ByteArrayOutputStream newErrBytes = null;
    
    try
      {
      stdErr.delete( 0, stdErr.length());
      System.setErr( (newErr = new PrintStream( (newErrBytes = new ByteArrayOutputStream()))));

      runnable.run();
      }
    catch( RuntimeException r)
      {
      throw r;
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't run", e);
      }
    finally
      {
      IOUtils.closeQuietly( newErr, null);

      System.setErr( prevErr);

      if( newErrBytes != null)
        {
        stdErr.append( new String( newErrBytes.toByteArray(), Charset.forName( "UTF-8")));
        }
      }
    }
  }
