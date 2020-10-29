//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.anon;

import org.cornutum.tcases.io.SystemInputResources;
import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.SystemInputDefMatcher;
import org.cornutum.tcases.anon.AnonCommand.Options;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.Optional;

/**
 * Runs tests for {@link AnonCommand}.
 */
public class AnonCommandTest
  {

  @Test
  public void whenInputFile() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "find-Input.xml");
    
    String[] args =
      {
        String.format( "%s/find", inFile.getParent())
      };
    
    // When...
    StringBuffer anonymized = new StringBuffer();
    runWithStdIO( new Options( args), null, anonymized);

    // Then...
    verifyAnonymized( "whenInputFile", anonymized);
    }

  /**
   * Return the file for the given resource.
   */
  private File getResourceFile( String resource)
    {
    URL classUrl = getClass().getResource( getClass().getSimpleName() + ".class");
    return new File( new File( classUrl.getFile()).getParent(), resource);
    }

  /**
   * Run AnonCommand with the given options, using the given standard input/output.
   * If <CODE>stdIn</CODE> is non-null, redirect standard input to read from the given file.
   * If <CODE>stdOut</CODE> is non-null, redirect standard output to write to the given buffer.
   */
  private void runWithStdIO( Options options, File stdIn, StringBuffer stdOut) throws Exception
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

      AnonCommand.run( options);
      }
    finally
      {
      IOUtils.closeQuietly( newIn);
      IOUtils.closeQuietly( newOut);

      System.setIn( prevIn);
      System.setOut( prevOut);

      if( newOutBytes != null)
        {
        stdOut.append( new String( newOutBytes.toByteArray(), Charset.forName( "UTF-8")));
        }
      }
    }

  /**
   * Verifies that the anonymized system input definition matches expectations.
   */
  private void verifyAnonymized( String baseName, StringBuffer anonymized) throws Exception
    {
    SystemInputDef anonDef = inputResources_.read( anonymized);

    String expectedFile = String.format( "%s-Expected-Input.xml", baseName);
    if( acceptAsExpected())
      {
      File anonExpected = new File( saveExpectedDir_, expectedFile);
      inputResources_.write( anonDef, anonExpected);
      }
    else
      {
      File anonExpected = getResourceFile( expectedFile);
      assertThat(
        baseName,
        anonDef,
        matches( new SystemInputDefMatcher( inputResources_.read( anonExpected))));
      }
    }

  /**
   * Returns true if all generated results are automatically accepted.
   */
  private boolean acceptAsExpected()
    {
    return saveExpectedDir_ != null;
    }

  private SystemInputResources inputResources_ = new SystemInputResources( getClass());

  private final File saveExpectedDir_ =
    Optional.ofNullable( StringUtils.trimToNull( System.getProperty( "saveExpectedTo")))
    .map( path -> new File( path))
    .orElse( null);
  }
