//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.anon;

import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.SystemInputDefMatcher;
import org.cornutum.tcases.anon.AnonCommand.Options;
import org.cornutum.tcases.generator.IGeneratorSet;
import org.cornutum.tcases.generator.io.GeneratorSetResources;
import org.cornutum.tcases.io.SystemInputResources;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

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
    File outFile = getResourceFile( "anon-Input.xml");
    outFile.delete();
    
    String[] args =
      {
        "-f", outFile.getPath(),
        String.format( "%s/find", inFile.getParent())
      };
    
    // When...
    AnonCommand.run( new Options( args));

    // Then...
    verifyAnonymized( "whenInputFile", outFile);
    }
  
  @Test
  public void whenInputStdin() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "system-input-def-objects.xml");
    
    String[] args = new String[0];
    
    // When...
    StringBuffer anonymized = new StringBuffer();
    runWithStdIO( new Options( args), inFile, anonymized);

    // Then...
    verifyAnonymized( "whenInputStdin", anonymized.toString());
    }
  
  @Test
  public void whenInputJson() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "Ice-Cream-Input.json");
    
    String[] args =
      {
        "-T", "json"
      };
    
    // When...
    StringBuffer anonymized = new StringBuffer();
    runWithStdIO( new Options( args), inFile, anonymized);

    // Then...
    verifyAnonymizedJson( "whenInputJson", anonymized.toString());
    }
  
  @Test
  public void whenAnnotations() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "annotations-Input.json");
    
    String[] args =
      {
        inFile.getPath()
      };
    
    // When...
    StringBuffer anonymized = new StringBuffer();
    runWithStdIO( new Options( args), null, anonymized);

    // Then...
    verifyAnonymizedJson( "whenAnnotations", anonymized.toString());
    }
  
  @Test
  public void whenOutputJson() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "find-Input.xml");
    File outFile = getResourceFile( "anon-find.json");
    File genOutFile= getResourceFile( "anon-find-Generators.json");

    outFile.delete();
    genOutFile.delete();
    
    String[] args =
      {
        "-f", outFile.getPath(),
        inFile.getPath()
      };
    
    // When...
    AnonCommand.run( new Options( args));

    // Then...
    verifyAnonymizedJson( "whenOutputJson", outFile);
    verifyAnonymizedGenJson( "whenOutputJson", genOutFile);
    }
  
  @Test
  public void whenGenDefDefined() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "find-Input.xml");
    File genFile = getResourceFile( "example-Generators.xml");
    File outFile = getResourceFile( "generated/anon-Input.xml");
    File genOutFile = getResourceFile( "generated/anon-Generators.xml");
    
    outFile.delete();
    genOutFile.delete();
    
    String[] args =
      {
        "-f", outFile.getPath(),
        "-g", genFile.getPath(),
        inFile.getPath()
      };
    
    // When...
    AnonCommand.run( new Options( args));

    // Then...
    verifyAnonymized( "whenGenDefDefined", outFile);
    verifyAnonymizedGen( "whenGenDefDefined", genOutFile);
    }
  
  @Test
  public void whenGenDefDefault() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "example.json");
    File outFile = getResourceFile( "generated/my-anon");
    File genOutFile = getResourceFile( "generated/my-anon-Generators.json");

    outFile.delete();
    genOutFile.delete();
    
    String[] args =
      {
        "-f", outFile.getPath(),
        inFile.getPath()
      };
    
    // When...
    AnonCommand.run( new Options( args));

    // Then...
    verifyAnonymizedJson( "whenGenDefDefault", outFile);
    verifyAnonymizedGenJson( "whenGenDefDefault", genOutFile);
    }
  
  @Test
  public void whenGenDefJson() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "example");
    File outFile = getResourceFile( "generated/anon-example-Input");
    File genFile = getResourceFile( "generators");
    File genOutFile = getResourceFile( "generated/anon-example-Generators.json");
    
    outFile.delete();
    genOutFile.delete();
    
    String[] args =
      {
        "-f", outFile.getPath(),
        "-g", genFile.getName(),
        inFile.getPath()
      };
    
    // When...
    AnonCommand.run( new Options( args));

    // Then...
    verifyAnonymizedJson( "whenGenDefJson", outFile);
    verifyAnonymizedGenJson( "whenGenDefJson", genOutFile);
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
  private void verifyAnonymized( String baseName, File anonymized) throws Exception
    {
    verifyAnonymized( baseName, FileUtils.readFileToString( anonymized, "UTF-8"));
    }

  /**
   * Verifies that the anonymized system input definition matches expectations.
   */
  private void verifyAnonymized( String baseName, String anonymized) throws Exception
    {
    SystemInputDef anonDef = inputResources_.readString( anonymized);

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
   * Verifies that the anonymized generator definition matches expectations.
   */
  private void verifyAnonymizedGen( String baseName, File anonymized) throws Exception
    {
    IGeneratorSet anonDef = generatorSetResources_.read( anonymized);

    String expectedFile = String.format( "%s-Expected-Generators.xml", baseName);
    if( acceptAsExpected())
      {
      File anonExpected = new File( saveExpectedDir_, expectedFile);
      generatorSetResources_.write( anonDef, anonExpected);
      }
    else
      {
      File anonExpected = getResourceFile( expectedFile);
      assertThat( baseName, anonDef, is( generatorSetResources_.read( anonExpected)));
      }
    }

  /**
   * Verifies that the anonymized system input definition matches expectations.
   */
  private void verifyAnonymizedJson( String baseName, File anonymized) throws Exception
    {
    verifyAnonymizedJson( baseName, FileUtils.readFileToString( anonymized, "UTF-8"));
    }

  /**
   * Verifies that the anonymized system input definition matches expectations.
   */
  private void verifyAnonymizedJson( String baseName, String anonymized) throws Exception
    {
    SystemInputDef anonDef = inputResources_.readJsonString( anonymized);

    String expectedFile = String.format( "%s-Expected-Input.json", baseName);
    if( acceptAsExpected())
      {
      File anonExpected = new File( saveExpectedDir_, expectedFile);
      inputResources_.writeJson( anonDef, anonExpected);
      }
    else
      {
      File anonExpected = getResourceFile( expectedFile);
      assertThat(
        baseName,
        anonDef,
        matches( new SystemInputDefMatcher( inputResources_.readJson( anonExpected))));
      }
    }

  /**
   * Verifies that the anonymized generator definition matches expectations.
   */
  private void verifyAnonymizedGenJson( String baseName, File anonymized) throws Exception
    {
    IGeneratorSet anonDef = generatorSetResources_.readJson( anonymized);

    String expectedFile = String.format( "%s-Expected-Generators.json", baseName);
    if( acceptAsExpected())
      {
      File anonExpected = new File( saveExpectedDir_, expectedFile);
      generatorSetResources_.writeJson( anonDef, anonExpected);
      }
    else
      {
      File anonExpected = getResourceFile( expectedFile);
      assertThat( baseName, anonDef, is( generatorSetResources_.readJson( anonExpected)));
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
  private GeneratorSetResources generatorSetResources_ = new GeneratorSetResources( getClass());
  
  private final File saveExpectedDir_ =
    Optional.ofNullable( StringUtils.trimToNull( System.getProperty( "saveExpectedTo")))
    .map( path -> new File( path))
    .orElse( null);
  }
