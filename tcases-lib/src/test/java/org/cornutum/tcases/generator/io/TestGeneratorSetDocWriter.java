//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.generator.*;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;

/**
 * Runs tests for the {@link GeneratorSetDocWriter}.
 *
 */
public class TestGeneratorSetDocWriter
  {
  @Test
  public void testWrite_0()
    {
    testWriteResource( "generator-set-0.xml");
    }

  @Test
  public void testWrite_1()
    {
    testWriteResource( "generator-set-1.xml");
    }

  @Test
  public void testWrite_2()
    {
    testWriteResource( "generator-set-2.xml");
    }

  @Test
  public void testWrite_3()
    {
    testWriteResource( "generator-set-3.xml");
    }

  @Test
  public void testWrite_4()
    {
    testWriteResource( "generator-set-4.xml");
    }

  @Test
  public void testWrite_5()
    {
    testWriteResource( "generator-set-once.xml");
    }
  
  public void testWriteResource( String generatorSetResource)
    {
    // Given...
    IGeneratorSet generatorSetBefore = generatorSetResources_.read( generatorSetResource);

    // When...
    File doc = getDocPath();
    generatorSetResources_.write( generatorSetBefore, doc);

    // Then...
    IGeneratorSet generatorSetAfter = null;
    try
      {
      generatorSetAfter = generatorSetResources_.read( doc);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read file=" + doc, e);
      }

    assertThat( "Output from definition=" + generatorSetResource, generatorSetAfter, is( generatorSetBefore));
    }

  /**
   * Returns the path to the test output file.
   */
  private File getDocPath()
    {
    return
      new File
      ( System.getProperty( "java.io.tmpdir"),
        "TestGeneratorSetDocWriter-Output.xml");
    }

  private GeneratorSetResources generatorSetResources_ = new GeneratorSetResources( TestGeneratorSetDocWriter.class);
  }
