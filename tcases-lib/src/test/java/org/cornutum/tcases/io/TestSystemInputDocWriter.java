//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.util.Asserts.*;

import org.junit.Test;

import java.io.File;

/**
 * Runs tests for the {@link SystemInputDocWriter}.
 *
 */
public class TestSystemInputDocWriter
  {
  @Test
  public void testWrite_0()
    {
    testWriteResource( "system-input-def-0.xml");
    }

  @Test
  public void testWrite_1()
    {
    testWriteResource( "system-input-def-1.xml");
    }

  @Test
  public void testWrite_2()
    {
    testWriteResource( "system-input-def-2.xml");
    }

  @Test
  public void testWrite_3()
    {
    testWriteResource( "system-input-def-3.xml");
    }

  @Test
  public void testWrite_4()
    {
    testWriteResource( "system-input-def-4.xml");
    }

  @Test
  public void testWrite_5()
    {
    testWriteResource( "system-input-def-5.xml");
    }

  @Test
  public void testWrite_6()
    {
    testWriteResource( "system-input-def-6.xml");
    }

  @Test
  public void testWrite_7()
    {
    testWriteResource( "system-input-def-7.xml");
    }

  @Test
  public void testWrite_43()
    {
    testWriteResource( "system-input-def-43.xml");
    }

  @Test
  public void testWrite_objects()
    {
    testWriteResource( "system-input-def-objects.xml");
    }

  public void testWriteResource( String systemInputResource)
    {
    // Given...
    SystemInputDef systemInputBefore = systemInputResources_.read( systemInputResource);

    // When...
    File doc = getDocPath();
    systemInputResources_.write( systemInputBefore, doc);

    // Then...
    SystemInputDef systemInputAfter = null;
    try
      {
      systemInputAfter = systemInputResources_.read( doc);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read file=" + doc, e);
      }

    assertMatches( "Output from definition=" + systemInputResource, systemInputBefore, systemInputAfter, Matchers.systemInputDefMatcher);
    }

  /**
   * Returns the path to the test output file.
   */
  private File getDocPath()
    {
    return
      new File
      ( System.getProperty( "java.io.tmpdir"),
        "TestSystemInputDocWriter-Output.xml");
    }

  private SystemInputResources systemInputResources_ = new SystemInputResources( TestSystemInputDocWriter.class);
  }
