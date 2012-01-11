//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.io;

import com.startingblocktech.tcases.*;

import org.junit.Test;
import static org.junit.Assert.*;

import java.io.File;

/**
 * Runs tests for the {@link SystemTestDocWriter}.
 *
 * @version $Revision$, $Date$
 */
public class TestSystemTestDocWriter
  {
  @Test
  public void testWrite_0()
    {
    testWriteResource( "system-test-def-0.xml");
    }

  @Test
  public void testWrite_1()
    {
    testWriteResource( "system-test-def-1.xml");
    }

  @Test
  public void testWrite_2()
    {
    testWriteResource( "system-test-def-2.xml");
    }

  @Test
  public void testWrite_3()
    {
    testWriteResource( "system-test-def-3.xml");
    }

  @Test
  public void testWrite_4()
    {
    testWriteResource( "system-test-def-4.xml");
    }
  
  public void testWriteResource( String systemTestResource)
    {
    // Given...
    SystemTestDef systemTestBefore = systemTestResources_.read( systemTestResource);

    // When...
    File doc = getDocPath();
    systemTestResources_.write( systemTestBefore, doc);

    // Then...
    SystemTestDef systemTestAfter = null;
    try
      {
      systemTestAfter = systemTestResources_.read( doc);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read file=" + doc, e);
      }

    assertEquals( "Output from definition=" + systemTestResource, systemTestBefore, systemTestAfter);
    }

  /**
   * Returns the path to the test output file.
   */
  private File getDocPath()
    {
    return
      new File
      ( System.getProperty( "java.io.tmpdir"),
        "TestSystemTestDocWriter-Output.xml");
    }

  private SystemTestResources systemTestResources_ = new SystemTestResources( TestSystemTestDocWriter.class);
  }