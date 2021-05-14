//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;

import org.apache.commons.io.IOUtils;
import org.junit.Test;

import java.io.File;
import java.io.FileWriter;

/**
 * Runs tests for the {@link SystemTestHtmlWriter}.
 *
 */
public class TestSystemTestHtmlWriter
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

  @Test
  public void testWrite_find()
    {
    testWriteResource( "find-Test.xml");
    }
  
  public void testWriteResource( String systemTestResource)
    {
    // Given...
    SystemTestDef systemTest = systemTestResources_.read( systemTestResource);

    // When...
    File doc = getDocPath( systemTestResource);

    SystemTestHtmlWriter writer = null;
    try
      {
      try
        {
        writer = new SystemTestHtmlWriter( new FileWriter( doc));
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't open file=" + doc, e);
        }
      writer.write( systemTest);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write " + systemTest + " to file=" + doc, e);
      }
    finally
      {
      IOUtils.closeQuietly( writer, null);
      }
    }

  /**
   * Returns the path to the test output file.
   */
  private File getDocPath( String systemTestResource)
    {
    return new File( System.getProperty( "java.io.tmpdir"), systemTestResource.replaceAll( "\\.xml", ".htm"));
    }

  private SystemTestResources systemTestResources_ = new SystemTestResources( TestSystemTestHtmlWriter.class);
  }
