//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.io;

import org.junit.Test;
import static org.junit.Assert.*;

import org.apache.commons.io.IOUtils;

import java.io.File;

/**
 * Runs tests for the {@link TransformFilter}.
 *
 * @version $Revision$, $Date$
 */
public class TestTransformFilter
  {
  @Test
  public void testWriteFile() throws Exception
    {
    // Given...
    TransformFilter filter =
      new TransformFilter
      ( getClass().getResourceAsStream( "/testDef2Junit.xsl"));

    File target = File.createTempFile( "TestTransformFilter-WriteFile-", ".java");
    target.deleteOnExit();
    filter.setTarget( target);

    // When...
    IOUtils.copy
      ( getClass().getResourceAsStream( "system-test-def-2.xml"),
        filter.getSource());
    filter.close();
    
    // Then...
    assertEquals( "Output written, target=" + target, true, target.length() > 0);
    }
  }