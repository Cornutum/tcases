//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.*;
import com.startingblocktech.tcases.io.SystemInputDocReader;

import org.junit.Test;
import static org.junit.Assert.*;

import org.apache.commons.collections15.IteratorUtils;
import org.apache.commons.io.IOUtils;

import java.io.InputStream;

/**
 * Runs tests for {@link TupleGenerator}
 *
 * @version $Revision$, $Date$
 */
public class TestTupleGenerator
  {
  @Test
  public void getTuples_Basic()
    {
    // Given...
    SystemInputDef systemInputDef = readSystemInputDef( "system-input-def-0.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    // When...
    FunctionTestDef functionTestDef = generator.getTests( functionInputDef, null);

    // Expect...
    TestCase[] testCases = IteratorUtils.toArray( functionTestDef.getTestCases(), TestCase.class);
    assertEquals( "Test cases", 3, testCases.length); 
    }

  /**
   * Returns the {@link SystemInputDef} defined by the given resource.
   */
  private SystemInputDef readSystemInputDef( String resource)
    {
    SystemInputDef  systemInputDef  = null;
    InputStream     stream          = null;
    
    try
      {
      stream = getClass().getResourceAsStream( resource);

      SystemInputDocReader reader = new SystemInputDocReader( stream);
      systemInputDef = reader.getSystemInputDef();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read resource=" + resource, e);
      }
    finally
      {
      IOUtils.closeQuietly( stream);
      }

    return systemInputDef;
    }
  }



