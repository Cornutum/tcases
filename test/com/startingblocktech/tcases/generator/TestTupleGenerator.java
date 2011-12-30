//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.*;
import com.startingblocktech.tcases.io.SystemInputDocReader;
import static com.startingblocktech.tcases.util.Asserts.*;

import org.junit.Test;
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
    TCaseBuilder tc = new TCaseBuilder();
    assertSeqEquals
      ( "Test cases",
        TCaseBuilder.sequence
        ( tc
          .start()
          .bind( "Color.Hue", "Red")
          .bind( "Color.Lightness", "Bright")
          .bind( "Color.Saturation", "Pale")
          .bind( "Size", "Small")
          .bind( "Shape", "Square")
          .build(),

          tc
          .start()
          .bind( "Color.Hue", "Green")
          .bind( "Color.Lightness", "Normal")
          .bind( "Color.Saturation", "Even")
          .bind( "Size", "Medium")
          .bind( "Shape", "Circle")
          .build(),

          tc
          .start()
          .bind( "Color.Hue", "Blue")
          .bind( "Color.Lightness", "Dark")
          .bind( "Color.Saturation", "Intense")
          .bind( "Size", "Large")
          .bind( "Shape", "Heart")
          .build()
          ),
        functionTestDef.getTestCases());
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



