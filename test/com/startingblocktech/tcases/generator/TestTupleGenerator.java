//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.*;
import com.startingblocktech.tcases.io.SystemInputResources;
import static com.startingblocktech.tcases.util.Asserts.*;

import org.junit.Test;

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
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-0.xml");
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

  private SystemInputResources systemInputResources_ = new SystemInputResources( TestTupleGenerator.class);
  }
