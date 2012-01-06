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
  
  @Test
  public void getTuples_Constrained()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-1.xml");
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
          .bind( "Shape", "Circle")
          .bind( "Size", "Small")
          .build(),

          tc
          .start()
          .bind( "Color.Hue", "Green")
          .bind( "Color.Lightness", "Normal")
          .bind( "Color.Saturation", "Even")
          .bind( "Shape", "Heart")
          .bind( "Size", "Small")
          .build(),

          tc
          .start()
          .bind( "Color.Hue", "Blue")
          .bind( "Color.Lightness", "Dark")
          .bind( "Color.Saturation", "Intense")
          .bind( "Shape", "Heart")
          .bind( "Size", "Small")
          .build(),

          tc
          .start()
          .bind( "Color.Hue", "NA")
          .bind( "Color.Lightness", "NA")
          .bind( "Color.Saturation", "NA")
          .bind( "Shape", "Square")
          .bind( "Size", "Medium")
          .build(),

          tc
          .start()
          .bind( "Color.Hue", "NA")
          .bind( "Color.Lightness", "NA")
          .bind( "Color.Saturation", "NA")
          .bind( "Shape", "Circle")
          .bind( "Size", "Large")
          .build()
          ),
        functionTestDef.getTestCases());
    }
  
  @Test
  public void getTuples_Combined()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-0.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    generator.addCombiner
      ( new TupleCombiner(2)
        .addIncludedVar( "Color.Hue")
        .addIncludedVar( "Size"));

    generator.addCombiner
      ( new TupleCombiner(2)
        .addIncludedVar( "Shape")
        .addIncludedVar( "Size"));
    
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
          .bind( "Shape", "Square")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .bind( "Color.Hue", "Red")
          .bind( "Color.Lightness", "Normal")
          .bind( "Color.Saturation", "Even")
          .bind( "Shape", "Square")
          .bind( "Size", "Medium")
          .build(),
          
          tc
          .start()
          .bind( "Color.Hue", "Red")
          .bind( "Color.Lightness", "Dark")
          .bind( "Color.Saturation", "Intense")
          .bind( "Shape", "Square")
          .bind( "Size", "Large")
          .build(),
          
          tc
          .start()
          .bind( "Color.Hue", "Green")
          .bind( "Color.Lightness", "Bright")
          .bind( "Color.Saturation", "Pale")
          .bind( "Shape", "Circle")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .bind( "Color.Hue", "Green")
          .bind( "Color.Lightness", "Normal")
          .bind( "Color.Saturation", "Even")
          .bind( "Shape", "Circle")
          .bind( "Size", "Medium")
          .build(),
          
          tc
          .start()
          .bind( "Color.Hue", "Green")
          .bind( "Color.Lightness", "Dark")
          .bind( "Color.Saturation", "Intense")
          .bind( "Shape", "Circle")
          .bind( "Size", "Large")
          .build(),
          
          tc
          .start()
          .bind( "Color.Hue", "Blue")
          .bind( "Color.Lightness", "Bright")
          .bind( "Color.Saturation", "Pale")
          .bind( "Shape", "Heart")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .bind( "Color.Hue", "Blue")
          .bind( "Color.Lightness", "Normal")
          .bind( "Color.Saturation", "Even")
          .bind( "Shape", "Heart")
          .bind( "Size", "Medium")
          .build(),
          
          tc.start()
          .bind( "Color.Hue", "Blue")
          .bind( "Color.Lightness", "Dark")
          .bind( "Color.Saturation", "Intense")
          .bind( "Shape", "Heart")
          .bind( "Size", "Large")
          .build()
          ),
        functionTestDef.getTestCases());
    }
  
  @Test
  public void getTuples_Once()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-2.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    generator.addCombiner
      ( new TupleCombiner(2)
        .addIncludedVar( "Color")
        .addIncludedVar( "Size"));
    
    // When...
    FunctionTestDef functionTestDef = generator.getTests( functionInputDef, null);

    // Expect...
    TCaseBuilder tc = new TCaseBuilder();
    assertSeqEquals
      ( "Test cases",
        TCaseBuilder.sequence
        ( tc
          .start()
          .bind( "Color", "Red")
          .bind( "Shape", "Square")
          .bind( "Size",  "Small")
          .build(),

          tc
          .start()
          .bind( "Color", "Red")
          .bind( "Shape", "Heart")
          .bind( "Size",  "Medium")
          .build(),

          tc
          .start()
          .bind( "Color", "Red")
          .bind( "Shape", "Square")
          .bind( "Size",  "Large")
          .build(),

          tc
          .start()
          .bind( "Color", "Green")
          .bind( "Shape", "Square")
          .bind( "Size",  "Small")
          .build(),

          tc
          .start()
          .bind( "Color", "Green")
          .bind( "Shape", "Square")
          .bind( "Size",  "Medium")
          .build(),

          tc
          .start()
          .bind( "Color", "Green")
          .bind( "Shape", "Square")
          .bind( "Size",  "Large")
          .build(),

          tc
          .start()
          .bind( "Color", "Blue")
          .bind( "Shape", "Square")
          .bind( "Size",  "Small")
          .build(),

          tc
          .start()
          .bind( "Color", "Blue")
          .bind( "Shape", "Square")
          .bind( "Size",  "Medium")
          .build(),

          tc
          .start()
          .bind( "Color", "Blue")
          .bind( "Shape", "Square")
          .bind( "Size",  "Large")
          .build(),

          tc
          .start()
          .bind( "Color", "Cyan")
          .bind( "Shape", "Circle")
          .bind( "Size",  "Small")
          .build(),

          tc
          .start()
          .bind( "Color", "Cyan")
          .bind( "Shape", "Circle")
          .bind( "Size",  "Medium")
          .build(),

          tc
          .start()
          .bind( "Color", "Cyan")
          .bind( "Shape", "Circle")
          .bind( "Size",  "Large")
          .build(),

          tc
          .start()
          .bind( "Color", "Magenta")
          .bind( "Shape", "Circle")
          .bind( "Size",  "Small")
          .build(),

          tc
          .start()
          .bind( "Color", "Magenta")
          .bind( "Shape", "Circle")
          .bind( "Size",  "Medium")
          .build(),

          tc
          .start()
          .bind( "Color", "Magenta")
          .bind( "Shape", "Circle")
          .bind( "Size",  "Large")
          .build(),

          tc
          .start()
          .bind( "Color", "Yellow")
          .bind( "Shape", "Circle")
          .bind( "Size",  "Small")
          .build(),

          tc
          .start()
          .bind( "Color", "Yellow")
          .bind( "Shape", "Circle")
          .bind( "Size",  "Medium")
          .build(),

          tc
          .start()
          .bind( "Color", "Yellow")
          .bind( "Shape", "Circle")
          .bind( "Size",  "Large")
          .build()
          ),
        functionTestDef.getTestCases());
    }
  
  @Test
  public void getTuples_Failures()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-3.xml");
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
          .bind( "Shape", "Circle")
          .bind( "Size", "Small")
          .build(),

          tc
          .start()
          .bind( "Color.Hue", "Green")
          .bind( "Color.Lightness", "Normal")
          .bind( "Color.Saturation", "Even")
          .bind( "Shape", "Heart")
          .bind( "Size", "Small")
          .build(),

          tc
          .start()
          .bind( "Color.Hue", "Blue")
          .bind( "Color.Lightness", "Dark")
          .bind( "Color.Saturation", "Intense")
          .bind( "Shape", "Heart")
          .bind( "Size", "Small")
          .build(),

          tc
          .start()
          .bind( "Color.Hue", "NA")
          .bind( "Color.Lightness", "NA")
          .bind( "Color.Saturation", "NA")
          .bind( "Shape", "Square")
          .bind( "Size", "Medium")
          .build(),

          tc
          .start()
          .bind( "Color.Hue", "NA")
          .bind( "Color.Lightness", "NA")
          .bind( "Color.Saturation", "NA")
          .bind( "Shape", "Circle")
          .bind( "Size", "Large")
          .build(),
          tc
          .start()
          .bind( "Color.Hue", "Green")
          .bind( "Color.Lightness", "Transparent", false)
          .bind( "Color.Saturation", "Even")
          .bind( "Shape", "Heart")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .bind( "Color.Hue", "Blue")
          .bind( "Color.Lightness", "Dark")
          .bind( "Color.Saturation", "Undefined", false)
          .bind( "Shape", "Heart")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .bind( "Color.Hue", "NA")
          .bind( "Color.Lightness", "NA")
          .bind( "Color.Saturation", "NA")
          .bind( "Shape", "Square")
          .bind( "Size", "Ginormous", false)
          .build()
          ),

        functionTestDef.getTestCases());
    }

  private SystemInputResources systemInputResources_ = new SystemInputResources( TestTupleGenerator.class);
  }
