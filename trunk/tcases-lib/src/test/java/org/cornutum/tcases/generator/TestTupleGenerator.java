//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.*;
import org.cornutum.tcases.io.SystemInputResources;
import org.cornutum.tcases.io.SystemTestResources;
import static org.cornutum.tcases.util.Asserts.*;

import org.junit.Test;

/**
 * Runs tests for {@link TupleGenerator}
 *
 * @version $Revision$, $Date$
 */
public class TestTupleGenerator
  {
  @Test
  public void getTests_Basic()
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
  public void getTests_Constrained()
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
  public void getTests_Combined()
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
  public void getTests_Once()
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
  public void getTests_Failures()
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
          .bind( "Color.Hue", "Red")
          .bind( "Color.Lightness", "Transparent", false)
          .bind( "Color.Saturation", "Pale")
          .bind( "Shape", "Heart")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .bind( "Color.Hue", "Green")
          .bind( "Color.Lightness", "Bright")
          .bind( "Color.Saturation", "Undefined", false)
          .bind( "Shape", "Circle")
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
  
  @Test
  public void getTests_FromBaseTests_Same()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-4.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    generator.addCombiner
      ( new TupleCombiner(2)
        .addIncludedVar( "Shape")
        .addIncludedVar( "Size"));
    
    // When...
    FunctionTestDef baseTestDef = generator.getTests( functionInputDef, null);
    FunctionTestDef functionTestDef = generator.getTests( functionInputDef, baseTestDef);

    // Expect...
    TCaseBuilder tc = new TCaseBuilder();
    assertSeqEquals
      ( "Test cases",
        TCaseBuilder.sequence
        ( tc
          .start()
          .bind( "Color", "Red")
          .bind( "Shape", "Square")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .bind( "Color", "Green")
          .bind( "Shape", "Square")
          .bind( "Size", "Medium")
          .build(),
          
          tc
          .start()
          .bind( "Color", "Blue")
          .bind( "Shape", "Square")
          .bind( "Size", "Large")
          .build(),
          
          tc
          .start()
          .bind( "Color", "Red")
          .bind( "Shape", "Heart")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .bind( "Color", "Green")
          .bind( "Shape", "Heart")
          .bind( "Size", "Medium")
          .build(),
          
          tc
          .start()
          .bind( "Color", "Blue")
          .bind( "Shape", "Heart")
          .bind( "Size", "Large")
          .build(),
          
          tc
          .start()
          .bind( "Color", "Red")
          .bind( "Shape", "Circle")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .bind( "Color", "Green")
          .bind( "Shape", "Circle")
          .bind( "Size", "Medium")
          .build(),
          
          tc
          .start()
          .bind( "Color", "Blue")
          .bind( "Shape", "Circle")
          .bind( "Size", "Large")
          .build(),
          
          tc
          .start()
          .bind( "Color", "Undefined", false)
          .bind( "Shape", "Square")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .bind( "Color", "Red")
          .bind( "Shape", "Undefined", false)
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .bind( "Color", "Green")
          .bind( "Shape", "Square")
          .bind( "Size", "Undefined", false)
          .build() 
          ),
        functionTestDef.getTestCases());
    }
  
  @Test
  public void getTests_FromBaseTests_Changed()
    {
    // Given...
    SystemTestDef systemTestDef = systemTestResources_.read( "system-test-def-4.xml");
    FunctionTestDef baseTestDef = systemTestDef.getFunctionTestDef( "Make");

    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-5.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    generator.addCombiner
      ( new TupleCombiner(2)
        .addIncludedVar( "Shape")
        .addIncludedVar( "Size"));
    
    // When...
    FunctionTestDef functionTestDef = generator.getTests( functionInputDef, baseTestDef);
    
    // Expect...
    TCaseBuilder tc = new TCaseBuilder();
    assertSeqEquals
      ( "Test cases",
        new TestCase[]
        {
          tc
          .start()
          .id(1)
          .bind( "Color", "Green")
          .bind( "Shape", "Diamond")
          .bind( "Size", "Medium")
          .build(),
          
          tc
          .start()
          .id(4)
          .bind( "Color", "Green")
          .bind( "Shape", "Heart")
          .bind( "Size", "Medium")
          .build(),
          
          tc
          .start()
          .id(7)
          .bind( "Color", "Green")
          .bind( "Shape", "Circle")
          .bind( "Size", "Medium")
          .build(),
          
          tc
          .start()
          .id(11)
          .bind( "Color", "Green")
          .bind( "Shape", "Diamond")
          .bind( "Size", "Undefined", false)
          .build(),
          
          tc
          .start()
          .id(12)
          .bind( "Color", "Blue")
          .bind( "Shape", "Diamond")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .id(13)
          .bind( "Color", "Undefined")
          .bind( "Shape", "Diamond")
          .bind( "Size", "Ginormous")
          .build(),
          
          tc
          .start()
          .id(14)
          .bind( "Color", "Green")
          .bind( "Shape", "Heart")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .id(15)
          .bind( "Color", "Blue")
          .bind( "Shape", "Heart")
          .bind( "Size", "Ginormous")
          .build(),
          
          tc
          .start()
          .id(16)
          .bind( "Color", "Undefined")
          .bind( "Shape", "Circle")
          .bind( "Size", "Small")
          .build(),
          
          tc
          .start()
          .id(17)
          .bind( "Color", "Green")
          .bind( "Shape", "Circle")
          .bind( "Size", "Ginormous")
          .build(),
          
          tc
          .start()
          .id(18)
          .bind( "Color", "Red")
          .bind( "Shape", "Diamond")
          .bind( "Size", "Medium")
          .build(),
          
          tc
          .start()
          .id(19)
          .bind( "Color", "Blue")
          .bind( "Shape", "Undefined", false)
          .bind( "Size", "Medium")
          .build(),
          
          tc
          .start()
          .id(20)
          .bind( "Color", "Undefined")
          .bind( "Shape", "Heart")
          .bind( "Size", "Large", false)
          .build()
        },
        functionTestDef.getTestCases());
    }

  private SystemInputResources systemInputResources_ = new SystemInputResources( TestTupleGenerator.class);
  private SystemTestResources systemTestResources_ = new SystemTestResources( TestTupleGenerator.class);
  }
