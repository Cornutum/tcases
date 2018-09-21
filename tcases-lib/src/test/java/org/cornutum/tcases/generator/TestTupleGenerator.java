//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.*;
import org.cornutum.tcases.io.SystemInputResources;
import static org.cornutum.tcases.AssertTestDef.*;

import org.apache.commons.collections4.IteratorUtils;
import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;

import java.util.Collection;
import java.util.List;

/**
 * Runs tests for {@link TupleGenerator}
 *
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
    assertTestCasesComplete( functionInputDef, functionTestDef);

    TupleCombiner combiner = new TupleCombiner();
    Collection<Tuple> tuplesExpected = combiner.getTuples( functionInputDef);
    assertThat
      ( "Tuples included",
        getTuplesIncluded( tuplesExpected, functionTestDef),
        containsMembers( tuplesExpected));
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
    assertTestCasesComplete( functionInputDef, functionTestDef);

    TupleCombiner combiner = new TupleCombiner();
    Collection<Tuple> tuplesExpected = combiner.getTuples( functionInputDef);
    assertThat
      ( "Tuples included",
        getTuplesIncluded( tuplesExpected, functionTestDef),
        containsMembers( tuplesExpected));

    assertIncluded( functionTestDef, 0, tuplesFor( functionInputDef, "Shape", "Square", "Color"));
    assertIncluded( functionTestDef, 0, tuplesFor( functionInputDef, "Size", "Medium", "Color"));
    assertIncluded( functionTestDef, 0, tuplesFor( functionInputDef, "Size", "Large", "Color"));

    assertIncluded( functionTestDef, 0, tupleFor( functionInputDef).bind( "Color.Lightness", "Dark").bind( "Shape", "Circle").build());
    assertIncluded( functionTestDef, 0, tupleFor( functionInputDef).bind( "Color.Lightness", "Bright").bind( "Shape", "Heart").build());

    assertIncluded( functionTestDef, 0, tupleFor( functionInputDef).bind( "Size", "Medium").bind( "Shape", "Circle").build());
    assertIncluded( functionTestDef, 0, tupleFor( functionInputDef).bind( "Size", "Medium").bind( "Shape", "Heart").build());

    assertIncluded( functionTestDef, 0, tupleFor( functionInputDef).bind( "Size", "Large").bind( "Shape", "Square").build());
    }
  
  @Test
  public void getTests_Combined()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-0.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    TupleCombiner combiner1 = new TupleCombiner(2).addIncludedVar( "Color.Hue").addIncludedVar( "Size");
    generator.addCombiner( combiner1);

    TupleCombiner combiner2 = new TupleCombiner(2).addIncludedVar( "Color.Hue") .addIncludedVar( "Size");
    generator.addCombiner( combiner2);
    
    // When...
    FunctionTestDef functionTestDef = generator.getTests( functionInputDef, null);

    // Expect...
    assertTestCasesComplete( functionInputDef, functionTestDef);

    Collection<Tuple> tuplesExpected;

    tuplesExpected = combiner1.getTuples( functionInputDef);
    assertThat
      ( "Tuples included, combiner=" + combiner1,
        getTuplesIncluded( tuplesExpected, functionTestDef),
        containsMembers( tuplesExpected));

    tuplesExpected = combiner2.getTuples( functionInputDef);
    assertThat
      ( "Tuples included, combiner=" + combiner2,
        getTuplesIncluded( tuplesExpected, functionTestDef),
        containsMembers( tuplesExpected));
    }
  
  @Test
  public void getTests_Once()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-2.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    TupleCombiner combiner = new TupleCombiner(2).addIncludedVar( "Color").addIncludedVar( "Size");
    generator.addCombiner( combiner);
    
    // When...
    FunctionTestDef functionTestDef = generator.getTests( functionInputDef, null);

    // Expect...
    assertTestCasesComplete( functionInputDef, functionTestDef);

    Collection<Tuple> tuplesExpected = combiner.getTuples( functionInputDef);
    assertThat
      ( "Tuples included",
        getTuplesIncluded( tuplesExpected, functionTestDef),
        containsMembers( tuplesExpected));
        
    assertIncluded( functionTestDef, 1, "Shape", "Heart");
    assertIncluded( functionTestDef, 9, "Shape", "Circle");
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
    assertTestCasesComplete( functionInputDef, functionTestDef);

    TupleCombiner combiner = new TupleCombiner();
    Collection<Tuple> tuplesExpected = combiner.getTuples( functionInputDef);
    assertThat
      ( "Tuples included",
        getTuplesIncluded( tuplesExpected, functionTestDef),
        containsMembers( tuplesExpected));

    assertIncluded( functionTestDef, 1, "Color.Lightness", "Transparent", false);
    assertIncluded( functionTestDef, 1, "Color.Saturation", "Undefined", false);
    assertIncluded( functionTestDef, 1, "Size", "Ginormous", false);
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
    List<TestCase> expectedTestCases = IteratorUtils.toList( baseTestDef.getTestCases());
    List<TestCase> actualTestCases = IteratorUtils.toList( functionTestDef.getTestCases());
    assertThat( "When base tests same", actualTestCases, containsMembers( expectedTestCases));
    }
  
  @Test
  public void getTests_FromBaseTests_Changed()
    {
    // TBD
    }


  private SystemInputResources systemInputResources_ = new SystemInputResources( TestTupleGenerator.class);
  }
