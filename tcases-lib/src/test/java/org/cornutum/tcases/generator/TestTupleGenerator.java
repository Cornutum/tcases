//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.AssertTestDef.*;
import static org.cornutum.tcases.VarValueDef.Type.*;
import static org.cornutum.tcases.conditions.Conditions.*;

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
    SystemInputDef systemInputDef = getSystemInputDefBasic();
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    // When...
    FunctionTestDef functionTestDef = Tcases.getTests( functionInputDef, generator, null);

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
    SystemInputDef systemInputDef = getSystemInputDefConstrained();
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    // When...
    FunctionTestDef functionTestDef = Tcases.getTests( functionInputDef, generator, null);

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
    SystemInputDef systemInputDef = getSystemInputDefBasic();
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    TupleCombiner combiner1 = new TupleCombiner(2).addIncludedVar( "Color.Hue").addIncludedVar( "Size");
    generator.addCombiner( combiner1);

    TupleCombiner combiner2 = new TupleCombiner(2).addIncludedVar( "Color.Hue") .addIncludedVar( "Size");
    generator.addCombiner( combiner2);
    
    // When...
    FunctionTestDef functionTestDef = Tcases.getTests( functionInputDef, generator, null);

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
    SystemInputDef systemInputDef = getSystemInputDefOnce();
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    TupleCombiner combiner = new TupleCombiner(2).addIncludedVar( "Color").addIncludedVar( "Size");
    generator.addCombiner( combiner);
    
    // When...
    FunctionTestDef functionTestDef = Tcases.getTests( functionInputDef, generator, null);

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
    SystemInputDef systemInputDef = getSystemInputDefFailures();
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    // When...
    FunctionTestDef functionTestDef = Tcases.getTests( functionInputDef, generator, null);

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
    SystemInputDef systemInputDef = getSystemInputDefBase();
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleGenerator generator = new TupleGenerator();

    generator.addCombiner
      ( new TupleCombiner(2)
        .addIncludedVar( "Shape")
        .addIncludedVar( "Size"));
    
    // When...
    FunctionTestDef baseTestDef = Tcases.getTests( functionInputDef, generator, null);
    FunctionTestDef functionTestDef = Tcases.getTests( functionInputDef, generator, baseTestDef);

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

  private SystemInputDef getSystemInputDefBasic()
    {
    return
      SystemInputDefBuilder.with( "Things")

      .functions(
        FunctionInputDefBuilder.with( "Make")
        .vars(
          "arg",

          VarSetBuilder.with( "Color")
          .members(
            VarDefBuilder.with( "Hue")
            .values(
              VarValueDefBuilder.with( "Red")
              .build(),
              VarValueDefBuilder.with( "Green")
              .build(),
              VarValueDefBuilder.with( "Blue")
              .build())
            .build(),

            VarDefBuilder.with( "Lightness")
            .values(
              VarValueDefBuilder.with( "Bright")
              .build(),
              VarValueDefBuilder.with( "Normal")
              .build(),
              VarValueDefBuilder.with( "Dark")
              .build())
            .build(),

            VarDefBuilder.with( "Saturation")
            .values(
              VarValueDefBuilder.with( "Pale")
              .build(),
              VarValueDefBuilder.with( "Even")
              .build(),
              VarValueDefBuilder.with( "Intense")
              .build())
            .build())
          .build(),

          VarDefBuilder.with( "Size")
          .values(
            VarValueDefBuilder.with( "Small")
            .build(),
            VarValueDefBuilder.with( "Medium")
            .build(),
            VarValueDefBuilder.with( "Large")
            .build())
          .build(),

          VarDefBuilder.with( "Shape")
          .values(
            VarValueDefBuilder.with( "Square")
            .build(),
            VarValueDefBuilder.with( "Circle")
            .build(),
            VarValueDefBuilder.with( "Heart")
            .build())
          .build())
        .build())
      .build();               
    }

  private SystemInputDef getSystemInputDefConstrained()
    {
    return
      SystemInputDefBuilder.with( "Things")

      .functions(
        FunctionInputDefBuilder.with( "Make")
        .vars(
          "arg",

          VarSetBuilder.with( "Color")
          .when( allOf( has( "small"), not( "polygon")))
          .members(
            VarDefBuilder.with( "Hue")
            .values(
              VarValueDefBuilder.with( "Red")
              .build(),
              VarValueDefBuilder.with( "Green")
              .build(),
              VarValueDefBuilder.with( "Blue")
              .build())
            .build(),

            VarDefBuilder.with( "Lightness")
            .values(
              VarValueDefBuilder.with( "Bright")
              .when( has( "round"))
              .build(),
              VarValueDefBuilder.with( "Normal")
              .build(),
              VarValueDefBuilder.with( "Dark")
              .when( not( "round"))
              .build())
            .build(),

            VarDefBuilder.with( "Saturation")
            .values(
              VarValueDefBuilder.with( "Pale")
              .build(),
              VarValueDefBuilder.with( "Even")
              .build(),
              VarValueDefBuilder.with( "Intense")
              .build())
            .build())
          .build(),

          VarDefBuilder.with( "Size")
          .values(
            VarValueDefBuilder.with( "Small")
            .properties( "small")
            .build(),
            VarValueDefBuilder.with( "Medium")
            .when( has( "polygon"))
            .build(),
            VarValueDefBuilder.with( "Large")
            .when( not( "polygon"))
            .build())
          .build(),

          VarDefBuilder.with( "Shape")
          .values(
            VarValueDefBuilder.with( "Square")
            .properties( "polygon")
            .build(),
            VarValueDefBuilder.with( "Circle")
            .properties( "round")
            .build(),
            VarValueDefBuilder.with( "Heart")
            .build())
          .build())
        .build())
      .build();               
    }

  private SystemInputDef getSystemInputDefOnce()
    {
    return
      SystemInputDefBuilder.with( "Things")

      .functions(
        FunctionInputDefBuilder.with( "Make")
        .vars(
          "arg",

          VarDefBuilder.with( "Color")
          .values(
            VarValueDefBuilder.with( "Red")
            .properties( "primary")
            .build(),
            VarValueDefBuilder.with( "Green")
            .properties( "primary")
            .build(),
            VarValueDefBuilder.with( "Blue")
            .properties( "primary")
            .build(),
            VarValueDefBuilder.with( "Cyan")
            .build(),
            VarValueDefBuilder.with( "Magenta")
            .build(),
            VarValueDefBuilder.with( "Yellow")
            .build())
          .build(),

          VarDefBuilder.with( "Size")
          .values(
            VarValueDefBuilder.with( "Small")
            .type( ONCE)
            .build(),
            VarValueDefBuilder.with( "Medium")
            .build(),
            VarValueDefBuilder.with( "Large")
            .build())
          .build(),

          VarDefBuilder.with( "Shape")
          .values(
            VarValueDefBuilder.with( "Square")
            .when( has( "primary"))
            .build(),
            VarValueDefBuilder.with( "Circle")
            .when( not( "primary"))
            .type( ONCE)
            .build(),
            VarValueDefBuilder.with( "Heart")
            .when( has( "primary"))
            .type( ONCE)
            .build())
          .build())
        .build())
      .build();               
    }

  private SystemInputDef getSystemInputDefFailures()
    {
    return
      SystemInputDefBuilder.with( "Things")

      .functions(
        FunctionInputDefBuilder.with( "Make")
        .vars(
          "arg",

          VarSetBuilder.with( "Color")
          .when( allOf( has( "small"), not( "polygon")))
          .members(
            VarDefBuilder.with( "Hue")
            .values(
              VarValueDefBuilder.with( "Red")
              .build(),
              VarValueDefBuilder.with( "Green")
              .build(),
              VarValueDefBuilder.with( "Blue")
              .build())
            .build(),

            VarDefBuilder.with( "Lightness")
            .values(
              VarValueDefBuilder.with( "Bright")
              .when( has( "round"))
              .build(),
              VarValueDefBuilder.with( "Normal")
              .build(),
              VarValueDefBuilder.with( "Dark")
              .when( not( "round"))
              .build(),
              VarValueDefBuilder.with( "Transparent")
              .when( not( "round"))
              .type( FAILURE)
              .build())
            .build(),

            VarDefBuilder.with( "Saturation")
            .values(
              VarValueDefBuilder.with( "Pale")
              .build(),
              VarValueDefBuilder.with( "Even")
              .build(),
              VarValueDefBuilder.with( "Intense")
              .build(),
              VarValueDefBuilder.with( "Undefined")
              .type( FAILURE)
              .build())
            .build())
          .build(),

          VarDefBuilder.with( "Size")
          .values(
            VarValueDefBuilder.with( "Ginormous")
            .type( FAILURE)
            .build(),
            VarValueDefBuilder.with( "Small")
            .properties( "small")
            .build(),
            VarValueDefBuilder.with( "Medium")
            .when( has( "polygon"))
            .build(),
            VarValueDefBuilder.with( "Large")
            .when( not( "polygon"))
            .build())
          .build(),

          VarDefBuilder.with( "Shape")
          .values(
            VarValueDefBuilder.with( "Square")
            .properties( "polygon")
            .build(),
            VarValueDefBuilder.with( "Circle")
            .properties( "round")
            .build(),
            VarValueDefBuilder.with( "Heart")
            .build())
          .build())
        .build())
      .build();               
    }

  private SystemInputDef getSystemInputDefBase()
    {
    return
      SystemInputDefBuilder.with( "Things")

      .functions(
        FunctionInputDefBuilder.with( "Make")
        .vars(
          "arg",

          VarDefBuilder.with( "Color")
          .values(
            VarValueDefBuilder.with( "Red")
            .build(),
            VarValueDefBuilder.with( "Green")
            .build(),
            VarValueDefBuilder.with( "Blue")
            .build(),
            VarValueDefBuilder.with( "Undefined")
            .type( FAILURE)
            .build())
          .build(),

          VarDefBuilder.with( "Size")
          .values(
            VarValueDefBuilder.with( "Small")
            .build(),
            VarValueDefBuilder.with( "Medium")
            .build(),
            VarValueDefBuilder.with( "Large")
            .build(),
            VarValueDefBuilder.with( "Undefined")
            .type( FAILURE)
            .build())
          .build(),

          VarDefBuilder.with( "Shape")
          .values(
            VarValueDefBuilder.with( "Square")
            .build(),
            VarValueDefBuilder.with( "Circle")
            .build(),
            VarValueDefBuilder.with( "Heart")
            .build(),
            VarValueDefBuilder.with( "Undefined")
            .type( FAILURE)
            .build())
          .build())
        .build())
      .build();               
    }
  }
