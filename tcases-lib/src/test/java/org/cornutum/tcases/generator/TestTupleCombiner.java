//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.*;
import org.cornutum.tcases.conditions.*;

import org.apache.commons.collections4.IteratorUtils;
import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Runs tests for {@link TupleCombiner}
 *
 */
public class TestTupleCombiner
  {
  @Test
  public void getTuples_CombinesValid()
    {
    // Given...
    VarValueDef value11 = new VarValueDef( "value-1-1");
    VarValueDef value12 = new VarValueDef( "value-1-2");
    VarValueDef value13 = new VarValueDef( "value-1-3", VarValueDef.Type.FAILURE);
    VarDef      var1    = new VarDef( "var-1").addValue( value11).addValue( value12).addValue( value13);

    VarValueDef value21 = new VarValueDef( "value-2-1", VarValueDef.Type.FAILURE);
    VarValueDef value22 = new VarValueDef( "value-2-2");
    VarDef var2 = new VarDef( "var-2").addValue( value21).addValue( value22);

    VarValueDef value31 = new VarValueDef( "value-3-1");
    VarValueDef value32 = new VarValueDef( "value-3-2");
    VarDef var3 = new VarDef( "var-3").addValue( value31).addValue( value32);
    
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( var1);
    varDefs.add( var2);
    varDefs.add( var3);
    
    // When...
    Collection<Tuple> tuples = TupleCombiner.getTuples( varDefs, 3);
    
    // Then...
    Collection<Tuple> expectedTuples = new ArrayList<Tuple>();
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value32)));

    assertThat( "Tuples", tuples, containsMembers( expectedTuples));
    }
  
  @Test
  public void getTuples_AllPairs()
    {
    // Given...
    VarValueDef value11 = new VarValueDef( "value-1-1");
    VarValueDef value12 = new VarValueDef( "value-1-2");
    VarDef      var1    = new VarDef( "var-1").addValue( value11).addValue( value12);

    VarValueDef value21 = new VarValueDef( "value-2-1");
    VarValueDef value22 = new VarValueDef( "value-2-2");
    VarDef      var2    = new VarDef( "var-2").addValue( value21).addValue( value22);

    VarValueDef value31 = new VarValueDef( "value-3-1");
    VarValueDef value32 = new VarValueDef( "value-3-2");
    VarDef      var3    = new VarDef( "var-3").addValue( value31).addValue( value32);
    
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( var1);
    varDefs.add( var2);
    varDefs.add( var3);
    
    // When...
    Collection<Tuple> tuples = TupleCombiner.getTuples( varDefs, 2);

    // Then...
    Collection<Tuple> expectedTuples = new ArrayList<Tuple>();
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value21)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value22)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value21)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value22)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var2, value21),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var2, value21),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value32)));

    assertThat( "Tuples", tuples, containsMembers( expectedTuples));
    }
  
  @Test
  public void getTuples_AllPairsCompatible()
    {
    // Given...
    VarValueDef value11 = new VarValueDef( "value-1-1").addProperties( "R1", "V1");
    VarValueDef value12 = new VarValueDef( "value-1-2").addProperties( "R1", "V2");
    VarDef      var1    = new VarDef( "var-1").addValue( value11).addValue( value12);

    VarValueDef value21 = new VarValueDef( "value-2-1").addProperties( "R2", "V1");
    VarValueDef value22 = new VarValueDef( "value-2-2").addProperties( "R2", "V2");
    VarDef      var2    = new VarDef( "var-2").addValue( value21).addValue( value22);

    VarValueDef value31 = new VarValueDef( "value-3-1").addProperties( "R3", "V1");
    VarValueDef value32 = new VarValueDef( "value-3-2").addProperties( "R3", "V2");
    VarDef      var3    = new VarDef( "var-3").addValue( value31).addValue( value32);

    value11.setCondition( new Not( new ContainsAny( "R2")));
    value22.setCondition( new AllOf( new ContainsAny( "U1"), new Not( new ContainsAll( "R3", "V1"))));
    value31.setCondition( new AnyOf( new Not( new ContainsAny( "R1")), new Not( new ContainsAll( "R2", "R3"))));
    
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( var1);
    varDefs.add( var2);
    varDefs.add( var3);
    
    // When...
    Collection<Tuple> tuples = TupleCombiner.getTuples( varDefs, 2);

    // Then...
    Collection<Tuple> expectedTuples = new ArrayList<Tuple>();
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value21)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value22)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var2, value21),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var2, value21),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value32)));

    assertThat( "Tuples", tuples, containsMembers( expectedTuples));
    }

  @Test
  public void getTuples_AllPermutations()
    {
    // Given...
    VarValueDef value11 = new VarValueDef( "value-1-1");
    VarValueDef value12 = new VarValueDef( "value-1-2");
    VarDef      var1    = new VarDef( "var-1").addValue( value11).addValue( value12);

    VarValueDef value21 = new VarValueDef( "value-2-1");
    VarValueDef value22 = new VarValueDef( "value-2-2");
    VarDef      var2    = new VarDef( "var-2").addValue( value21).addValue( value22);

    VarValueDef value31 = new VarValueDef( "value-3-1");
    VarValueDef value32 = new VarValueDef( "value-3-2");
    VarDef      var3    = new VarDef( "var-3").addValue( value31).addValue( value32);
    
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( var1);
    varDefs.add( var2);
    varDefs.add( var3);
    
    // When...
    Collection<Tuple> tuples = TupleCombiner.getTuples( varDefs, 0);

    // Then...
    Collection<Tuple> expectedTuples = new ArrayList<Tuple>();
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value21),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value21),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value21),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value21),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value32)));

    assertThat( "Tuples", tuples, containsMembers( expectedTuples));
    }

  @Test
  public void getTuples_MinVars()
    {
    // Given...
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( new VarDef( "var-1").addValue( new VarValueDef( "value-1-1")));
    varDefs.add( new VarDef( "var-2").addValue( new VarValueDef( "value-2-1")));
    
    expectFailure( IllegalArgumentException.class)
      .when( () -> TupleCombiner.getTuples( varDefs, 3))
      .then( failure -> {
        assertThat( "Failure message", failure.getMessage(), is( "Can't create 3-tuples for 2 combined variables"));
        });
    }

  @Test
  public void getTuples_ValidValues()
    {
    // Given...
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( new VarDef( "var-1").addValue( new VarValueDef( "value-1-1")));
    varDefs.add( new VarDef( "var-2").addValue( new VarValueDef( "value-2-1", VarValueDef.Type.FAILURE)));
    
    expectFailure( IllegalStateException.class)
      .when( () -> TupleCombiner.getTuples( varDefs, 2))
      .then( failure -> {
        assertThat( "Failure message", failure.getMessage(), is( "Can't complete tuples -- no valid values defined for var=VarDef[var-2]"));
        });
    }
  
  @Test
  public void getCombinedVars_All()
    {
    // Given...
    SystemInputDef systemInputDef = getSystemInputDef();
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleCombiner combiner = new TupleCombiner();
    
    // When...
    List<VarDef> combined = combiner.getCombinedVars( functionInputDef);
    
    // Then...
    assertThat(
      "Combined vars",
      IteratorUtils.transformedIterator(
        combined.iterator(),
        VarDef::getPathName),
      visitsMembers(
        "Color.Hue",
        "Color.Lightness",
        "Color.Saturation",
        "Shape",
        "Size"));
    }
  
  @Test
  public void getCombinedVars_Included()
    {
    // Given...
    SystemInputDef systemInputDef = getSystemInputDef();
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleCombiner combiner = new TupleCombiner().addIncludedVar( "Size");
    
    // When...
    List<VarDef> combined = combiner.getCombinedVars( functionInputDef);
    
    // Then...
    assertThat(
      "Combined vars",
      IteratorUtils.transformedIterator(
        combined.iterator(),
        VarDef::getPathName),
      visitsMembers( "Size"));
    }
  
  @Test
  public void getCombinedVars_Excluded()
    {
    // Given...
    SystemInputDef systemInputDef = getSystemInputDef();
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleCombiner combiner = new TupleCombiner().addExcludedVar( "Size");
    
    // When...
    List<VarDef> combined = combiner.getCombinedVars( functionInputDef);
    
    // Then...
    assertThat(
      "Combined vars",
      IteratorUtils.transformedIterator(
        combined.iterator(),
        VarDef::getPathName),
      visitsMembers(
        "Color.Hue",
        "Color.Lightness",
        "Color.Saturation",
        "Shape"));
    }
  
  @Test
  public void getCombinedVars_Some()
    {
    // Given...
    SystemInputDef systemInputDef = getSystemInputDef();
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleCombiner combiner = new TupleCombiner().addIncludedVar( "Color.*").addExcludedVar( "Color.Hue");
    
    // When...
    List<VarDef> combined = combiner.getCombinedVars( functionInputDef);
    
    // Then...
    assertThat(
      "Combined vars",
      IteratorUtils.transformedIterator(
        combined.iterator(),
        VarDef::getPathName),
      visitsMembers(
        "Color.Lightness",
        "Color.Saturation"));
    }
  
  @Test
  public void getCombinedVars_IncludedNotApplicable()
    {
    // Given...
    SystemInputDef systemInputDef = getSystemInputDef();
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleCombiner combiner = new TupleCombiner().addIncludedVar( "Size.*").addExcludedVar( "Color.Hue");
    
    expectFailure( IllegalArgumentException.class)
      .when( () -> combiner.getCombinedVars( functionInputDef))
      .then( failure -> {
        assertThat( "Failure message", failure.getMessage(), is( "Can't apply " + combiner + " to FunctionInputDef[Make]"));
        });
    }
  
  @Test
  public void getCombinedVars_ExcludedNotApplicable()
    {
    // Given...
    SystemInputDef systemInputDef = getSystemInputDef();
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleCombiner combiner = new TupleCombiner().addExcludedVar( "Color.Red");
    
    expectFailure( IllegalArgumentException.class)
      .when( () -> combiner.getCombinedVars( functionInputDef))
      .then( failure -> {
        assertThat( "Failure message", failure.getMessage(), is( "Can't apply " + combiner + " to FunctionInputDef[Make]"));
        });
    }

  private SystemInputDef getSystemInputDef()
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
  }
