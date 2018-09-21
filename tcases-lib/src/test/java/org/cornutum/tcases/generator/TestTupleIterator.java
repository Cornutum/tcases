//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.VarBindingDef;
import org.cornutum.tcases.VarDef;
import org.cornutum.tcases.VarValueDef;
import org.cornutum.tcases.conditions.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.ArrayList;
import java.util.List;

/**
 * Runs tests for {@link TupleIterator}
 *
 */
public class TestTupleIterator
  {
  @Test
  public void testCombinesValid()
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
    TupleIterator tuples = new TupleIterator( 3, varDefs);

    // Then...
    List<Tuple> expectedTuples = new ArrayList<Tuple>();
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
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
          new VarBindingDef( var3, value32)));

    assertThat( "Tuples", tuples, visitsList( expectedTuples));
    }
  
  @Test
  public void testAllPairs()
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
    TupleIterator tuples = new TupleIterator( 2, varDefs);

    // Then...
    List<Tuple> expectedTuples = new ArrayList<Tuple>();
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value21)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value21)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value22)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value22)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value32)));
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
        ( new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var2, value21),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value32)));

    assertThat( "Tuples", tuples, visitsList( expectedTuples));
    }
  
  @Test
  public void testAllPairsCompatible()
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
    TupleIterator tuples = new TupleIterator( 2, varDefs);

    // Then...
    List<Tuple> expectedTuples = new ArrayList<Tuple>();
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
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value32)));
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

    assertThat( "Tuples", tuples, visitsList( expectedTuples));
    }
  
  @Test
  public void testAllPairsRandom()
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
    RandSeq randSeq = new RandSeq( getClass().getName().hashCode());
    TupleIterator tuples = new TupleIterator( 2, varDefs, randSeq);

    // Then...
    List<Tuple> expectedTuples = new ArrayList<Tuple>();
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value21)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value21)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value22)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value22)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value32)));
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
        ( new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var2, value21),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( new Tuple
        ( new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value32)));

    assertThat( "Tuples", tuples, visitsMembers( expectedTuples));
    }

  @Test
  public void testMinTupleSize()
    {
    // Given...
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( new VarDef( "var-1").addValue( new VarValueDef( "value-1-1")));
    varDefs.add( new VarDef( "var-2").addValue( new VarValueDef( "value-2-1")));
    
    expectFailure( IllegalArgumentException.class)
      .when( () -> new TupleIterator( 0, varDefs))
      .then( failure -> {
        assertThat( "Failure message", failure.getMessage(), is( "Can't create 0-tuples for a list of size=2"));
        });
    }

  @Test
  public void testMinVars()
    {
    // Given...
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( new VarDef( "var-1").addValue( new VarValueDef( "value-1-1")));
    varDefs.add( new VarDef( "var-2").addValue( new VarValueDef( "value-2-1")));
    
    expectFailure( IllegalArgumentException.class)
      .when( () -> new TupleIterator( 3, varDefs))
      .then( failure -> {
        assertThat( "Failure message", failure.getMessage(), is( "Can't create 3-tuples for a list of size=2"));
        });
    }

  @Test
  public void testValidValues()
    {
    // Given...
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( new VarDef( "var-1").addValue( new VarValueDef( "value-1-1")));
    varDefs.add( new VarDef( "var-2").addValue( new VarValueDef( "value-2-1", VarValueDef.Type.FAILURE)));

    expectFailure( IllegalStateException.class)
      .when( () -> toStream( new TupleIterator( 2, varDefs)).count())
      .then( failure -> {
        assertThat( "Failure message", failure.getMessage(), is( "Can't complete tuples -- no valid values defined for var=VarDef[var-2]"));
        });
    }
  }



