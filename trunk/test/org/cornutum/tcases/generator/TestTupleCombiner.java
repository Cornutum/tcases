//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.FunctionInputDef;
import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.VarBindingDef;
import org.cornutum.tcases.VarDef;
import org.cornutum.tcases.VarValueDef;
import org.cornutum.tcases.conditions.*;
import org.cornutum.tcases.io.SystemInputResources;
import org.cornutum.tcases.util.Asserts;

import org.junit.Test;
import static org.junit.Assert.*;
import org.apache.commons.collections15.IteratorUtils;
import org.apache.commons.collections15.Transformer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Runs tests for {@link TupleCombiner}
 *
 * @version $Revision$, $Date$
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

    Asserts.assertSetEquals( "Tuples", expectedTuples, tuples);
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

    Asserts.assertSetEquals( "Tuples", expectedTuples, tuples);
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

    Asserts.assertSetEquals( "Tuples", expectedTuples, tuples);
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

    Asserts.assertSetEquals( "Tuples", expectedTuples, tuples);
    }

  @Test
  public void getTuples_MinVars()
    {
    // Given...
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( new VarDef( "var-1").addValue( new VarValueDef( "value-1-1")));
    varDefs.add( new VarDef( "var-2").addValue( new VarValueDef( "value-2-1")));
    
    // When...
    try
      {
      Collection<Tuple> tuples = TupleCombiner.getTuples( varDefs, 3);
      fail( "Unexpected TupleCombiner=" + tuples);
      }

    // Then...
    catch( IllegalArgumentException iae)
      {
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Unexpected exception", e);
      }
    }

  @Test
  public void getTuples_ValidValues()
    {
    // Given...
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( new VarDef( "var-1").addValue( new VarValueDef( "value-1-1")));
    varDefs.add( new VarDef( "var-2").addValue( new VarValueDef( "value-2-1", VarValueDef.Type.FAILURE)));
    
    // When...
    try
      {
      Collection<Tuple> tuples = TupleCombiner.getTuples( varDefs, 2);
      Asserts.assertSetEquals( "Tuples", (Collection<Tuple>)null, tuples);
      }

    // Then...
    catch( IllegalStateException ise)
      {
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Unexpected exception", e);
      }
    }
  
  @Test
  public void getCombinedVars_All()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-0.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleCombiner combiner = new TupleCombiner();
    
    // When...
    List<VarDef> combined = combiner.getCombinedVars( functionInputDef);
    
    // Then...
    Asserts.assertSetEquals
      ( "Combined vars",
        new String[]  
        {
          "Color.Hue",
          "Color.Lightness",
          "Color.Saturation",
          "Shape",
          "Size"
        },
        IteratorUtils.transformedIterator
        ( combined.iterator(),
          new Transformer<VarDef,String>()
            {
            public String transform( VarDef var)
              {
              return var.getPathName();
              }
            }));
    }
  
  @Test
  public void getCombinedVars_Included()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-0.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleCombiner combiner = new TupleCombiner().addIncludedVar( "Size");
    
    // When...
    List<VarDef> combined = combiner.getCombinedVars( functionInputDef);
    
    // Then...
    Asserts.assertSetEquals
      ( "Combined vars",
        new String[]  
        {
          "Size"
        },
        IteratorUtils.transformedIterator
        ( combined.iterator(),
          new Transformer<VarDef,String>()
            {
            public String transform( VarDef var)
              {
              return var.getPathName();
              }
            }));
    }
  
  @Test
  public void getCombinedVars_Excluded()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-0.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleCombiner combiner = new TupleCombiner().addExcludedVar( "Size");
    
    // When...
    List<VarDef> combined = combiner.getCombinedVars( functionInputDef);
    
    // Then...
    Asserts.assertSetEquals
      ( "Combined vars",
        new String[]  
        {
          "Color.Hue",
          "Color.Lightness",
          "Color.Saturation",
          "Shape"
        },
        IteratorUtils.transformedIterator
        ( combined.iterator(),
          new Transformer<VarDef,String>()
            {
            public String transform( VarDef var)
              {
              return var.getPathName();
              }
            }));
    }
  
  @Test
  public void getCombinedVars_Some()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-0.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleCombiner combiner = new TupleCombiner().addIncludedVar( "Color.*").addExcludedVar( "Color.Hue");
    
    // When...
    List<VarDef> combined = combiner.getCombinedVars( functionInputDef);
    
    // Then...
    Asserts.assertSetEquals
      ( "Combined vars",
        new String[]  
        {
          "Color.Lightness",
          "Color.Saturation"
        },
        IteratorUtils.transformedIterator
        ( combined.iterator(),
          new Transformer<VarDef,String>()
            {
            public String transform( VarDef var)
              {
              return var.getPathName();
              }
            }));
    }
  
  @Test
  public void getCombinedVars_IncludedNotApplicable()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-0.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleCombiner combiner = new TupleCombiner().addIncludedVar( "Size.*").addExcludedVar( "Color.Hue");
    
    // When...
    try
      {
      List<VarDef> combined = combiner.getCombinedVars( functionInputDef);

      fail( "Unexpected vars=" + combined);
      }

    // Then...
    catch( IllegalArgumentException iae)
      {
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Unexpected exception", e);
      }
    }
  
  @Test
  public void getCombinedVars_ExcludedNotApplicable()
    {
    // Given...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-0.xml");
    FunctionInputDef functionInputDef = systemInputDef.getFunctionInputDef( "Make");
    TupleCombiner combiner = new TupleCombiner().addExcludedVar( "Color.Red");
    
    // When...
    try
      {
      List<VarDef> combined = combiner.getCombinedVars( functionInputDef);

      fail( "Unexpected vars=" + combined);
      }

    // Then...
    catch( IllegalArgumentException iae)
      {
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Unexpected exception", e);
      }
    }

  private SystemInputResources systemInputResources_ = new SystemInputResources( TestTupleCombiner.class);
  }
