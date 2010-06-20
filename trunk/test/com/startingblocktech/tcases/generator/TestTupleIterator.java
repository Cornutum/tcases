//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.VarDef;
import com.startingblocktech.tcases.VarValueDef;
import com.startingblocktech.tcases.util.Asserts;

import org.junit.Test;
import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Runs tests for {@link TupleIterator}
 *
 * @version $Revision$, $Date$
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
    
    VarDef var2 = new VarDef( "var-2");

    VarValueDef value31 = new VarValueDef( "value-3-1", VarValueDef.Type.FAILURE);
    VarValueDef value32 = new VarValueDef( "value-3-2");
    VarDef var3 = new VarDef( "var-3").addValue( value31).addValue( value32);
    
    VarValueDef value41 = new VarValueDef( "value-4-1", VarValueDef.Type.FAILURE);
    VarDef var4 = new VarDef( "var-4").addValue( value41);

    VarValueDef value51 = new VarValueDef( "value-5-1");
    VarValueDef value52 = new VarValueDef( "value-5-2");
    VarDef var5 = new VarDef( "var-5").addValue( value51).addValue( value52);
    
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( var1);
    varDefs.add( var2);
    varDefs.add( var3);
    varDefs.add( var4);
    varDefs.add( var5);
    
    // When...
    TupleIterator tuples = new TupleIterator( 5, varDefs);

    // Then...
    List<List<VarBindingDef>> expectedTuples = new ArrayList<List<VarBindingDef>>();
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value32),
          new VarBindingDef( var5, value51)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var3, value32),
          new VarBindingDef( var5, value51)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value32),
          new VarBindingDef( var5, value52)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var3, value32),
          new VarBindingDef( var5, value52)));

    Asserts.assertSeqEquals( "Tuples", expectedTuples, tuples);
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
    List<List<VarBindingDef>> expectedTuples = new ArrayList<List<VarBindingDef>>();
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value21)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value21)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var2, value22)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var2, value22)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var1, value11),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var1, value12),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var2, value21),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value31)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var2, value21),
          new VarBindingDef( var3, value32)));
    expectedTuples.add
      ( Arrays.asList
        ( new VarBindingDef( var2, value22),
          new VarBindingDef( var3, value32)));

    Asserts.assertSeqEquals( "Tuples", expectedTuples, tuples);
    }

  @Test
  public void testMinTupleSize()
    {
    // Given...
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( new VarDef( "var-1").addValue( new VarValueDef( "value-1-1")));
    varDefs.add( new VarDef( "var-2").addValue( new VarValueDef( "value-2-1")));
    
    // When...
    try
      {
      TupleIterator tuples = new TupleIterator( 0, varDefs);
      fail( "Unexpected TupleIterator=" + tuples);
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
  public void testMinVars()
    {
    // Given...
    List<VarDef> varDefs = new ArrayList<VarDef>();
    varDefs.add( new VarDef( "var-1").addValue( new VarValueDef( "value-1-1")));
    varDefs.add( new VarDef( "var-2").addValue( new VarValueDef( "value-2-1")));
    
    // When...
    try
      {
      TupleIterator tuples = new TupleIterator( 3, varDefs);
      fail( "Unexpected TupleIterator=" + tuples);
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
  }



