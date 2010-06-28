//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator.io;

import com.startingblocktech.tcases.generator.*;

import org.junit.Test;
import static org.junit.Assert.*;

import org.apache.commons.io.IOUtils;
import org.apache.commons.collections15.IteratorUtils;

import java.io.InputStream;
import org.xml.sax.SAXParseException;

/**
 * Runs tests for the {@link GeneratorSetDocReader}.
 *
 * @version $Revision$, $Date$
 */
public class TestGeneratorSetDocReader
  {
  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0.   getGeneratorSet (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> None </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD> NA </TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD> NA </TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD> NA </TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD> NA </TD></TR>
   * <TR><TD> Include.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Include.Var </TD> <TD> NA </TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_0()
    {
    IGeneratorSet generatorSet = readGeneratorSet( "generator-set-0.xml");
    ITestCaseGenerator[] generators = IteratorUtils.toArray( generatorSet.getGenerators(), ITestCaseGenerator.class);
    assertEquals( "Generators", 0, generators.length);    
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1.   getGeneratorSet (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD> Defined </TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD> Defined </TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD> Default </TD></TR>
   * <TR><TD> Include.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Include.Var </TD> <TD> NA </TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD> Valid </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_1()
    {
    IGeneratorSet generatorSet = readGeneratorSet( "generator-set-1.xml");
    ITestCaseGenerator[] generators = IteratorUtils.toArray( generatorSet.getGenerators(), ITestCaseGenerator.class);
    assertEquals( "Generators", 1, generators.length);

    TupleGenerator  tupleGenerator;
    String          functionName;
    TupleCombiner[] combiners;
    TupleCombiner   combiner;
    
    tupleGenerator = (TupleGenerator ) generators[0];
    assertEquals( "Generator 0, seed", new Long( 1234L), tupleGenerator.getRandomSeed());
    assertEquals( "Generator 0, defaultTupleSize", 3, tupleGenerator.getDefaultTupleSize());

    functionName = "F1";
    assertEquals( "getGenerator, function=" + functionName, tupleGenerator, generatorSet.getGenerator( functionName));

    functionName = null;
    assertEquals( "getGenerator, function=" + functionName, null, generatorSet.getGenerator( functionName));
    
    combiners = tupleGenerator.getCombiners().toArray( new TupleCombiner[ tupleGenerator.getCombiners().size()]);
    assertEquals( "Generator 0, combiners", 1, combiners.length);

    combiner = combiners[0];
    assertEquals( "Generator 0, combiner 0, empty", false, combiner.isEmpty());
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2.   getGeneratorSet (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD> Default </TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD> Default </TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD> Default </TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD> NA </TD></TR>
   * <TR><TD> Include.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Include.Var </TD> <TD> NA </TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_2()
    {
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3.   getGeneratorSet (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD> All </TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD> Default </TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Include.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Include.Var </TD> <TD> Valid </TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD> Valid </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_3()
    {
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4.   getGeneratorSet (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD> Default </TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD> Defined </TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD> Default </TD></TR>
   * <TR><TD> Include.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Include.Var </TD> <TD> Valid </TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_4()
    {
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5.   getGeneratorSet (Failure: TupleGenerator.Function) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD><FONT color=red> Non-identifier </FONT></TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD> Defined </TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD> Default </TD></TR>
   * <TR><TD> Include.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Include.Var </TD> <TD> NA </TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD> Valid </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_5()
    {
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6.   getGeneratorSet (Failure: TupleGenerator.Seed) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD> Default </TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD><FONT color=red> Non-numeric </FONT></TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD> Default </TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD> NA </TD></TR>
   * <TR><TD> Include.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Include.Var </TD> <TD> NA </TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_6()
    {
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7.   getGeneratorSet (Failure: TupleGenerator.Tuples) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD> All </TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD> Default </TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD><FONT color=red> Non-numeric </FONT></TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Include.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Include.Var </TD> <TD> Valid </TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD> Valid </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_7()
    {
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8.   getGeneratorSet (Failure: Combiner.Tuples) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD> Default </TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD> Defined </TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD><FONT color=red> Non-numeric </FONT></TD></TR>
   * <TR><TD> Include.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Include.Var </TD> <TD> Valid </TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_8()
    {
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9.   getGeneratorSet (Failure: Include.Var) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD> All </TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD> Default </TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Include.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Include.Var </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD> Valid </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_9()
    {
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10.  getGeneratorSet (Failure: Include.Var) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD> Default </TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD> Defined </TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD> Default </TD></TR>
   * <TR><TD> Include.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Include.Var </TD> <TD><FONT color=red> Invalid </FONT></TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_10()
    {
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11.  getGeneratorSet (Failure: Exclude.Var) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD> Defined </TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD> Defined </TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD> Default </TD></TR>
   * <TR><TD> Include.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Include.Var </TD> <TD> NA </TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_11()
    {
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12.  getGeneratorSet (Failure: Exclude.Var) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD> All </TD></TR>
   * <TR><TD> TupleGenerator.Seed </TD> <TD> Default </TD></TR>
   * <TR><TD> TupleGenerator.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Combiner.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Combiner.Tuples </TD> <TD> Defined </TD></TR>
   * <TR><TD> Include.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Include.Var </TD> <TD> Valid </TD></TR>
   * <TR><TD> Exclude.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Exclude.Var </TD> <TD><FONT color=red> Invalid </FONT></TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_12()
    {
    }

  /**
   * Returns the {@link IGeneratorSet} defined by the given resource.
   */
  private IGeneratorSet readGeneratorSet( String resource)
    {
    IGeneratorSet  generatorSet  = null;
    InputStream     stream          = null;
    
    try
      {
      stream = getClass().getResourceAsStream( resource);

      GeneratorSetDocReader reader = new GeneratorSetDocReader( stream);
      generatorSet = reader.getGeneratorSet();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read resource=" + resource, e);
      }
    finally
      {
      IOUtils.closeQuietly( stream);
      }

    return generatorSet;
    }

  /**
   * Reports a failure if reading the given resource does <U>not</U> cause the expected exception at the expected location.
   */
  private void assertException( String resource, int expectedLine, String expectedMsg)
    {
    Throwable failure = null;
    try
      {
      readGeneratorSet( resource);
      }
    catch( Throwable t)
      {
      failure = t;
      }

    if( failure == null)
      {
      fail( "Expected exception not thrown.");
      }
    
    Throwable cause;
    for( cause = failure.getCause();
         !(cause == null || cause instanceof SAXParseException);
         cause = cause.getCause());

    if( cause == null)
      {
      throw new RuntimeException( "Unexpected exception thrown", failure);
      }

    SAXParseException spe = (SAXParseException) cause;
    assertEquals( "Exception line", expectedLine, spe.getLineNumber());

    String actualMsg = spe.getException()==null? spe.getMessage() : spe.getException().getMessage();
    assertEquals( "Exception message", expectedMsg, actualMsg);
    }

  }
