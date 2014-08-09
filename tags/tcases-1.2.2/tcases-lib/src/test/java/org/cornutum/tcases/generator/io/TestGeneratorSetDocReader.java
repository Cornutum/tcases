//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.IVarDef;
import org.cornutum.tcases.VarDef;
import org.cornutum.tcases.VarSet;
import org.cornutum.tcases.generator.*;

import org.junit.Test;
import static org.junit.Assert.*;

import org.apache.commons.collections4.IteratorUtils;

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
    IGeneratorSet generatorSet = generatorSetResources_.read( "generator-set-0.xml");
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
    IGeneratorSet generatorSet = generatorSetResources_.read( "generator-set-1.xml");
    ITestCaseGenerator[] generators = IteratorUtils.toArray( generatorSet.getGenerators(), ITestCaseGenerator.class);
    assertEquals( "Generators", 1, generators.length);

    TupleGenerator  tupleGenerator;
    String          functionName;
    TupleCombiner[] combiners;
    TupleCombiner   combiner;
    VarSet          varSet;
    IVarDef         varDef;
    
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

    varSet =
      new VarSet( "include1")
      .addMember( new VarSet( "B").addMember( new VarDef( "C")));
    
    varDef = varSet.getDescendant( "B.C");
    assertEquals( "Combiner[0], var=" + varDef + ", eligible", true, combiner.isEligible( varDef));
    
    varDef = new VarDef( "include2");
    assertEquals( "Combiner[0], var=" + varDef + ", eligible", true, combiner.isEligible( varDef));

    varSet =
      new VarSet( "exclude1")
      .addMember( new VarDef( "var"))
      .addMember( new VarDef( "var2"));
    
    varDef = varSet.getDescendant( "var");
    assertEquals( "Combiner[0], var=" + varDef + ", eligible", false, combiner.isEligible( varDef));
    
    varDef = varSet.getDescendant( "var2");
    assertEquals( "Combiner[0], var=" + varDef + ", eligible", true, combiner.isEligible( varDef));

    varSet =
      new VarSet( "exclude2")
      .addMember( new VarSet( "var").addMember( new VarSet( "A").addMember( new VarDef( "B"))))
      .addMember( new VarDef( "var2"));
    
    varDef = varSet.getDescendant( "var.A.B");
    assertEquals( "Combiner[0], var=" + varDef + ", eligible", false, combiner.isEligible( varDef));
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
    IGeneratorSet generatorSet = generatorSetResources_.read( "generator-set-2.xml");
    ITestCaseGenerator[] generators = IteratorUtils.toArray( generatorSet.getGenerators(), ITestCaseGenerator.class);
    assertEquals( "Generators", 4, generators.length);

    TupleGenerator  tupleGenerator;
    String          functionName;
    
    functionName = "F1";
    tupleGenerator = (TupleGenerator ) generatorSet.getGenerator( functionName);
    assertEquals( "getGenerator, function=" + functionName, true, tupleGenerator != null);
    assertEquals( "Generator(" + functionName + "), seed", null, tupleGenerator.getRandomSeed());
    assertEquals( "Generator(" + functionName + "), defaultTupleSize", 1, tupleGenerator.getDefaultTupleSize());
    assertEquals( "Generator(" + functionName + "), combiners", 0, tupleGenerator.getCombiners().size());
    
    functionName = "F2";
    tupleGenerator = (TupleGenerator ) generatorSet.getGenerator( functionName);
    assertEquals( "getGenerator, function=" + functionName, true, tupleGenerator != null);
    assertEquals( "Generator(" + functionName + "), seed", null, tupleGenerator.getRandomSeed());
    assertEquals( "Generator(" + functionName + "), defaultTupleSize", 1, tupleGenerator.getDefaultTupleSize());
    assertEquals( "Generator(" + functionName + "), combiners", 0, tupleGenerator.getCombiners().size());
    
    functionName = "F3";
    tupleGenerator = (TupleGenerator ) generatorSet.getGenerator( functionName);
    assertEquals( "getGenerator, function=" + functionName, true, tupleGenerator != null);
    assertEquals( "Generator(" + functionName + "), seed", null, tupleGenerator.getRandomSeed());
    assertEquals( "Generator(" + functionName + "), defaultTupleSize", 1, tupleGenerator.getDefaultTupleSize());
    assertEquals( "Generator(" + functionName + "), combiners", 0, tupleGenerator.getCombiners().size());
    
    functionName = null;
    tupleGenerator = (TupleGenerator ) generatorSet.getGenerator( functionName);
    assertEquals( "getGenerator, function=" + functionName, true, tupleGenerator != null);
    assertEquals( "Generator(" + functionName + "), seed", null, tupleGenerator.getRandomSeed());
    assertEquals( "Generator(" + functionName + "), defaultTupleSize", 1, tupleGenerator.getDefaultTupleSize());
    assertEquals( "Generator(" + functionName + "), combiners", 0, tupleGenerator.getCombiners().size());

    functionName = "F4";
    assertEquals( "Found default generator for function=" + functionName, true, tupleGenerator == generatorSet.getGenerator( functionName));
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
    IGeneratorSet generatorSet = generatorSetResources_.read( "generator-set-3.xml");
    ITestCaseGenerator[] generators = IteratorUtils.toArray( generatorSet.getGenerators(), ITestCaseGenerator.class);
    assertEquals( "Generators", 1, generators.length);

    TupleGenerator  tupleGenerator;
    String          functionName;
    TupleCombiner[] combiners;
    TupleCombiner   combiner;
    VarSet          varSet;
    IVarDef         varDef;
    
    tupleGenerator = (TupleGenerator ) generators[0];
    functionName = "F4";
    assertEquals( "Found default generator for function=" + functionName, true, tupleGenerator == generatorSet.getGenerator( functionName));
    assertEquals( "Default generator, seed", null, tupleGenerator.getRandomSeed());
    assertEquals( "Default generator, defaultTupleSize", 3, tupleGenerator.getDefaultTupleSize());

    combiners = tupleGenerator.getCombiners().toArray( new TupleCombiner[ tupleGenerator.getCombiners().size()]);
    assertEquals( "Default generator, combiners", 2, tupleGenerator.getCombiners().size());

    combiner = combiners[0];
    assertEquals( "Combiner[0], tupleSize", 1, combiner.getTupleSize());

    varSet =
      new VarSet( "A")
      .addMember( new VarDef( "B"))
      .addMember( new VarSet( "X").addMember( new VarDef( "Y")));
    
    varDef = varSet.getDescendant( "X.Y");
    assertEquals( "Combiner[0], var=" + varDef + ", eligible", true, combiner.isEligible( varDef));
    
    varDef = varSet.getDescendant( "B");
    assertEquals( "Combiner[0], var=" + varDef + ", eligible", false, combiner.isEligible( varDef));
    
    varDef = new VarDef( "C");
    assertEquals( "Combiner[0], var=" + varDef + ", eligible", true, combiner.isEligible( varDef));

    varDef = new VarDef( "D");
    assertEquals( "Combiner[0], var=" + varDef + ", eligible", false, combiner.isEligible( varDef));

    combiner = combiners[1];
    assertEquals( "Combiner[1], tupleSize", 3, combiner.getTupleSize());

    varSet =
      new VarSet( "D")
      .addMember( new VarSet( "X").addMember( new VarDef( "Y")))
      .addMember( new VarDef( "Z"))
      .addMember( new VarSet( "E").addMember( new VarDef( "E1")));
    
    varDef = varSet.getDescendant( "X.Y");
    assertEquals( "Combiner[1], var=" + varDef + ", eligible", true, combiner.isEligible( varDef));
    
    varDef = varSet.getDescendant( "Z");
    assertEquals( "Combiner[1], var=" + varDef + ", eligible", true, combiner.isEligible( varDef));
    
    varDef = varSet.getDescendant( "E.E1");
    assertEquals( "Combiner[1], var=" + varDef + ", eligible", false, combiner.isEligible( varDef));

    varDef = new VarDef( "F");
    assertEquals( "Combiner[1], var=" + varDef + ", eligible", false, combiner.isEligible( varDef));
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
    IGeneratorSet generatorSet = generatorSetResources_.read( "generator-set-4.xml");
    ITestCaseGenerator[] generators = IteratorUtils.toArray( generatorSet.getGenerators(), ITestCaseGenerator.class);
    assertEquals( "Generators", 1, generators.length);

    TupleGenerator  tupleGenerator;
    String          functionName;
    TupleCombiner[] combiners;
    TupleCombiner   combiner;
    VarSet          varSet;
    IVarDef         varDef;
    
    tupleGenerator = (TupleGenerator ) generators[0];
    functionName = "F4";
    assertEquals( "Found default generator for function=" + functionName, true, tupleGenerator == generatorSet.getGenerator( functionName));
    assertEquals( "Default generator, seed", new Long( 12345L), tupleGenerator.getRandomSeed());
    assertEquals( "Default generator, defaultTupleSize", 3, tupleGenerator.getDefaultTupleSize());

    combiners = tupleGenerator.getCombiners().toArray( new TupleCombiner[ tupleGenerator.getCombiners().size()]);
    assertEquals( "Default generator, combiners", 1, tupleGenerator.getCombiners().size());

    combiner = combiners[0];
    assertEquals( "Combiner[0], tupleSize", tupleGenerator.getDefaultTupleSize(), combiner.getTupleSize());

    varSet = new VarSet( "A").addMember( new VarSet( "X").addMember( new VarDef( "Y")));
    
    varDef = varSet.getDescendant( "X");
    assertEquals( "Combiner[0], var=" + varDef + ", eligible", false, combiner.isEligible( varDef));
    
    varDef = new VarDef( "B");
    assertEquals( "Combiner[0], var=" + varDef + ", eligible", false, combiner.isEligible( varDef));

    varDef = new VarDef( "X");
    assertEquals( "Combiner[0], var=" + varDef + ", eligible", true, combiner.isEligible( varDef));
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
    assertException( "generator-set-5.xml", 3, "\"WTF!!\" is not a valid identifier");
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
    assertException( "generator-set-6.xml", 4, "Invalid \"seed\" attribute: \"WTF!!\" is not a number");
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
    assertException( "generator-set-7.xml", 3, "Invalid \"tuples\" attribute: \"WTF!!\" is not a number");
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
    assertException( "generator-set-8.xml", 5, "Invalid \"tuples\" attribute: \"WTF!!\" is not a number");
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
    assertException( "generator-set-9.xml", 8, "No \"var\" attribute specified");
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
    assertException( "generator-set-10.xml", 6, "\"X.**.Y\" is not a valid variable name pattern");
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
    assertException( "generator-set-11.xml", 5, "No \"var\" attribute specified");
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
    assertException( "generator-set-12.xml", 13, "\"D..\" is not a valid variable name pattern");
    }

  @Test
  public void failWhenElementUnknown()
    {
    assertException( "generator-set-13.xml", 4, "Unknown element: Combiner");
    }

  @Test
  public void failWhenElementMisplaced()
    {
    assertException( "generator-set-14.xml", 11, "The Include element is not allowed at this location");
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15.   getGeneratorSet (Failure: TupleGenerator.Function) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TupleGenerator.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TupleGenerator.Function </TD> <TD><FONT color=red> Duplicate </FONT></TD></TR>
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
  public void testGetGeneratorSet_15()
    {
    assertException( "generator-set-15.xml", 6, "Generator already defined for function=F1");
    }

  /**
   * Tests {@link GeneratorSetDocReader#getGeneratorSet getGeneratorSet()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16.   getGeneratorSet (Failure: Invalid attribute) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetGeneratorSet_16()
    {
    assertException( "generator-set-16.xml", 3, "Attribute=var is not allowed for TupleGenerator elements");
    }

  /**
   * Reports a failure if reading the given resource does <U>not</U> cause the expected exception at the expected location.
   */
  private void assertException( String resource, int expectedLine, String expectedMsg)
    {
    Throwable failure = null;
    try
      {
      generatorSetResources_.read( resource);
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

  private GeneratorSetResources generatorSetResources_ = new GeneratorSetResources( TestGeneratorSetDocReader.class);
  }
