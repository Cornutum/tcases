//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.generator.*;
import static org.cornutum.tcases.generator.GeneratorSet.ALL;

import org.junit.Test;
import static org.junit.Assert.*;

import org.xml.sax.SAXParseException;

/**
 * Runs tests for the {@link GeneratorSetDocReader}.
 *
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
    // Given...
    GeneratorSet expected =
      new GeneratorSetBuilder()
      .build();

    // When...
    IGeneratorSet generatorSet = generatorSetResources_.read( "generator-set-0.xml");

    // Then...
    assertEquals( "generator-set-0.xml", expected, generatorSet);
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
    // Given...
    GeneratorSet expected =
      new GeneratorSetBuilder()
      .generator(
        "F1",
        new TupleGeneratorBuilder()
        .seed( 1234L)
        .tuples( 3)
        .combiners(
          new TupleCombinerBuilder()
          .tuples( 3)
          .exclude( "exclude1.var", "exclude2.var.**")
          .build())
        .build())
      .build();

    // When...
    IGeneratorSet generatorSet = generatorSetResources_.read( "generator-set-1.xml");

    // Then...
    assertEquals( "generator-set-1.xml", expected, generatorSet);
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
    // Given...
    GeneratorSet expected =
      new GeneratorSetBuilder()
      .generator(
        ALL,
        new TupleGeneratorBuilder()
        .tuples(1)
        .build())
      .generator(
        "F1",
        new TupleGeneratorBuilder()
        .tuples(1)
        .build())
      .generator(
        "F2",
        new TupleGeneratorBuilder()
        .tuples(1)
        .build())
      .generator(
        "F3",
        new TupleGeneratorBuilder()
        .tuples(1)
        .build())
      .build();

    // When...
    IGeneratorSet generatorSet = generatorSetResources_.read( "generator-set-2.xml");

    // Then...
    assertEquals( "generator-set-2.xml", expected, generatorSet);
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
    // Given...
    GeneratorSet expected =
      new GeneratorSetBuilder()
      .generator(
        ALL,
        new TupleGeneratorBuilder()
        .tuples( 3)
        .combiners(
          new TupleCombinerBuilder()
          .tuples( 1)
          .include( "A.**", "C")
          .exclude( "A.B")
          .build(),
          new TupleCombinerBuilder()
          .tuples( 3)
          .include( "D.**")
          .exclude( "D.E.*")
          .build())
        .build())          
      .build();

    // When...
    IGeneratorSet generatorSet = generatorSetResources_.read( "generator-set-3.xml");

    // Then...
    assertEquals( "generator-set-3.xml", expected, generatorSet);
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
    // Given...
    GeneratorSet expected =
      new GeneratorSetBuilder()
      .generator(
        ALL,
        new TupleGeneratorBuilder()
        .tuples( 3)
        .seed( 12345L)
        .combiners(
          new TupleCombinerBuilder()
          .tuples( 3)
          .include( "X")
          .build())
        .build())
      .build();

    // When...
    IGeneratorSet generatorSet = generatorSetResources_.read( "generator-set-4.xml");

    // Then...
    assertEquals( "generator-set-4.xml", expected, generatorSet);
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
  
  @Test
  public void testOnceTuples()
    {
    // Given...
    GeneratorSet expected =
      new GeneratorSetBuilder()
      .generator(
        ALL,
        new TupleGeneratorBuilder()
        .tuples( 2)
        .combiners(
          new TupleCombinerBuilder()
          .tuples( 3)
          .include( "A.**", "C")
          .exclude( "A.B")
          .once(
            new TupleRefBuilder()
            .bind( "X1", "V1")
            .bind( "X2", "V2")
            .bind( "X3", "V3")
            .build(),
            new TupleRefBuilder()
            .bind( "X4", "V4")
            .bind( "X5", "V5")
            .bind( "X6", "V6")
            .build())
          .build())
        .build())
      .build();

    // When...
    IGeneratorSet generatorSet = generatorSetResources_.read( "generator-set-once.xml");

    // Then...
    assertEquals( "generator-set-once.xml", expected, generatorSet);
    }
  
  @Test
  public void testOnceTupleSize()
    {
    assertException( "generator-set-once-size.xml", 8, "Once-only tuple=TupleRef[{}] has size=0, expected size=3");
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
