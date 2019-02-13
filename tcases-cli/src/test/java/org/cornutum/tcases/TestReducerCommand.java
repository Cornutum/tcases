//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2014, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.ReducerCommand.Options;
import org.cornutum.tcases.generator.*;
import org.cornutum.tcases.generator.io.*;

import org.apache.commons.io.FilenameUtils;
import org.junit.Test;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;
import java.io.FileInputStream;
import java.net.URL;

/**
 * Runs tests for {@link ReducerCommand}.
 *
 */
public class TestReducerCommand
  {
  /**
   * Tests {@link ReducerCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Function.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Function.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Function.Has-Generator </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> ResampleFactor.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> ResampleFactor.Numeric </TD> <TD> Yes </TD> </TR>
   * <TR><TD> SampleCount.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> SampleCount.Numeric </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Absolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> InputDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Matches </TD> <TD> Full-Path </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenFunctionDefined() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "Reducer-whenFunctionDefined-Input.xml");
    File genFile = getResourceFile( "Reducer-whenFunctionDefined-Generators.xml");
    String function1 = "find-1";
    String function2 = "find-2";
    
    String[] args =
      {
        "-f", function2,
        "-g", genFile.getName(),
        "-r", "0.5",
        "-s", "8",
        inFile.getPath()
      };
    
    // When...
    ReducerCommand reducer = new ReducerCommand();
    reducer.run( new Options( args));
        
    // Then...
    IGeneratorSet generators = getGenerators( genFile);

    TupleGenerator gen = getDefaultTupleGenerator( generators);
    assertThat( "Default, generator defined", gen != null, is( false));

    TupleGenerator gen1 = getTupleGenerator( generators, function1);
    assertThat( function1 + ", generator defined", gen1 != null, is( true));
    assertThat( function1 + ", seed defined", gen1.getRandomSeed() != null, is( false));

    TupleGenerator gen2 = getTupleGenerator( generators, function2);
    assertThat( function2 + ", generator defined", gen2 != null, is( true));
    assertThat( function2 + ", seed defined", gen2.getRandomSeed() != null, is( true));
    }

  /**
   * Tests {@link ReducerCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Function.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Function.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> Function.Has-Generator </TD> <TD> NA </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> ResampleFactor.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> ResampleFactor.Numeric </TD> <TD> NA </TD> </TR>
   * <TR><TD> SampleCount.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> SampleCount.Numeric </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Absolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> InputDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Matches </TD> <TD> Project-Xml </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenMatchesProjectXml() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "Reducer-whenMatchesProjectXml.xml");
    File genFile = getResourceFile( "Reducer-whenMatchesProjectXml-Generators.xml");
    
    String[] args =
      {
        new File( inFile.getParentFile(), FilenameUtils.getBaseName( inFile.getName())).getPath()
      };
    
    // When...
    ReducerCommand reducer = new ReducerCommand();
    reducer.run( new Options( args));
        
    // Then...
    IGeneratorSet generators = getGenerators( genFile);

    TupleGenerator gen = getDefaultTupleGenerator( generators);
    assertThat( "Default, generator defined", gen != null, is( true));
    assertThat( "Default, seed defined", gen.getRandomSeed() != null, is( false));

    String function1 = "find-1";
    String function2 = "find-2";

    TupleGenerator gen1 = getTupleGenerator( generators, function1);
    assertThat( function1 + ", generator defined", gen1 != null, is( true));
    assertThat( function1 + ", seed defined", gen1.getRandomSeed() != null, is( true));

    TupleGenerator gen2 = getTupleGenerator( generators, function2);
    assertThat( function2 + ", generator defined", gen2 != null, is( true));
    assertThat( function2 + ", seed defined", gen2.getRandomSeed() != null, is( true));
    }

  /**
   * Tests {@link ReducerCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Function.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Function.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> Function.Has-Generator </TD> <TD> NA </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> ResampleFactor.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> ResampleFactor.Numeric </TD> <TD> Yes </TD> </TR>
   * <TR><TD> SampleCount.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> SampleCount.Numeric </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Absolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> InputDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Matches </TD> <TD> Project-Input </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenGeneratorsNew() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "Reducer-whenGeneratorsNew-Input.xml");
    String genFileName = "Reducer-Generators-New.xml";
    
    String[] args =
      {
        "-g", genFileName,
        "-r", "-0.5",
        new File( inFile.getParentFile(), "Reducer-whenGeneratorsNew").getPath()
      };
    
    // When...
    ReducerCommand reducer = new ReducerCommand();
    reducer.run( new Options( args));
        
    // Then...
    File genFile = new File( inFile.getParentFile(), genFileName);
    IGeneratorSet generators = getGenerators( genFile);

    TupleGenerator gen = getDefaultTupleGenerator( generators);
    assertThat( "Default, generator defined", gen != null, is( true));
    assertThat( "Default, seed defined", gen.getRandomSeed() != null, is( false));

    String function1 = "find-1";
    String function2 = "find-2";

    TupleGenerator gen1 = getTupleGenerator( generators, function1);
    assertThat( function1 + ", generator defined", gen1 != null, is( true));
    assertThat( function1 + ", seed defined", gen1.getRandomSeed() != null, is( true));

    TupleGenerator gen2 = getTupleGenerator( generators, function2);
    assertThat( function2 + ", generator defined", gen2 != null, is( true));
    assertThat( function2 + ", seed defined", gen2.getRandomSeed() != null, is( true));
    }

  /**
   * Tests {@link ReducerCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Function.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Function.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Function.Has-Generator </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> ResampleFactor.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> ResampleFactor.Numeric </TD> <TD> NA </TD> </TR>
   * <TR><TD> SampleCount.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> SampleCount.Numeric </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * <TR><TD> TestDef.Absolute </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Matches </TD> <TD> Full-Path </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenTestDefNotExists() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "Reducer-whenFunctionDefined-Input.xml");
    File genFile = getResourceFile( "Reducer-whenFunctionDefined-Generators.xml");
    File testFile = new File( inFile.getParent(), "Undefined");
    String function1 = "find-1";
    
    String[] args =
      {
        "-f", function1,
        "-g", genFile.getName(),
        "-t", testFile.getPath(),
        inFile.getPath()
      };
    
    expectFailure( RuntimeException.class)
      .when( () -> new ReducerCommand().run( new Options( args)))
      .then( failure -> assertThat( "Expected failure", failure.getMessage(), is( "Can't read test definition file=" + testFile)));
    }

  /**
   * Tests {@link ReducerCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Function.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Function.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Function.Has-Generator </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> ResampleFactor.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> ResampleFactor.Numeric </TD> <TD> Yes </TD> </TR>
   * <TR><TD> SampleCount.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> SampleCount.Numeric </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Absolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> InputDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Matches </TD> <TD> Project-Xml </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenNoFunctionGenerator() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "Reducer-whenNoFunctionGenerator-Input.xml");
    String function1 = "find-1";
    String function2 = "find-2";
    
    String[] args =
      {
        "-f", function1,
        "-r", "0.5",
        "-s", "8",
        new File( inFile.getParentFile(), "Reducer-whenNoFunctionGenerator-Input").getPath()
      };
    
    // When...
    ReducerCommand reducer = new ReducerCommand();
    reducer.run( new Options( args));
        
    // Then...
    File genFile = new File( inFile.getParentFile(), "Reducer-whenNoFunctionGenerator-Generators.xml");
    IGeneratorSet generators = getGenerators( genFile);

    TupleGenerator gen = getDefaultTupleGenerator( generators);
    assertThat( "Default, generator defined", gen != null, is( true));
    assertThat( "Default, seed defined", gen.getRandomSeed() != null, is( false));

    TupleGenerator gen1 = getTupleGenerator( generators, function1);
    assertThat( function1 + ", generator defined", gen1 != null, is( true));
    assertThat( function1 + ", seed defined", gen1.getRandomSeed() != null, is( true));

    TupleGenerator gen2 = getTupleGenerator( generators, function2);
    assertThat( function2 + ", generator defined", gen2 != null, is( false));
    }

  /**
   * Tests {@link ReducerCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Function.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Function.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> Function.Has-Generator </TD> <TD> NA </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> ResampleFactor.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> ResampleFactor.Numeric </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * <TR><TD> SampleCount.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> SampleCount.Numeric </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Absolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> InputDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Matches </TD> <TD> Project-Input </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenResampleFactorInvalid() throws Exception
    {
     // Given...
    File inFile = getResourceFile( "Reducer-whenGeneratorsNew-Input.xml");
    String genFileName = "Reducer-Generators-New.xml";
    
    String[] args =
      {
        "-g", genFileName,
        "-r", "?",
        new File( inFile.getParentFile(), "Reducer-whenGeneratorsNew").getPath()
      };
    
    expectFailure( RuntimeException.class)
      .when( () -> new ReducerCommand().run( new Options( args)))
      .then( failure -> assertThat( "Expected failure", failure.getCause().getMessage(), is( "Invalid resample factor")));
    }

  /**
   * Tests {@link ReducerCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Function.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Function.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> Function.Has-Generator </TD> <TD> NA </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> ResampleFactor.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> ResampleFactor.Numeric </TD> <TD> <FONT color="red"> Below-Min </FONT> </TD> </TR>
   * <TR><TD> SampleCount.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> SampleCount.Numeric </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Absolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> InputDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Matches </TD> <TD> Project-Input </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenResampleFactorBelowMin() throws Exception
    {
     // Given...
    File inFile = getResourceFile( "Reducer-whenGeneratorsNew-Input.xml");
    String genFileName = "Reducer-Generators-New.xml";
    
    String[] args =
      {
        "-g", genFileName,
        "-r", "-1.1",
        new File( inFile.getParentFile(), "Reducer-whenGeneratorsNew").getPath()
      };
    
    expectFailure( RuntimeException.class)
      .when( () -> new ReducerCommand().run( new Options( args)))
      .then( failure -> {
        assertThat( "Expected failure", failure.getCause().getMessage(), is( "Invalid resample factor"));

        Throwable cause = failure.getCause().getCause();
        assertThat( "Expected cause", cause == null? null : cause.getClass(), equalTo( IllegalArgumentException.class));
        assertThat( "Expected cause", cause.getMessage(), is( "Resample factor must be >= -1.0"));
        });
    }

  /**
   * Tests {@link ReducerCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Function.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Function.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Function.Has-Generator </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> ResampleFactor.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> ResampleFactor.Numeric </TD> <TD> NA </TD> </TR>
   * <TR><TD> SampleCount.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> SampleCount.Numeric </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Absolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> InputDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Matches </TD> <TD> Full-Path </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenSampleCountInvalid() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "Reducer-whenFunctionDefined-Input.xml");
    File genFile = getResourceFile( "Reducer-whenFunctionDefined-Generators.xml");
    String function2 = "find-2";
    
    String[] args =
      {
        "-f", function2,
        "-g", genFile.getName(),
        "-s", "WTF",
        inFile.getPath()
      };
    
    expectFailure( RuntimeException.class)
      .when( () -> new ReducerCommand().run( new Options( args)))
      .then( failure -> assertThat( "Expected failure", failure.getCause().getMessage(), is( "Invalid sample count")));
    }

  /**
   * Tests {@link ReducerCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Function.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Function.Exists </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * <TR><TD> Function.Has-Generator </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> ResampleFactor.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> ResampleFactor.Numeric </TD> <TD> Yes </TD> </TR>
   * <TR><TD> SampleCount.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> SampleCount.Numeric </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Absolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> InputDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Matches </TD> <TD> Project-Xml </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenFunctionUndefined() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "Reducer-whenFunctionDefined-Input.xml");
    File genFile = getResourceFile( "Reducer-whenFunctionDefined-Generators.xml");
    String function3 = "find-3";
    
    String[] args =
      {
        "-f", function3,
        "-g", genFile.getPath(),
        "-r", "0.5",
        "-s", "2",
        inFile.getPath()
      };
    
    expectFailure( RuntimeException.class)
      .when( () -> new ReducerCommand().run( new Options( args)))
      .then( failure -> assertThat( "Expected failure", failure.getMessage(), is( "Function=" + function3 + " is not defined")));
    }

  /**
   * Tests {@link ReducerCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Function.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Function.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> Function.Has-Generator </TD> <TD> NA </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> ResampleFactor.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> ResampleFactor.Numeric </TD> <TD> NA </TD> </TR>
   * <TR><TD> SampleCount.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> SampleCount.Numeric </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Absolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> InputDef.Exists </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * <TR><TD> InputDef.Matches </TD> <TD> Project-Input </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenInputDefNotExists() throws Exception
    {
    // Given...
    File inFile = new File( getResourceFile( "Reducer-whenFunctionDefined-Input.xml").getParentFile(), "Undefined");
    
    String[] args =
      {
        inFile.getPath()
      };
    
    expectFailure( RuntimeException.class)
      .when( () -> new ReducerCommand().run( new Options( args)))
      .then( failure -> assertThat( "Expected failure", failure.getMessage(), is( "Can't locate input file for path=" + inFile)));
    }
  /**
   * Tests {@link ReducerCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Function.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Function.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Function.Has-Generator </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> ResampleFactor.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> ResampleFactor.Numeric </TD> <TD> Yes </TD> </TR>
   * <TR><TD> SampleCount.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> SampleCount.Numeric </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TestDef.Absolute </TD> <TD> No </TD> </TR>
   * <TR><TD> InputDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Matches </TD> <TD> Full-Path </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenReducedTestFileDefined() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "Reducer-whenBaseTests-Input.xml");
    String function2 = "find-2";
    
    String[] args =
      {
        "-f", function2,
        "-r", "0.5",
        "-s", "8",
        "-t", "Reducer-whenBaseTests-Test.xml",
        inFile.getPath()
      };
    
    // When...
    ReducerCommand reducer = new ReducerCommand();
    reducer.run( new Options( args));
        
    // Then...
    File genFile = getResourceFile( "Reducer-whenBaseTests-Generators.xml");
    assertThat( "Generator file created", genFile.exists(), is( false));
    }

  /**
   * Returns the TupleGenerator defined for the given function. Returns null if only the default generator
   * applies to this function.
   */
  private TupleGenerator getTupleGenerator( IGeneratorSet generators, String function)
    {
    ITestCaseGenerator generator = generators.getGenerator( function);
    return
      generator != null && generator != generators.getGenerator( null)
      ? (TupleGenerator) generator
      : null;
    }

  /**
   * Returns the default TupleGenerator.
   * applies to this function.
   */
  private TupleGenerator getDefaultTupleGenerator( IGeneratorSet generators)
    {
    return (TupleGenerator) generators.getGenerator( null);
    }

  /**
   * Return the file for the given resource.
   */
  private File getResourceFile( String resource)
    {
    URL classUrl = getClass().getResource( getClass().getSimpleName() + ".class");
    return new File( new File( classUrl.getFile()).getParent(), resource);
    }

  /**
   * Returns the IGeneratorSet defined by the given file.
   */
  private IGeneratorSet getGenerators( File genDefFile)
    {
    try( GeneratorSetDocReader reader = new GeneratorSetDocReader( new FileInputStream( genDefFile)))
      {
      return reader.getGeneratorSet();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read generator definition file=" + genDefFile, e);
      }
    }
  }
