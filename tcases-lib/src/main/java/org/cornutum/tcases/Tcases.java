//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.generator.*;
import org.cornutum.tcases.generator.io.*;
import org.cornutum.tcases.io.*;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Iterator;

/**
 * Generates a set of {@link TestCase test cases} from a {@link SystemInputDef system input definition}.
 *
 */
public class Tcases
  {

  /**
   * Creates a new Tcases object.
   */
  public Tcases()
    {
    }

  /**
   * Returns test case definitions for the given system input definition, using the given generator set and
   * base test definitions. If <CODE>genDef</CODE> is null, the default generator is used.
   * If <CODE>baseDef</CODE> is null, no base test definitions are used.
   */
  public static SystemTestDef getTests( SystemInputDef inputDef, IGeneratorSet genDef, SystemTestDef baseDef, GeneratorOptions options)
    {
    if( genDef == null)
      {
      genDef = GeneratorSet.basicGenerator();
      }

    SystemTestDef testDef = new SystemTestDef( inputDef.getName());
    testDef.addAnnotations( inputDef);
    for( Iterator<FunctionInputDef> functionDefs = inputDef.getFunctionInputDefs(); functionDefs.hasNext();)
      {
      FunctionTestDef functionTestDef = getTests( functionDefs.next(), genDef, baseDef, options);

      testDef.addFunctionTestDef(functionTestDef);

      // Add system input def annotations to function test def
      functionTestDef.addAnnotations( inputDef);

      // Add system input def annotations to test case def
      for( Iterator<TestCase> testCases = functionTestDef.getTestCases(); testCases.hasNext(); )
        {
        TestCase testCase = testCases.next();
        testCase.addAnnotations(inputDef);
        }
      }

    return testDef;
    }

  public static FunctionTestDef getTests( FunctionInputDef functionDef, IGeneratorSet genDef, SystemTestDef baseDef, GeneratorOptions options)
    {
    FunctionTestDef functionBase = baseDef==null? null : baseDef.getFunctionTestDef( functionDef.getName());
    ITestCaseGenerator functionGen = genDef.getGenerator( functionDef.getName());
    if( functionGen == null)
      {
      throw new RuntimeException( "No generator for function=" + functionDef.getName());
      }

    // If applicable, apply specified generator options.
    Long seed = options==null? null : options.getRandomSeed();
    Integer defaultTupleSize = options==null? null : options.getDefaultTupleSize();
    if( seed != null)
      {
      functionGen.setRandomSeed( seed);
      }
    if( defaultTupleSize != null && functionGen instanceof TupleGenerator)
      {
      ((TupleGenerator) functionGen).setDefaultTupleSize( defaultTupleSize);
      }

    FunctionTestDef functionTestDef = functionGen.getTests(functionDef, functionBase);
    annotateTests( functionDef, functionTestDef);

    return functionTestDef;
    }

  /**
   * Returns test case definitions for the given system input definition, using the given generator set and
   * base test definitions. If <CODE>genDef</CODE> is null, the default generator is used.
   * If <CODE>baseDef</CODE> is null, no base test definitions are used.
   */
  public static SystemTestDef getTests( SystemInputDef inputDef, IGeneratorSet genDef, SystemTestDef baseDef)
    {
    return getTests( inputDef, genDef, baseDef, null);
    }

  /**
   * Returns test case definitions for the given system input definition, using the given generator set and
   * base test definitions. If <CODE>genDef</CODE> is null, the default generator is used.
   * If <CODE>baseDef</CODE> is null, no base test definitions are used.
   */
  public static SystemTestDef getTests( InputStream inputDefStream, InputStream genDefStream, InputStream baseDefStream)
    {
    try
      {
      return
        getTests
        ( new SystemInputDocReader( inputDefStream).getSystemInputDef(),
          genDefStream==null? null : new GeneratorSetDocReader( genDefStream).getGeneratorSet(),
          baseDefStream==null? null : new SystemTestDocReader( baseDefStream).getSystemTestDef());
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get test definitions", e);
      }
    }

  /**
   * Returns new test case definitions for the given system input definition, using the default generator.
   */
  public static SystemTestDef getTests( InputStream inputDefStream)
    {
    return getTests( inputDefStream, null, null);
    }

  /**
   * Updates the given test definitions by adding all applicable annotations from the given input definition.
   */
  static void annotateTests( FunctionInputDef functionInputDef, FunctionTestDef functionTestDef)
    {
    // Add function annotations
    functionTestDef.addAnnotations( functionInputDef);
      
    // Add test case annotations.
    for( Iterator<TestCase> testCases = functionTestDef.getTestCases(); testCases.hasNext(); )
      {
      TestCase testCase = testCases.next();
      testCase.addAnnotations( functionInputDef);

      // Add variable binding annotations.
      for( Iterator<VarBinding> varBindings = testCase.getVarBindings(); varBindings.hasNext(); )
        {
        VarBinding binding = varBindings.next();
        VarDef varDef = functionInputDef.findVarDefPath( binding.getVar());
        String value = binding.getValue();

        // Add value annotations...
        if( !value.equals( VarValueDef.NA.getName()))
          {
          VarValueDef valueDef = varDef.getValue( value);
          binding.addAnnotations( valueDef);
          }

        // ...and any other annotations for this variable...
        binding.addAnnotations( varDef);

        // ...and any other annotations for variable sets that contain this variable.
        for( IVarDef ancestor = varDef.getParent(); ancestor != null; ancestor = ancestor.getParent())
          {
          if( ancestor instanceof Annotated)
            {
            binding.addAnnotations( (Annotated) ancestor);
            }
          }
        }
      }
    }

  /**
   * Writes an XML document describing given test case definitions to the given output stream.
   */
  @SuppressWarnings("resource")
  public static void writeTests( SystemTestDef testDef, OutputStream outputStream)
    {
    try
      {
      SystemTestDocWriter writer = new SystemTestDocWriter( outputStream);
      writer.write( testDef);
      writer.flush();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write test definitions", e);
      }
    }
  }
