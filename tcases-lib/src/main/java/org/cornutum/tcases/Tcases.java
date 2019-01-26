//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.generator.*;

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
  private Tcases()
    {
    // Static methods only
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
    for( Iterator<FunctionInputDef> functionDefs = inputDef.getFunctionInputDefs(); functionDefs.hasNext();)
      {
      FunctionTestDef functionTestDef = getTests( functionDefs.next(), genDef, baseDef, options);
      annotateTests( inputDef, functionTestDef);

      testDef.addFunctionTestDef(functionTestDef);
      }

    testDef.addAnnotations( inputDef);
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

        // Add value annotations...
        if( !binding.isValueNA())
          {
          VarValueDef valueDef = varDef.getValue( binding.getValue());
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
   * Updates the given test definitions by adding all applicable annotations from the given input definition.
   */
  static void annotateTests( SystemInputDef inputDef, FunctionTestDef functionTestDef)
    {
    functionTestDef.addAnnotations( inputDef);
    for( Iterator<TestCase> testCases = functionTestDef.getTestCases(); testCases.hasNext(); )
      {
      TestCase testCase = testCases.next();
      testCase.addAnnotations(inputDef);
      }
    }
  }
