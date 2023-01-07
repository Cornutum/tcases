//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.generator.*;
import org.cornutum.tcases.resolve.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Iterator;
import java.util.Optional;
import java.util.Random;

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
   * Returns test case definitions for the given system input definition, using the given generator set, resolvers, and
   * base test definitions. If <CODE>genDef</CODE> is null, the default generator is used.  If <CODE>baseDef</CODE> is
   * null, no base test definitions are used.  <P/> The <CODE>options</CODE> are optional and may be null. See also
   * {@link #getTests(SystemInputDef,IGeneratorSet,SystemTestDef)}.
   */
  public static SystemTestDef getTests( SystemInputDef inputDef, IGeneratorSet genDef, TestCaseResolverFactory resolverFactory, SystemTestDef baseDef, GeneratorOptions options)
    {
    if( resolverFactory == null)
      {
      resolverFactory = schemaResolverFactoryFor( inputDef);
      }
    SystemTestDef testDef = new SystemTestDef( inputDef.getName());
    for( Iterator<FunctionInputDef> functionDefs = inputDef.getFunctionInputDefs(); functionDefs.hasNext();)
      {
      FunctionInputDef functionDef = functionDefs.next();

      FunctionTestDef functionBase =
        Optional.ofNullable( baseDef)
        .map( base -> base.getFunctionTestDef( functionDef.getName()))
        .orElse( null);

      ITestCaseGenerator functionGen =
        Optional.ofNullable( genDef).orElse( GeneratorSet.basicGenerator())
        .getGenerator( functionDef.getName());

      FunctionTestDef functionTestDef =
        getTests(
          functionDef,
          functionGen,
          resolverFactory,
          functionBase,
          options);

      annotateTests( inputDef, functionTestDef);
      testDef.addFunctionTestDef(functionTestDef);
      }

    testDef.addAnnotations( inputDef);
    return testDef;
    }

  /**
   * Returns test case definitions for the given function input definition, using the given test case generator,
   * resolvers, and base test definitions. The <CODE>functionGen</CODE> must be non-null.  If <CODE>functionBase</CODE>
   * is null, no base test definitions are used.  <P/> The <CODE>options</CODE> are optional and may be null. See also
   * {@link #getTests(FunctionInputDef,ITestCaseGenerator,FunctionTestDef)}.
   */
  public static FunctionTestDef getTests( FunctionInputDef functionDef, ITestCaseGenerator functionGen, TestCaseResolverFactory resolverFactory, FunctionTestDef functionBase, GeneratorOptions options)
    {
    if( functionGen == null)
      {
      throw new RuntimeException( "No generator for function=" + functionDef.getName());
      }

    // If applicable, apply specified generator options.
    if( options != null)
      {
      Optional.ofNullable( options.getRandomSeed())
        .ifPresent( seed -> functionGen.setRandomSeed( seed));

      Optional.ofNullable( options.getDefaultTupleSize())
        .filter( tuples -> functionGen instanceof TupleGenerator)
        .ifPresent( tuples -> ((TupleGenerator) functionGen).setDefaultTupleSize( tuples));
      }

    // Resolve random test case values using a function-specific sequence.
    TestCaseResolver resolver =
      Optional.ofNullable( resolverFactory)
      .orElse( TestCaseResolverFactory.DEFAULT)
      .resolverFor( functionDef);
    
    resolver.getContext()
      .setRandom(
        new Random(
          Optional.ofNullable( functionGen.getRandomSeed())
          .orElse( (long) functionDef.getName().hashCode())));

    FunctionTestDef functionTestDef = new FunctionTestDef( functionDef.getName());
    resolver.resolve( f -> functionGen.getTests( f, functionBase))
      .forEach( testCase -> functionTestDef.addTestCase( testCase));

    annotateTests( resolver.getInputDef(), functionTestDef);

    return functionTestDef;
    }

  /**
   * Returns test case definitions for the given function input definition, using the given test case generator and
   * base test definitions. The <CODE>functionGen</CODE> must be non-null.
   * If <CODE>functionBase</CODE> is null, no base test definitions are used.
   */
  public static FunctionTestDef getTests( FunctionInputDef functionDef, ITestCaseGenerator functionGen, FunctionTestDef functionBase)
    {
    return getTests( functionDef, functionGen, null, functionBase, null);
    }

  /**
   * Returns test case definitions for the given system input definition, using the given generator set and
   * base test definitions. If <CODE>genDef</CODE> is null, the default generator is used.
   * If <CODE>baseDef</CODE> is null, no base test definitions are used.
   */
  public static SystemTestDef getTests( SystemInputDef inputDef, IGeneratorSet genDef, SystemTestDef baseDef)
    {
    return getTests( inputDef, genDef, null, baseDef, null);
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
          VarValueDef valueDef = varDef.getValue( binding.getSource());
          binding.addAnnotations( valueDef);
          }

        // ...and any other annotations for this variable...
        binding.addAnnotations( varDef);

        // ...and any other annotations for variable sets that contain this variable.
        for( VarSet ancestor = varDef.getParent(); ancestor != null; ancestor = ancestor.getParent())
          {
          binding.addAnnotations( ancestor);
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

  /**
   * Returns the effective system input definition.
   */
  public static SystemInputDef getEffectiveInputDef( TestCaseResolverFactory resolverFactory, SystemInputDef inputDef)
    {
    return
      SystemInputDefBuilder.with( inputDef.getName())
      .annotations( inputDef)
      .functions( toStream( inputDef.getFunctionInputDefs()).map( f -> resolverFactory.resolverFor( f).getInputDef()))
      .build();
    }

  /**
   * Returns the effective system input definition, using the standard {@link TestCaseResolver}.
   */
  public static SystemInputDef getEffectiveInputDef( SystemInputDef inputDef)
    {
    return getEffectiveInputDef( schemaResolverFactoryFor( inputDef), inputDef);
    }

  /**
   * Returns the standard {@link TestCaseResolverFactory} for the given system input definition.
   */
  public static TestCaseSchemaResolverFactory schemaResolverFactoryFor( SystemInputDef inputDef)
    {
    return
      new TestCaseSchemaResolverFactory(
        ResolverContext.builder( inputDef.getName())
        .notifier( TestCaseConditionNotifier.log())
        .build());
    }
  }
