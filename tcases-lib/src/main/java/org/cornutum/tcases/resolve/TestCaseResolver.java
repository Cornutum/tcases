//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.*;
import org.cornutum.tcases.util.ContextHandler;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.List;
import java.util.function.Function;
import static java.util.stream.Collectors.toList;

/**
 * Resolves {@link ITestCaseDef test case definitions} to create new {@link TestCase} instances.
 */
public abstract class TestCaseResolver extends ContextHandler<ResolverContext>
  {
  /**
   * Creates a new TestCaseResolver instance.
   */
  protected TestCaseResolver()
    {
    this( ResolverContext.builder().notifier( TestCaseConditionNotifier.log()).build());
    }
  
  /**
   * Creates a new TestCaseResolver instance.
   */
  protected TestCaseResolver( ResolverContext context)
    {
    super( context);
    }
  
  /**
   * Resolves the {@link ITestCaseDef test case definitions} to create new {@link TestCase} instances for the given input model.
   */
  public List<TestCase> resolve( FunctionInputDef inputDef, Function<FunctionInputDef,List<ITestCaseDef>> testCaseDefSupplier)
    {
    nextId_ = -1;
    
    return
      testCaseDefSupplier.apply( prepareValueDefs( inputDef))
      .stream()
      .map( this::resolveTestDef)
      .collect( toList());
    }

  /**
   * Resolves a {@link ITestCaseDef test case definition} to create a new {@link TestCase}.
   */
  private TestCase resolveTestDef( ITestCaseDef testCaseDef)
    {
    Integer defId = testCaseDef.getId();
    nextId_ = defId == null? nextId_ + 1 : defId;

    TestCase testCase = new TestCase( nextId_);
    testCase.setName( testCaseDef.getName());

    doFor( String.valueOf( testCase.getId()), () -> {
      toStream( testCaseDef.getVars())
        .forEach( var -> {
          doFor( var.getPathName(), () -> {
            VarValueDef valueDef = testCaseDef.getValue( var);
            doFor( String.valueOf( valueDef.getName()), () -> {
              testCase.addVarBinding( resolveBinding( var, valueDef));
              });
            });
          });
      });

    // Annotate test case with its property set
    testCaseDef.getProperties().stream()
      .reduce( (properties, property) -> properties + "," + property)
      .ifPresent( properties -> testCase.setAnnotation( Annotated.TEST_CASE_PROPERTIES, properties));
    
    return testCase;
    }

  /**
   * Returns a binding that resolves the value of the given input variable.
   */
  protected abstract VarBinding resolveBinding( VarDef varDef, VarValueDef valueDef);
  
  /**
   * Returns a {@link FunctionInputDef function input model} that is ready for resolution of input value definitions.
   */
  protected abstract FunctionInputDef prepareValueDefs( FunctionInputDef inputDef);

  /**
   * The default TestCaseResolver uses only the basic input model without schemas.
   */
  public static TestCaseResolver DEFAULT =
    new TestCaseResolver()
      {
      /**
       * Returns a binding that resolves the value of the given input variable.
       */
      @Override
      protected VarBinding resolveBinding( VarDef varDef, VarValueDef valueDef)
        {
        return VarBinding.create( varDef, valueDef);
        }
  
      /**
       * Returns a {@link FunctionInputDef function input model} that is ready for resolution of input value definitions.
       */
      @Override
      protected FunctionInputDef prepareValueDefs( FunctionInputDef inputDef)
        {
        return inputDef;
        }
      };

  private int nextId_;
  }
