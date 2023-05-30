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
import java.util.Objects;
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
  protected TestCaseResolver( ResolverContext context, FunctionInputDef inputDef)
    {
    super( context);
    inputDef_ = inputDef;
    }

  /**
   * Changes the input definition for this resolver.
   */
  protected void setInputDef( FunctionInputDef inputDef)
    {
    inputDef_ = inputDef;
    }

  /**
   * Returns the input definition for this resolver.
   */
  public FunctionInputDef getInputDef()
    {
    return inputDef_;
    }

  /**
   * Resolves the {@link ITestCaseDef test case definitions} to create new {@link TestCase} instances for the given input model.
   */
  public List<TestCase> resolve( Function<FunctionInputDef,List<ITestCaseDef>> testCaseDefSupplier)
    {
    FunctionInputDef inputDef = getInputDef();
    nextId_ = -1;
    
    return
      resultFor(
        inputDef.getName(),

        () -> 
        testCaseDefSupplier.apply( inputDef)
        .stream()
        .map( this::resolveTestDef)
        .filter( Objects::nonNull)
        .collect( toList()));
    }

  /**
   * Resolves a {@link ITestCaseDef test case definition} to create a new {@link TestCase}.
   */
  private TestCase resolveTestDef( ITestCaseDef testCaseDef)
    {
    TestCase resolved;

    try
      {
      Integer defId = testCaseDef.getId();
      int nextId = defId == null? nextId_ + 1 : defId;

      TestCase testCase = new TestCase( nextId);
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
    
      resolved = testCase;
      nextId_ = nextId;
      }
    catch( ResolverSkipException skip)
      {
      getNotifier().error( skip.getLocation(), skip.getMessage(), "Ignoring this test case");
      resolved = null;
      }

    return resolved;
    }

  /**
   * Returns a binding that resolves the value of the given input variable.
   */
  protected abstract VarBinding resolveBinding( VarDef varDef, VarValueDef valueDef);

  private int nextId_;
  private FunctionInputDef inputDef_;
  }
