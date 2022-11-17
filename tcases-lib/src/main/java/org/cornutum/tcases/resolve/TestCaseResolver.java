//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.List;
import java.util.function.Function;
import static java.util.stream.Collectors.toList;

/**
 * Resolves {@link {@link ITestCaseDef test case definitions} to create new {@link TestCase} instances.
 */
public class TestCaseResolver
  {
  /**
   * Creates a new TestCaseResolver instance.
   */
  public TestCaseResolver()
    {
    }

  /**
   * Resolves the {@link {@link ITestCaseDef test case definitions} to create new {@link TestCase} instances for the given input model.
   */
  public List<TestCase> resolve(  FunctionInputDef inputDef, Function<FunctionInputDef,List<ITestCaseDef>> testCaseDefSupplier)
    {
    prepareValueDefs( inputDef);
    nextId_ = -1;
    
    return
      testCaseDefSupplier.apply( inputDef)
      .stream()
      .map( this::resolveTestDef)
      .collect( toList());
    }

  /**
   * Resolves a {@link {@link ITestCaseDef test case definition} to create a new {@link TestCase}.
   */
  private TestCase resolveTestDef( ITestCaseDef testCaseDef)
    {
    Integer defId = testCaseDef.getId();
    nextId_ = defId == null? nextId_ + 1 : defId;

    TestCase testCase = new TestCase( nextId_);
    testCase.setName( testCaseDef.getName());

    toStream( testCaseDef.getVars())
      .forEach( var -> testCase.addVarBinding( VarBinding.create( var, resolveValueDef( testCaseDef.getValue( var)))));

    // Annotate test case with its property set
    testCaseDef.getProperties().stream()
      .reduce( (properties, property) -> properties + "," + property)
      .ifPresent( properties -> testCase.setAnnotation( Annotated.TEST_CASE_PROPERTIES, properties));
    
    return testCase;
    }

  /**
   * Resolves a variable value definition.
   */
  protected VarValueDef resolveValueDef( VarValueDef valueDef)
    {
    return valueDef;
    }
  
  /**
   * Prepare for resolution of input value definitions
   */
  protected void prepareValueDefs( FunctionInputDef inputDef)
    {
    }

  private int nextId_ = 0;
  }
